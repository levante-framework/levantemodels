# CFA scoring from a scheme-B survey model record.
#
# Scheme B: the record holds no response data, only the lavaan parameter table
# (estimates and SEs), the lavaan options and the category map (every model
# category of every item). Scoring builds a single-group model with all
# parameters fixed (the site's own group, or for a new site the pooled model of
# all groups, see pooled_single_group_partable()), and prepends one "template"
# row per item x category (that item answered, all others NA) so lavaan sees
# every calibration category. Each row's EBM score depends only on its own
# responses and the fixed parameters, so the template rows do not change the
# real scores; they are dropped afterwards.
#
# A scorer is a function `scorer_<scheme>(rec, group, new_site, newdata)`,
# chosen from `rec$scheme` by score_survey_cfa(): newdata = data frame of the
# model's items (model coding), all rows to be scored; it returns a matrix of
# factor scores with nrow(newdata) rows, columns = latent variable names.

# Single-group parameter table for one group of a fitted (multigroup) model:
# every parameter fixed at its estimate, equality / inequality constraints
# dropped (they are satisfied by the fixed values).
single_group_partable <- function(pt, group) {
  p <- pt[pt$group == group, c("lhs", "op", "rhs", "block", "group", "free", "ustart", "est", "exo")]
  p$ustart <- p$est; p$free <- 0L; p$group <- 1L; p$block <- 1L
  p$label <- ""; p$plabel <- ""
  p$est <- NULL
  # A single-group model ignores ordinal item intercepts (lavaan fixes them at
  # 0), but groups 2+ of a multigroup model can have nonzero intercepts (when
  # "intercepts" is not in group.equal). P(y <= k) depends on tau_k - nu only,
  # so move each intercept into the item's thresholds (tau' = tau - nu,
  # nu' = 0): the same model, expressed the way a single-group model can hold it.
  for (v in unique(p$lhs[p$op == "|"])) {
    i <- which(p$op == "~1" & p$lhs == v)
    if (length(i) == 1 && abs(p$ustart[i]) > 0) {
      th <- which(p$op == "|" & p$lhs == v)
      p$ustart[th] <- p$ustart[th] - p$ustart[i]
      p$ustart[i] <- 0
    }
  }
  p
}

# One row per item x model category: that item answered, all others NA.
survey_template_rows <- function(meta) {
  items <- meta$items
  rows <- purrr::map(items, \(it) purrr::map(meta$categories[[it]], \(k) {
    r <- as.list(rep(NA_real_, length(items))); names(r) <- items; r[[it]] <- k; as_tibble(r)
  })) |> purrr::list_flatten() |> bind_rows()
  as.data.frame(rows)
}

# Single-group parameter table for a site that is not a group of the model
# (IRT: pooled_group_model()). Every group's single-group table is built with
# single_group_partable(), then, with equal weights for the groups:
#   - latent means: the mean of the groups' latent means;
#   - latent (co)variances: the mean of the groups' latent covariance matrices
#     plus the covariance of their latent means, i.e. the moments of the
#     equally weighted mixture of the groups' normal distributions;
#   - every other parameter (loadings, thresholds, residual variances and
#     covariances): the mean over the groups. Loadings and thresholds are equal
#     across groups in a scalar model, so only parameters that lavaan lets
#     differ by group (residual variances / scale factors, residual
#     covariances) actually change. Note: lavPredict(method = "EBM") uses only
#     the residual variances (items independent given the factors), so
#     residual covariances, in the pooled or in a group's own table, do not
#     enter the scores.
# The scale-factor rows (~*~) are dropped and the model is built in the theta
# parameterization, where the residual variances are parameters: the averaged
# residual variances are then used as they are. (For a group's own table both
# parameterizations give the same scores.)
pooled_single_group_partable <- function(pt, meta) {
  G <- seq_along(meta$group_labels)
  sg <- purrr::map(G, \(g) single_group_partable(pt, g))
  key <- \(p) paste(p$lhs, p$op, p$rhs)
  stopifnot("groups have different parameters" = all(purrr::map_lgl(sg, \(p) identical(key(p), key(sg[[1]])))))
  out <- sg[[1]]
  out$ustart <- rowMeans(sapply(sg, \(p) p$ustart))
  lv <- meta$lv; k <- length(lv)
  means <- sapply(sg, \(p) p$ustart[match(paste(lv, "~1", ""), key(p))])
  means <- matrix(means, nrow = k)                               # k x groups
  covs <- purrr::map(sg, \(p) {
    S <- matrix(NA_real_, k, k, dimnames = list(lv, lv))
    for (i in which(p$op == "~~" & p$lhs %in% lv & p$rhs %in% lv)) S[p$lhs[i], p$rhs[i]] <- S[p$rhs[i], p$lhs[i]] <- p$ustart[i]
    S[is.na(S)] <- 0                                             # latent covariances not in the model are 0
    S
  })
  m <- rowMeans(means)
  dev <- means - m
  S <- Reduce(`+`, covs) / length(G) + dev %*% t(dev) / length(G)
  im <- match(paste(lv, "~1", ""), key(out)); stopifnot(!anyNA(im))
  out$ustart[im] <- m
  for (i in which(out$op == "~~" & out$lhs %in% lv & out$rhs %in% lv)) out$ustart[i] <- S[out$lhs[i], out$rhs[i]]
  # a covariance that is fixed at 0 in every group but nonzero in the mixture
  # cannot occur in these models (one factor, or freely correlated factors)
  ut <- upper.tri(S); a <- lv[row(S)[ut]]; b <- lv[col(S)[ut]]
  miss <- which(abs(S[ut]) > 1e-12 & !paste(a, "~~", b) %in% key(out) & !paste(b, "~~", a) %in% key(out))
  stopifnot("pooled latent covariance not representable" = length(miss) == 0)
  attr(out, "pooled") <- list(mean = stats::setNames(m, lv), cov = S)
  out[out$op != "~*~", ]
}

# Scorer for scheme-B records (see top of file).
scorer_B <- function(rec, group, new_site, newdata) {
  meta <- rec$meta; items <- meta$items; opt <- meta$lavaan_options
  if (new_site) {
    pt1 <- pooled_single_group_partable(rec$partable, meta); param <- "theta"
    message("--Site not in model: scoring under the pooled model of groups ", paste(meta$group_labels, collapse = ", "))
  } else {
    pt1 <- single_group_partable(rec$partable, match(group, meta$group_labels)); param <- opt$parameterization
  }
  tmpl <- survey_template_rows(meta)
  dd <- rbind(tmpl, as.data.frame(newdata[items]))
  fit_single <- suppressWarnings(lavaan::cfa(pt1, data = dd, ordered = items, estimator = opt$estimator,
                                     parameterization = param, missing = "pairwise", do.fit = FALSE))
  x <- as.matrix(lavaan::lavPredict(fit_single, method = "EBM"))
  stopifnot(nrow(x) == nrow(dd))
  x[-seq_len(nrow(tmpl)), , drop = FALSE]
}
