# Fitting IRT models

> **Note:** The content of this vignette was generated using Claude
> agents and then reviewed by package authors.

LEVANTE task scores are ability estimates from item response theory
(IRT) models fit with [`mirt`](https://cran.r-project.org/package=mirt).
This vignette shows how to fit those models for one task with
`levantemodels`: what the input data looks like, the calls that fit the
models, and how to work with the model files they write. (For scoring
data against models that have already been fit, see
[`vignette("scoring-and-model-registry")`](https://levante-framework.github.io/levantemodels/articles/scoring-and-model-registry.md).)

The vignette uses simulated data so that it runs offline in a few
seconds. The last section shows how to build the same input from real
LEVANTE data.

``` r

library(levantemodels)
library(dplyr)
library(tidyr)
```

## Input data

The fitting functions take trial-level data **nested by task and by
group**: one row per combination of task and grouping variables, with
the trials in a list-column called `data`.

Here we simulate a 4-alternative task (`chance = 0.25`) given at two
sites. Children at `site_a` (English) saw 15 items, and children at
`site_b` (German) saw only the first 12 of them. Responses follow a 2PL
model with a guessing floor at chance.

``` r

set.seed(1)

items <- tibble(
  item_uid = sprintf("matrix_%02d", 1:15),
  a = rlnorm(15, 0, 0.3),              # discrimination
  b = seq(-2, 2, length.out = 15),     # difficulty
  chance = 0.25
)

simulate_site <- \(dataset, language, n_runs, theta_mean, item_uids) {
  tibble(dataset, language,
         run_id = paste0(dataset, "_run", 1:n_runs),
         theta = rnorm(n_runs, theta_mean)) |>
    cross_join(filter(items, item_uid %in% item_uids)) |>
    mutate(p = chance + (1 - chance) * plogis(a * (theta - b)),
           correct = runif(n()) < p) |>
    select(dataset, language, run_id, item_uid, chance, correct)
}

trials <- bind_rows(
  simulate_site("site_a", "en-US", 400, 0.0, items$item_uid),
  simulate_site("site_b", "de-DE", 300, 0.5, items$item_uid[1:12])
)

task_data <- trials |>
  mutate(item_task = "matrix") |>
  nest(data = -c(item_task, dataset, language))

task_data
#> # A tibble: 2 × 4
#>   dataset language item_task data                
#>   <chr>   <chr>    <chr>     <list>              
#> 1 site_a  en-US    matrix    <tibble [6,000 × 4]>
#> 2 site_b  de-DE    matrix    <tibble [3,600 × 4]>
```

Each element of `data` holds one row per trial:

``` r

task_data$data[[1]]
#> # A tibble: 6,000 × 4
#>    run_id      item_uid  chance correct
#>    <chr>       <chr>      <dbl> <lgl>  
#>  1 site_a_run1 matrix_01   0.25 TRUE   
#>  2 site_a_run1 matrix_02   0.25 FALSE  
#>  3 site_a_run1 matrix_03   0.25 FALSE  
#>  4 site_a_run1 matrix_04   0.25 TRUE   
#>  5 site_a_run1 matrix_05   0.25 TRUE   
#>  6 site_a_run1 matrix_06   0.25 TRUE   
#>  7 site_a_run1 matrix_07   0.25 TRUE   
#>  8 site_a_run1 matrix_08   0.25 FALSE  
#>  9 site_a_run1 matrix_09   0.25 FALSE  
#> 10 site_a_run1 matrix_10   0.25 TRUE   
#> # ℹ 5,990 more rows
```

The columns the fitting functions need are:

| Column | Level | Contents |
|----|----|----|
| `item_task` | outer | task name, used to select a task and to name output files |
| grouping variable(s) | outer | e.g. `language`, `dataset`, `site`: what models are fit separately for, or what groups multigroup models compare |
| `run_id` | `data` | one task administration; each run becomes one row of the response matrix |
| `item_uid` | `data` | item identifier; an item that is answered more than once in a run is treated as several instances whose parameters are constrained to be equal |
| `correct` | `data` | response (logical or 0/1); `NA` responses are dropped |
| `chance` | `data` | chance level for the item, used as its fixed lower asymptote (guessing parameter) |

Before fitting, items with no variance in responses are dropped. For
multigroup models, items without variance within some group are also
dropped.

## Specifying the models

Models are specified as a data frame with one row per model. For
*pooled* models (fit separately for each subset of the data) the columns
are `nfact` (number of factors) and `itemtype` (`"Rasch"` or `"2PL"`).
For *multigroup* models there’s also `invariance`, which says what is
constrained to be equal across groups:

- `"configural"`: nothing is shared, so each group has its own item
  parameters.
- `"metric"`: slopes are shared.
- `"scalar"`: slopes and intercepts are shared, and group means and
  variances are freely estimated.

(See `invariances` for the mapping to `mirt` arguments.)

``` r

models_pooled <- tribble(
  ~nfact, ~itemtype,
       1,   "Rasch",
       1,     "2PL"
)

models_multigroup <- tribble(
  ~nfact, ~itemtype, ~invariance,
       1,   "Rasch", "configural",
       1,   "Rasch", "scalar",
       1,     "2PL", "configural",
       1,     "2PL", "metric",
       1,     "2PL", "scalar"
)
```

Priors are a named list: each name is a `mirt` parameter, and each value
gives the prior type and its two parameters. These are the priors that
LEVANTE uses:

``` r

priors <- list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3))
```

## Fitting

The fitting functions write each fitted model to `registry_dir` as a
`ModelRecord` saved in an `.rds` file, and they are called for this side
effect. Models are fit with verbose `mirt` output, which we hide here.

``` r

registry_dir <- "model_registry"
```

**Pooled models by language.**
[`fit_bylanguage_task()`](https://levante-framework.github.io/levantemodels/reference/fit_bylanguage_task.md)
fits every model in `models_pooled` separately for each language in the
data.
[`fit_task_models_pooled()`](https://levante-framework.github.io/levantemodels/reference/fit_task_models_pooled.md)
does the same for a single subset of any variable,
e.g. `subset_var = dataset, subset_val = "site_a"`.

``` r

fit_bylanguage_task(task_data, models = models_pooled, priors = priors,
                    task = "matrix", registry_dir = registry_dir)
```

**Multigroup models.**
[`fit_task_models_multigroup()`](https://levante-framework.github.io/levantemodels/reference/fit_task_models_multigroup.md)
fits one model with every level of `group` as a group, for each row of
`models_multigroup`:

``` r

fit_task_models_multigroup(task_data, models = models_multigroup,
                           priors = priors, task = "matrix", group = dataset,
                           registry_dir = registry_dir)
```

Each multigroup specification is fit in up to two steps:

1.  **`overlap_items/`**: a model fit to just the items that every group
    saw (here, the 12 items common to both sites).
2.  **`all_items/`**: a model fit to all items. Parameters for the
    overlapping items are fixed at their values from step 1, so these
    items anchor the scale and the remaining items are calibrated onto
    it. This step is skipped for configural models, since they put no
    constraints across groups.

The resulting registry:

``` r

list.files(registry_dir, recursive = TRUE)
#>  [1] "matrix/by_language/de-DE/matrix_2pl_f1.rds"                            
#>  [2] "matrix/by_language/de-DE/matrix_rasch_f1.rds"                          
#>  [3] "matrix/by_language/en-US/matrix_2pl_f1.rds"                            
#>  [4] "matrix/by_language/en-US/matrix_rasch_f1.rds"                          
#>  [5] "matrix/multigroup_dataset/all_items/matrix_2pl_f1_metric.rds"          
#>  [6] "matrix/multigroup_dataset/all_items/matrix_2pl_f1_scalar.rds"          
#>  [7] "matrix/multigroup_dataset/all_items/matrix_rasch_f1_scalar.rds"        
#>  [8] "matrix/multigroup_dataset/overlap_items/matrix_2pl_f1_configural.rds"  
#>  [9] "matrix/multigroup_dataset/overlap_items/matrix_2pl_f1_metric.rds"      
#> [10] "matrix/multigroup_dataset/overlap_items/matrix_2pl_f1_scalar.rds"      
#> [11] "matrix/multigroup_dataset/overlap_items/matrix_rasch_f1_configural.rds"
#> [12] "matrix/multigroup_dataset/overlap_items/matrix_rasch_f1_scalar.rds"
```

Paths follow the pattern
`{task}/{model_set}/{subset}/{task}_{itemtype}_f{nfact}[_{invariance}].rds`.
This is the same layout as the LEVANTE model registry on Redivis: each
row of the scoring table
([`fetch_scoring_table()`](https://levante-framework.github.io/levantemodels/reference/fetch_scoring_table.md))
gives a `model_set`, `subset`, `itemtype`, `nfact`, and `invariance`,
and
[`model_spec_filename()`](https://levante-framework.github.io/levantemodels/reference/model_spec_filename.md)
turns these into a path in this layout.

## Working with model files

Each file contains a `ModelRecord`, a lightweight record of the fitted
model that holds its parameters, the data it was fit to, its fit
statistics, and the EAP ability scores for the data it was fit to.

``` r

mod_rec <- readRDS(file.path(registry_dir,
                             "matrix/multigroup_dataset/all_items/matrix_2pl_f1_scalar.rds"))
mod_rec
#> ModelRecord of a MultipleGroupClass model (1 factor 2PL)
```

**Item parameters.**
[`mod_coefs()`](https://levante-framework.github.io/levantemodels/reference/mod_coefs.md)
returns one row per item and group, with slope (`a1`), intercept (`d`),
lower and upper asymptotes (`g`, `u`), difficulty on the IRT scale
(`-d / a1`), and the number of responses to the item. Under scalar
invariance the parameters are the same for both groups. Items 13–15 have
no responses from `site_b`, but they still get that group’s (shared)
parameters.

``` r

mod_coefs(mod_rec)
#> # A tibble: 30 × 9
#>    group  item         a1     d     g     u n_responses n_inst difficulty
#>    <chr>  <chr>     <dbl> <dbl> <dbl> <dbl>       <int>  <int>      <dbl>
#>  1 site_a matrix_02 1.01  1.84   0.25     1         400      1      -1.82
#>  2 site_b matrix_02 1.01  1.84   0.25     1         300      1      -1.82
#>  3 site_a matrix_01 1.05  1.72   0.25     1         400      1      -1.64
#>  4 site_b matrix_01 1.05  1.72   0.25     1         300      1      -1.64
#>  5 site_a matrix_04 1.28  1.85   0.25     1         400      1      -1.44
#>  6 site_b matrix_04 1.28  1.85   0.25     1         300      1      -1.44
#>  7 site_a matrix_03 0.783 1.08   0.25     1         400      1      -1.38
#>  8 site_b matrix_03 0.783 1.08   0.25     1         300      1      -1.38
#>  9 site_a matrix_05 0.835 0.883  0.25     1         400      1      -1.06
#> 10 site_b matrix_05 0.835 0.883  0.25     1         300      1      -1.06
#> # ℹ 20 more rows
```

**Ability scores.**
[`scores()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
returns EAP ability estimates and their standard errors for each run the
model was fit to:

``` r

scores(mod_rec)
#> # A tibble: 700 × 3
#>    run_id       ability    se
#>    <chr>          <dbl> <dbl>
#>  1 site_a_run1   -0.600 0.643
#>  2 site_a_run2    0.230 0.673
#>  3 site_a_run3    0.924 0.636
#>  4 site_a_run4   -0.738 0.607
#>  5 site_a_run5    1.07  0.665
#>  6 site_a_run6    0.892 0.641
#>  7 site_a_run7   -0.161 0.675
#>  8 site_a_run8   -0.119 0.648
#>  9 site_a_run9   -1.10  0.646
#> 10 site_a_run10  -0.585 0.674
#> # ℹ 690 more rows
```

As a check, the estimates approximately recover the simulated difference
between sites (`site_b` was simulated 0.5 SD higher):

``` r

scores(mod_rec) |>
  mutate(group = mod_rec@groups) |>
  summarise(mean_ability = mean(ability), .by = group)
#> # A tibble: 2 × 2
#>   group  mean_ability
#>   <chr>         <dbl>
#> 1 site_a     0.000445
#> 2 site_b     0.558
```

**Model comparison.** [`AIC()`](https://rdrr.io/r/stats/AIC.html),
[`BIC()`](https://rdrr.io/r/stats/AIC.html), and
[`logLik()`](https://rdrr.io/r/stats/logLik.html) work on a
`ModelRecord`. Models are only comparable when they’re fit to the same
data, so here we compare the five `overlap_items` models:

``` r

overlap_dir <- file.path(registry_dir, "matrix/multigroup_dataset/overlap_items")
overlap_files <- list.files(overlap_dir, full.names = TRUE)

tibble(model = basename(overlap_files),
       mod_rec = lapply(overlap_files, readRDS)) |>
  mutate(logLik = sapply(mod_rec, logLik),
         AIC = sapply(mod_rec, AIC),
         BIC = sapply(mod_rec, BIC)) |>
  select(-mod_rec) |>
  arrange(BIC)
#> # A tibble: 5 × 4
#>   model                          logLik   AIC   BIC
#>   <chr>                           <dbl> <dbl> <dbl>
#> 1 matrix_rasch_f1_scalar.rds     -4491. 9011. 9079.
#> 2 matrix_2pl_f1_scalar.rds       -4483. 9017. 9135.
#> 3 matrix_rasch_f1_configural.rds -4483. 9019. 9137.
#> 4 matrix_2pl_f1_metric.rds       -4477. 9027. 9190.
#> 5 matrix_2pl_f1_configural.rds   -4474. 9043. 9262.
```

**Other `mirt` functions.** A `ModelRecord` doesn’t store the full
`mirt` model object.
[`model_from_record()`](https://levante-framework.github.io/levantemodels/reference/model_from_record.md)
rebuilds it from the stored parameters and data (without re-estimating
anything), so you can use other `mirt` functions, e.g. for item fit or
plots:

``` r

mod <- model_from_record(mod_rec)
mirt::itemfit(mirt::extract.group(mod, group = "site_a"))
#>           item   S_X2 df.S_X2 RMSEA.S_X2 p.S_X2
#> 1  matrix_01-1  3.737      10      0.000  0.958
#> 2  matrix_02-1 11.247      10      0.018  0.339
#> 3  matrix_03-1 10.100      11      0.000  0.521
#> 4  matrix_04-1  5.605      10      0.000  0.847
#> 5  matrix_05-1  4.807      10      0.000  0.904
#> 6  matrix_06-1  6.420      10      0.000  0.779
#> 7  matrix_07-1  7.653      10      0.000  0.663
#> 8  matrix_08-1 14.382      10      0.033  0.156
#> 9  matrix_09-1 11.731      10      0.021  0.303
#> 10 matrix_10-1 19.123      11      0.043  0.059
#> 11 matrix_11-1  4.248      11      0.000  0.962
#> 12 matrix_12-1 12.705      11      0.020  0.313
#> 13 matrix_13-1  7.372       9      0.000  0.598
#> 14 matrix_14-1  6.047       9      0.000  0.735
#> 15 matrix_15-1  9.282       9      0.009  0.412
```

Other contents are available as slots, e.g. `mod_rec@data` (the response
matrix, with columns in `items(mod_rec)` order), `mod_rec@groups` (group
of each row), and `model_vals(mod_rec)` (the full
[`mirt::mod2values()`](https://philchalmers.github.io/mirt/reference/mod2values.html)
parameter table).

## Using real LEVANTE data

To fit models to real data, build `task_data` from the raw (unprocessed)
per-site LEVANTE datasets.
[`process_trials()`](https://levante-framework.github.io/levantemodels/reference/process_trials.md)
loads their trials, drops practice trials, incomplete runs, and invalid
runs, and adds item identifiers and metadata. Language comes from the
run data and dataset from the participant data, and
[`recode_trials()`](https://levante-framework.github.io/levantemodels/reference/recode_trials.md)
then makes the trials ready for scoring:

``` r

dataset_spec <- list(
  list(name = "pilot_western_ca_main_raw:97mt", version = "v15_28")
)

runs <- process_runs(dataset_spec)
participants <- process_participants(dataset_spec)

task_data <- process_trials(dataset_spec) |>
  left_join(runs |> select(run_id, language), by = "run_id") |>
  left_join(participants |> select(user_id, dataset), by = "user_id") |>
  recode_trials() |>
  nest(data = -c(item_task, dataset, language))
```

The production LEVANTE models also apply further exclusions before
fitting (e.g. duplicate runs, straightlining, responses that are too
fast or too slow, and items flagged for exclusion). These exclusions are
applied in the `levante-pilots` repository, in
`01_fetch_data/02_scoring_prep.qmd`, and the models are fit in
`02_score_data/01_fit_irt/01_fit_irt_modular.qmd`, which runs the fits
above for every task in parallel.
