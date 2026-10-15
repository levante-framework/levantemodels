# For a given task + subsetting variable/value, fit set of pooled models

Fits each model in `models` as a single-group model to the trials for
`task` where `subset_var == subset_val`, and saves each one as a
[ModelRecord](https://levante-framework.github.io/levantemodels/reference/ModelRecord-class.md)
at
`{registry_dir}/{task}/by_{subset_var}/{subset_val}/{task}_{itemtype}_f{nfact}.rds`
(with `itemtype` lowercased). See
[`vignette("fitting-irt-models")`](https://levante-framework.github.io/levantemodels/articles/fitting-irt-models.md)
for a worked example.

## Usage

``` r
fit_task_models_pooled(
  task_data,
  models,
  priors,
  task,
  subset_var,
  subset_val,
  registry_dir
)
```

## Arguments

- task_data:

  data frame nested by task and grouping variables, with columns
  `item_task`, the grouping variable(s) (e.g. `language`, `dataset`),
  and `data`, a list-column of trial-level data frames with (at least)
  columns `run_id`, `item_uid`, `correct` (logical or 0/1), and `chance`
  (chance level of the item)

- models:

  data frame of models to fit, one per row, with columns `nfact` (number
  of factors) and `itemtype` (`"Rasch"` or `"2PL"`), and no other
  columns

- priors:

  list of priors, where names are parameter names (e.g. d, a1) and
  values are vectors of length 3 (priorType, val1, val2), e.g.
  `list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3))`

- task:

  one value in item_task

- subset_var:

  variable to subset by (string or bare variable); must be a column of
  `task_data` (not of the nested `data`)

- subset_val:

  value to subset to (string)

- registry_dir:

  string indicating directory in which to save models

## Value

Called for its side effect of writing model files.

## Details

Before fitting, trials with `NA` `correct` are dropped, repeated
presentations of an item within a run are treated as separate instances
whose parameters are constrained to be equal, and item instances with no
variance in responses are dropped. Each item's `chance` is used as its
fixed lower asymptote (guessing parameter).
