# Wrapper: given task, fit by_language model for each language

Calls
[`fit_task_models_pooled()`](https://levante-framework.github.io/levantemodels/reference/fit_task_models_pooled.md)
with `subset_var = "language"` for each language present for `task`, so
`task_data` must have a `language` column.

## Usage

``` r
fit_bylanguage_task(task_data, models, priors, task, registry_dir)
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

- registry_dir:

  string indicating directory in which to save models

## Value

Called for its side effect of writing model files.
