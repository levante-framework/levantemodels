# For a given task and group variable, fit and record set of multigroup models

For each model in `models`, fits a multigroup model to the trials for
`task`, with each level of `group` as a group, in up to two steps:

## Usage

``` r
fit_task_models_multigroup(
  task_data,
  models,
  priors,
  task,
  group,
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
  of factors), `itemtype` (`"Rasch"` or `"2PL"`), and `invariance` (a
  name in
  [invariances](https://levante-framework.github.io/levantemodels/reference/invariances.md):
  `"configural"`, `"metric"`, or `"scalar"`), and no other columns

- priors:

  list of priors, where names are parameter names (e.g. d, a1) and
  values are vectors of length 3 (priorType, val1, val2), e.g.
  `list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3))`

- task:

  one value in item_task

- group:

  variable to use as groups (string or bare variable); can be a column
  of either `task_data` or the nested `data`

- registry_dir:

  string indicating directory in which to save models

## Value

Called for its side effect of writing model files (returns `NULL`
without fitting anything if `task` isn't in `task_data` or no items are
shared by all groups).

## Details

1.  An `overlap_items` model fit to only the items that are present (and
    have variance) in every group.

2.  For models with metric or scalar invariance, an `all_items` model
    fit to all items, with the parameters of the overlapping items fixed
    at their values from the `overlap_items` model, so that these items
    anchor the scale for the remaining items.

Each model is saved as a
[ModelRecord](https://levante-framework.github.io/levantemodels/reference/ModelRecord-class.md)
at
`{registry_dir}/{task}/multigroup_{group}/{overlap_items|all_items}/{task}_{itemtype}_f{nfact}_{invariance}.rds`
(with `itemtype` lowercased). Data preparation is as in
[`fit_task_models_pooled()`](https://levante-framework.github.io/levantemodels/reference/fit_task_models_pooled.md).
See
[`vignette("fitting-irt-models")`](https://levante-framework.github.io/levantemodels/articles/fitting-irt-models.md)
for a worked example.
