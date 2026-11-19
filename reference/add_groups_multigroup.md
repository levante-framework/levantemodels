# Add new groups to a scalar multigroup model

Estimates the latent mean and variance of each new group in `task_data`
with every item parameter and every existing group's mean and variance
held fixed at their values in `mod_rec` (fixed item parameter
calibration), and returns a
[ModelRecord](https://levante-framework.github.io/levantemodels/reference/ModelRecord-class.md)
of the model with the new groups added. Nothing is written to disk.

## Usage

``` r
add_groups_multigroup(mod_rec, task_data, task, group, min_runs = 100)
```

## Arguments

- mod_rec:

  ModelRecord object for a scalar multigroup model

- task_data:

  data frame nested by task and grouping variables, with columns
  `item_task`, the grouping variable(s) (e.g. `language`, `dataset`),
  and `data`, a list-column of trial-level data frames with (at least)
  columns `run_id`, `item_uid`, `correct` (logical or 0/1), and `chance`
  (chance level of the item)

- task:

  one value in item_task

- group:

  variable whose values are the new groups (string or bare variable);
  can be a column of either `task_data` or the nested `data`. None of
  its values can already be groups in `mod_rec`.

- min_runs:

  minimum number of runs for a new group

## Value

ModelRecord object

## Details

The model is fit to the record's calibration data together with the new
groups' data, so that every item has the response categories it was
calibrated with. Since everything else is fixed, each new group's
estimates depend only on its own data, and are the same whether groups
are added together or one at a time. The existing groups' parameters are
unchanged, so their scores are too.

Trials are prepared as for scoring: `NA` `correct` is dropped and
repeated presentations are numbered by
[`dedupe_items()`](https://levante-framework.github.io/levantemodels/reference/dedupe_items.md).
Items not in the model are dropped (items with no variance are kept,
since their parameters are fixed), as are runs with no responses to the
model's items. This assumes scalar invariance holds for the new groups
and does not test it.

In the returned record, each new group's parameter values are those of
the model's second group with its own mean and variance, and the fit
statistics and convergence are those of the estimation of the new
groups' means and variances. `mod_rec@fit$added_groups` accumulates,
over successive additions, a tibble with each added group's number of
runs, number of the model's items with responses, and estimated mean and
variance with their standard errors.
