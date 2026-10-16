# Get the scoring specification in scoring_table for a given task + dataset

Get the scoring specification in scoring_table for a given task +
dataset

## Usage

``` r
get_model_spec(score_task, score_dataset, scoring_table)
```

## Arguments

- score_task:

  string indicating task

- score_dataset:

  string indicating dataset

- scoring_table:

  tibble returned by get_scoring_table()

## Value

list with entries item_task, dataset, model_set, subset, itemtype,
nfact, invariance
