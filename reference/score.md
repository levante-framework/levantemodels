# Score

Score

## Usage

``` r
score(
  task,
  dataset,
  trials,
  runs,
  scoring_table,
  registry_dir = NULL,
  exclusions = NULL
)
```

## Arguments

- task:

  task

- dataset:

  dataset

- trials:

  trial data from one task and one dataset

- runs:

  run data from one task and one dataset

- scoring_table:

  tibble returned by fetch_scoring_table()

- registry_dir:

  redivis directory returned by fetch_registry_dir(), used for scoring
  table rows with no `registry_version`

- exclusions:

  optional exclusion table (e.g. from fetch_exclusions()), applied to
  trials with apply_exclusions() before scoring
