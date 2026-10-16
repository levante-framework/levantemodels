# Process trial data (preliminary steps)

Process trial data (preliminary steps)

## Usage

``` r
process_trials_prelim(
  dataset_spec,
  remove_incomplete_runs = TRUE,
  remove_invalid_runs = TRUE,
  remove_invalid_trials = FALSE,
  tasks = NULL,
  participants = NULL,
  max_results = NULL
)
```

## Arguments

- dataset_spec:

  List of dataset names and versions to retrieve.

- remove_incomplete_runs:

  Boolean indicating whether to drop runs that were marked as incomplete
  (defaults to TRUE).

- remove_invalid_runs:

  Boolean indicating whether to drop runs that were marked as invalid
  (defaults to TRUE).

- remove_invalid_trials:

  Boolean indicating whether to drop trials that were marked as invalid
  (defaults to FALSE).

- tasks:

  Character vector of tasks to include.

- participants:

  (Optional) Data frame that includes the columns "dataset" and
  "user_id", if supplied trial data will be filtered to only those user
  IDs.

- max_results:

  Max number of records to load for each table (defaults to entire
  table).

## Examples

``` r
if (FALSE) { # \dontrun{
dataset_spec <- list(list(name = "levante_data_example:d0rt", version = "current"))
trials_prelim <- process_trials_prelim(dataset_spec)
} # }
```
