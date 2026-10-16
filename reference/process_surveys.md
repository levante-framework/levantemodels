# Process survey data

Process survey data

## Usage

``` r
process_surveys(
  dataset_spec,
  survey_types = c("caregiver", "student", "teacher"),
  remove_incomplete_surveys = FALSE,
  max_results = NULL
)
```

## Arguments

- dataset_spec:

  List of dataset names and versions to retrieve.

- survey_types:

  Character vector of survey types to include (caregiver, student,
  teacher).

- remove_incomplete_surveys:

  Boolean indicating whether to drop surveys that were marked as
  incomplete (defaults to FALSE).

- max_results:

  Max number of records to load for each table (defaults to entire
  table).

## Examples

``` r
if (FALSE) { # \dontrun{
dataset_spec <- list(list(name = "levante_data_example_raw:bm7r", version = "current"))
surveys <- process_surveys(dataset_spec)
} # }
```
