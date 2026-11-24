# Constructor helper

Constructor helper

Helper to extract and tidy scores

Helper to count non-NA responses per group and item

Helper to count runs per group

Method for show generic

Method for AIC generic

Method for BIC generic

Method for logLik generic

## Usage

``` r
modelrecord(mod, row_ids)

extract_scores(mod, row_ids)

count_responses(data, groups)

count_runs(groups)

# S4 method for class 'ModelRecord'
show(object)

# S4 method for class 'ModelRecord'
AIC(object)

# S4 method for class 'ModelRecord'
BIC(object)

# S4 method for class 'ModelRecord'
logLik(object)

model_class(object)

model_vals(object)

items(object)

scores(object)

response_counts(object)

run_counts(object)

tabdata(object)

convergence(object)
```

## Arguments

- mod:

  mirt model object

- row_ids:

  character vector

- object:

  ModelRecord object

## Value

`response_counts()`: tibble with `group`, `item`, and `n_responses`
(number of non-NA calibration responses); for records created before
these were stored, computed from the calibration data

`run_counts()`: tibble with `group` and `n_runs` (number of calibration
runs); for records created before these were stored, computed from the
calibration groups

`convergence()`: one-row tibble with `converged` (whether estimation
converged) and `iterations` (number of EM cycles); both `NA` for records
created before these were stored
