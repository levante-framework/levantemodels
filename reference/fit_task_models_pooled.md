# for a given task + subsetting variable/value, fit set of pooled models

for a given task + subsetting variable/value, fit set of pooled models

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

  dataframe with columns item_task, the value of subset_var, and data,
  where data is trial-level data

- models:

  dataframe of models to fit, specified with columns nfact (number of
  factors), itemtype (Rasch/2PL), invariance (configural/metric/scalar)

- priors:

  list of priors, where names are parameter names (e.g. d, a1) and
  values are vectors of length 3 (priorType, val1, val2)

- task:

  one value in item_task

- subset_var:

  variable to subset by (string or bare variable)

- subset_val:

  value to subset to (string)

- registry_dir:

  string indicating directory in which to save models
