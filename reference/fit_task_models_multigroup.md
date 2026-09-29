# for a given task and group variable, fit and record set of multigroup models

for a given task and group variable, fit and record set of multigroup
models

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

- group:

  variable to use as groups (string or bare variable)

- registry_dir:

  string indicating directory in which to save models
