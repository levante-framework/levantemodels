# levantemodels

Internal R tooling for LEVANTE data **processing and psychometric modeling**.
This package holds the model intermediates — trial recoding, the IRT scoring
pipeline, the model registry, and the `ModelRecord` class.

For **loading** LEVANTE data into R (the user-facing `get_*` functions), see the
companion [`levante`](https://github.com/levante-framework/levante-r) package.
`levantemodels` is internally-facing tooling and depends on `levante` for data
access.

Some useful links:

- [An overview of the LEVANTE project](https://researcher.levante-network.org/overview).
- [Details about LEVANTE's scoring and psychometrics](https://researcher.levante-network.org/measures/scoring-and-psychometrics).
- [Details about the LEVANTE child tasks](https://researcher.levante-network.org/measures/direct-child-measures).

## Installation
```r
# install.packages("devtools") # if you need the devtools package
devtools::install_github("levante-framework/levantemodels")
```

## What's here
- **Data processing** — `process_trials()`, `process_runs()`, `process_surveys()`,
  `process_participants()`.
- **Trial recoding** — `recode_trials()` makes loaded trial data scoring-ready
  (slider thresholding, Hearts & Flowers / SDS / ToM recodes, item-key fixes).
- **IRT model fitting** — `fit_task_models_pooled()` for SingleGroup models and
  `fit_task_models_multigroup()` for MultiGroup models, along with iteration
  wrappers and helper functions for prepping data and accompanying objects.
- **IRT scoring** — `score()` / `score_irt()`, and the model
  registry accessors `fetch_scoring_table()`, `fetch_registry_table()`,
  `fetch_registry_dir()`.
- **The `ModelRecord` class** and its accessors (`items()`, `model_vals()`,
  `model_class()`, `scores()`).

See the [Scoring and the model registry](https://levante-framework.github.io/levantemodels/articles/scoring-and-model-registry.html)
vignette for the full trials → scores pipeline.

See the [Fitting IRT models](https://levante-framework.github.io/levantemodels/articles/fitting-irt-models.html)
vignette for a walkthrough of the model fitting pipeline.
