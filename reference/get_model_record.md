# Get the model record indexed in registry_dir for a given scoring specification

Each model record is downloaded once per session and then reused. The
cache holds one registry directory at a time: passing a different
`registry_dir` (e.g. from a new call to
[`fetch_registry_dir()`](https://levante-framework.github.io/levantemodels/reference/fetch_registry_dir.md))
clears it.

## Usage

``` r
get_model_record(spec, registry_dir)
```

## Arguments

- spec:

  Model specification (list with names item_task, model_set, subset,
  itemtype, nfact, invariance).

- registry_dir:

  Model registry directory as returned by
  [`fetch_registry_dir()`](https://levante-framework.github.io/levantemodels/reference/fetch_registry_dir.md).

## Value

ModelRecord object
