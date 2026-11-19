# Get the model record indexed in the registry for a given scoring specification

If `spec` has a non-missing `registry_version` (a column of the scoring
table), the model record is fetched from that version of the registry;
otherwise it is fetched from `registry_dir`. Each model record is
downloaded once per session per registry directory and then reused.

## Usage

``` r
get_model_record(spec, registry_dir = NULL)
```

## Arguments

- spec:

  Model specification (list with names item_task, model_set, subset,
  itemtype, nfact, invariance, and optionally registry_version).

- registry_dir:

  Model registry directory as returned by
  [`fetch_registry_dir()`](https://levante-framework.github.io/levantemodels/reference/fetch_registry_dir.md),
  used if `spec` has no `registry_version`.

## Value

ModelRecord object
