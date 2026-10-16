# Given ModelRecord object, instantiate mirt model object

Models are cached for the session (see
[`get_model_record()`](https://levante-framework.github.io/levantemodels/reference/get_model_record.md)),
so scoring several datasets with the same model record reconstructs it
only once.

## Usage

``` r
model_from_record(mod_rec)
```

## Arguments

- mod_rec:

  ModelRecord object
