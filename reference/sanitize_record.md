# Remove calibration data from a model record

For publishing a model record without the data it was calibrated on:
removes the calibration runs and scores, and replaces the response
matrix with a minimal placeholder. The placeholder has, for each group,
one row per response category, with each item taking each of the
categories it had in the calibration data, since
[`mirt::mirt()`](https://philchalmers.github.io/mirt/reference/mirt.html)
needs data with each item's categories to reconstruct the model from its
parameter values. Scoring with a sanitized record gives the same scores
as with the original record. Response and run counts are kept (see
[`response_counts()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
and
[`run_counts()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)),
but functions that use the calibration data itself, such as
[`count_items()`](https://levante-framework.github.io/levantemodels/reference/count_items.md)
and
[`empirical_reliability()`](https://levante-framework.github.io/levantemodels/reference/empirical_reliability.md),
don't work on a sanitized record.

## Usage

``` r
sanitize_record(mod_rec)
```

## Arguments

- mod_rec:

  ModelRecord object

## Value

ModelRecord object with `data`, `groups`, `runs`, and `scores` replaced
