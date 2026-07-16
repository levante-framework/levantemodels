# Recode trial correctness for scoring

`recode_trials()` applies the task-specific corrections that make
[`levante::get_trials()`](https://levante-framework.github.io/levante-r/reference/get_trials.html)
output scoring-ready, and **must be applied before IRT scoring**: the
calibrated models were fit on recoded trials, so scoring raw
`get_trials()` output yields subtly wrong values. Corrections include
slider thresholding (a response is correct within `slider_threshold` of
the target, with chance set to `1 / slider_threshold / 100`), Hearts &
Flowers RT and start/stay/switch coding, Same/Different Selection and
Theory of Mind recodes, known item-key fixes (e.g.
`math_subtract_37_24`), and chance-level backfill. See
[`vignette("scoring-and-model-registry")`](https://levante-framework.github.io/levantemodels/articles/scoring-and-model-registry.md).

## Usage

``` r
recode_trials(df, slider_threshold = 0.15)
```

## Arguments

- df:

  trial data

- slider_threshold:

  max normalized distance from slider target

## Value

A data frame of recoded, scoring-ready trials.
