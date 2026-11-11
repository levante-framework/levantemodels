# Mapping from invariance names to corresponding mirt argument

Named list giving, for each invariance level that can be used in the
`invariance` column of `models` in
[`fit_task_models_multigroup()`](https://levante-framework.github.io/levantemodels/reference/fit_task_models_multigroup.md),
the value passed to the `invariance` argument of
[`mirt::multipleGroup()`](https://philchalmers.github.io/mirt/reference/multipleGroup.html).

## Usage

``` r
invariances
```

## Format

An object of class `list` of length 3.

## Details

mirt fixes every group's latent distribution at N(0, 1) unless
`free_means` or `free_var` is given, and always fixes the reference
(first) group's. So:

- `configural`: item parameters free in every group; all groups N(0, 1).

- `metric`: slopes equal across groups; latent variances free
  (identified by the equal slopes), means fixed at 0.

- `scalar`: slopes and intercepts equal; means and variances free.

Each level adds constraints to the one before, so the three are nested.
(Before 2026-11, `metric` was `"slopes"` alone, following an earlier
mirt example, which also held every group's variance at 1.)
