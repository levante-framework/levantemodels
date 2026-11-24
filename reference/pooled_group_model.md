# Single group model with the pooled distribution of a multigroup model's groups

For scoring data from a group that is not in a scalar multigroup model:
returns a single group model with the shared item parameters and the
pooled latent distribution of the model's groups, i.e. the mean and
variance of the equally weighted mixture of the groups' normal
distributions. Nothing is estimated. Groups added by
[`add_groups_multigroup()`](https://levante-framework.github.io/levantemodels/reference/add_groups_multigroup.md)
are left out, so that adding groups doesn't change the scores of groups
that aren't in the model.

## Usage

``` r
pooled_group_model(mod_rec)
```

## Arguments

- mod_rec:

  ModelRecord object for a scalar multigroup model

## Value

mirt SingleGroupClass model
