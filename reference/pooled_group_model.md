# single group model with the pooled distribution of a multigroup model's groups

For scoring data from a group that is not in a scalar multigroup model:
returns a single group model with the shared item parameters and the
pooled latent distribution of the model's groups, i.e. the mean and
variance of the equally weighted mixture of the groups' normal
distributions. Nothing is estimated.

## Usage

``` r
pooled_group_model(mod_rec)
```

## Arguments

- mod_rec:

  ModelRecord object for a scalar multigroup model

## Value

mirt SingleGroupClass model
