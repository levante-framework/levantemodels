# Parameter values for one group of a multigroup model, as a single group model

Gives the same single group model as
[`mirt::extract.group()`](https://philchalmers.github.io/mirt/reference/extract.group.html)
on the reconstructed multigroup model, without reconstructing it: the
group's rows of the model record's parameter values, relabelled as a
single group's. Build the model with
`mirt::mirt(data = mod_rec@data, pars = group_vals(mod_rec, group), TOL = NaN)`.
This uses all of the model's calibration data, not just the group's, so
that every item has the response categories the model was calibrated
with (the data doesn't affect scoring with
`fscores(response.pattern = )`).

## Usage

``` r
group_vals(mod_rec, group)
```

## Arguments

- mod_rec:

  ModelRecord object for a multigroup model

- group:

  name of one of the model's groups

## Value

data frame of parameter values, as from
[`mirt::mod2values()`](https://philchalmers.github.io/mirt/reference/mod2values.html)
