# Slope classes of Same and Different items

Classes of SDS items that share one slope in a `"2PLclass"` model (see
[`fit_task_models_multigroup()`](https://levante-framework.github.io/levantemodels/reference/fit_task_models_multigroup.md)):
the dimensions block, the same block, and three classes of match-block
picks. A match item is named `sds_<k>match_choice<j>_<m>m<n>n`, the
`j`-th pick of a `k`-pick trial after `m` new matches and `n` new
non-matches (see
[`recode_sds()`](https://levante-framework.github.io/levantemodels/reference/recode_sds.md)).
Its class is:

- `match_first`: the trial's first pick;

- `match_after_nonmatch`: a later pick with an earlier non-match in the
  trial;

- `match_later`: any other later pick (earlier repeats, which count as
  neither match nor non-match, don't change its class).

## Usage

``` r
sds_slope_classes(item_uids)
```

## Arguments

- item_uids:

  character vector of item UIDs

## Value

character vector of slope classes, one per item; an item that isn't an
SDS item of a known block gets its UID, i.e. its own slope

## Details

These classes were chosen in the 2026-11 recalibration (levante-pilots
`03_summaries/calibration/sds_slopes.R`): of 112 partitions of the match
items by pick position, earlier errors, set size and earlier matches,
they fit best by BIC in each half of the data and on the earlier data.
First picks and picks after a non-match discriminate about half as well
as other later picks.

## Examples

``` r
sds_slope_classes(c("sds_dimensions_color", "sds_same_shape",
                    "sds_2match_choice1_0m0n", "sds_3match_choice3_1m1n",
                    "sds_3match_choice3_1m0n"))
#> [1] "dimensions"           "same"                 "match_first"         
#> [4] "match_after_nonmatch" "match_later"         
```
