# Remove excluded items from trial data

Removes the trials matched by any applicable row of the item exclusion
table
([`fetch_exclusions()`](https://levante-framework.github.io/levantemodels/reference/fetch_exclusions.md)).
Each row documents one decision to exclude an item, optionally only for
some datasets, languages, or dates. Released trial data keep all trials;
exclusions should be applied before fitting models (e.g. in the
calibration notebook) and can be applied when scoring via
[`score()`](https://levante-framework.github.io/levantemodels/reference/score.md).

## Usage

``` r
apply_exclusions(trials, exclusions)
```

## Arguments

- trials:

  trial data after
  [`recode_trials()`](https://levante-framework.github.io/levantemodels/reference/recode_trials.md),
  with columns `item_uid` and whichever of `dataset`, `language`,
  `timestamp` the exclusions use

- exclusions:

  exclusion table, e.g. from
  [`fetch_exclusions()`](https://levante-framework.github.io/levantemodels/reference/fetch_exclusions.md)

## Value

`trials` without the excluded trials.

## Details

|  |  |  |  |  |
|----|----|----|----|----|
| column | type | required | meaning | example |
| `item_uid` | text | yes | item uid as produced by [`recode_trials()`](https://levante-framework.github.io/levantemodels/reference/recode_trials.md); for Theory of Mind, a uid without a story matches that question in every story | `tom_reference_reference`, `tom_story4_deception_reality_check_2` |
| `dataset` | text | no | dataset the row applies to; `NA` = all datasets | `pilot_uniandes_co_bogota` |
| `language` | text | no | language the row applies to, exactly as in the trial `language` column; `NA` = all languages | `es-CO` |
| `start_date` | date | no | first date excluded (inclusive); `NA` = unbounded | `2024-10-24` |
| `end_date` | date | no | date the exclusion ends (exclusive); `NA` = unbounded | `2024-07-01` |
| `exclude` | logical | no | only `TRUE` rows apply; if the column is absent, every row applies | `TRUE` |
| `reason` | text | no | documentation only | `below chance since image 4g` |

Multi-valued Airtable fields (`dataset`, `language`) are synced as one
row per value. Absent optional columns are treated as `NA`; a blank
`dataset` or `language` must be `NA`, and `""` or `"NULL"` is an error.
POSIXct timestamps are compared as UTC dates. Character timestamps are
compared by their written date, so they must be UTC as stored on
Redivis.
