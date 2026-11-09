# Get item exclusions

The item exclusion table, synced from Airtable. See
[`apply_exclusions()`](https://levante-framework.github.io/levantemodels/reference/apply_exclusions.md)
for its columns.

## Usage

``` r
fetch_exclusions(version = "current")
```

## Arguments

- version:

  Redivis version tag of the `levante_metadata_items` dataset

## Examples

``` r
if (FALSE) { # \dontrun{
exclusions <- fetch_exclusions()
} # }
```
