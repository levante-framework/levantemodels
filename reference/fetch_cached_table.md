# Get a table from a Redivis dataset, cached on disk across sessions

If the option `levantemodels.cache_dir` is set, tables from released
dataset versions are saved there and read back on later calls instead of
being downloaded again. Released versions don't change, so a cached
table is always current for its version; unreleased versions are never
cached.

## Usage

``` r
fetch_cached_table(ds, table_name)
```

## Arguments

- ds:

  Redivis dataset object

- table_name:

  name of table in dataset

## Value

tibble
