# Generates mirt model strings

Generates mirt model strings

## Usage

``` r
generate_model_str_numeric(
  df,
  df_prepped,
  item_type,
  f,
  priors = NULL,
  item_sep = "-"
)
```

## Arguments

- df:

  trial data

- df_prepped:

  trial data in wide mirt shape (runs x items)

- item_type:

  "Rasch", "2PL", etc

- f:

  number of factors

- priors:

  list of priors

- item_sep:

  string to put in between item_uid and instance index (defaults to "-")

## Value

string that can be passed to `mirt.model()` as `input`
