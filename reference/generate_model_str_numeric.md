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
  item_sep = "-",
  constrain_guess_groups = FALSE
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

- constrain_guess_groups:

  whether to constrain estimated lower asymptotes to be equal across
  groups (for scalar multigroup models)

## Value

string that can be passed to `mirt.model()` as `input`

## Details

If `df` has a logical `free_guess` column, the lower asymptote (`g`) of
the items with `free_guess = TRUE` is estimated instead of fixed at
their `chance`, bounded above by `chance`, with one value per item
shared by its instances. This applies to Rasch models only; in other
models it stays fixed at `chance`.
