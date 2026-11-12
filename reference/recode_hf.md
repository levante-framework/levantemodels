# Recode correctness + reclassify items for HF

Codes each trial's item as start, stay, or switch within its block and
codes responses faster than 200 ms or slower than the trial's window as
incorrect. The window is 2000 ms, except in the mixed block of H&F v2,
where it is the form's response window: 3000, 2000, and 1500 ms for
mixed trials 1-16, 17-32, and 33-48. v2 mixed-block items are
form-specific and split by window (e.g.
`hf_heartsflowers_heart_switch_v2_1500ms`) and get `free_guess = TRUE`,
so that their lower asymptote is estimated (a missed window scores 0,
not chance); the pure-block items are shared by both forms.

## Usage

``` r
recode_hf(df)
```

## Arguments

- df:

  trial data; H&F trials with an `hf_form` column (see
  [`hf_forms()`](https://levante-framework.github.io/levantemodels/reference/hf_forms.md))
  are recoded by form, and without one are all treated as v1
