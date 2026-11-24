# Recode correctness for slider

Also fixes every slider item's chance at 0.15. Of single floors from
0.10 to 0.30, 0.15 fit the math model best with the default
`slider_threshold` of 0.15 (levante-analysis,
studies/2026-11_number-line). It is not derived from the threshold, so a
different threshold needs its own floor.

## Usage

``` r
recode_slider(df, slider_threshold)
```

## Arguments

- df:

  trial data

- slider_threshold:

  max normalized distance from slider target
