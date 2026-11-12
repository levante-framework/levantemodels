# Identify the Hearts & Flowers form of each run

H&F v2 (core-tasks \#442, shipped 2026-04-21) adds response windows and
feedback, gated on a variant parameter that isn't exported to Redivis. A
variant is v2 if any of its runs has more than 32 mixed-block trials (v1
has 32, v2 48) or its first run is on or after `v2_start`, since every
variant created from 2026-04-23 on runs v2. (On the four pilot datasets
as of 2026-11 the two signals agree on every variant.)

## Usage

``` r
hf_forms(runs, trials, v2_start = "2026-04-23")
```

## Arguments

- runs:

  run data with columns `run_id`, `task_id`, `variant_id`,
  `time_started`

- trials:

  trial data with columns `run_id`, `item_task`, `item_group`

- v2_start:

  first date of v2 variants

## Value

tibble with columns `run_id` and `hf_form` (`"v1"` or `"v2"`), one row
per H&F run
