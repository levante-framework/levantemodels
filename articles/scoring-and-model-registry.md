# Scoring and the model registry

Loading LEVANTE data (the `get_*` functions used below) is handled by
the companion
[`levante`](https://github.com/levante-framework/levante-r) package;
`levantemodels` provides the processing and modeling tooling on top of
it. This vignette covers how scores are produced, and how to reproduce
or audit them yourself using the LEVANTE **model registry**.

``` r

library(levantemodels)
library(levante)   # data loaders: get_scores(), get_trials(), ...
library(dplyr)
```

## How LEVANTE scores are produced

Most cognitive-task scores in LEVANTE are IRT ability estimates (θ).
They are produced by fitting item-response models once (calibration) and
then scoring each child’s trial data against the appropriate fitted
model. The fitted models and the bookkeeping needed to find them live in
the `levante_metadata_scoring` dataset on Redivis, which rlevante reads
through a handful of functions:

| Function | Returns |
|----|----|
| [`fetch_scoring_table()`](https://levante-framework.github.io/levantemodels/reference/fetch_scoring_table.md) | which model specification applies to each (task, dataset) |
| `fetch_registry_table()` | an index mapping model files to Redivis file ids |
| [`fetch_registry_dir()`](https://levante-framework.github.io/levantemodels/reference/fetch_registry_dir.md) | a Redivis directory handle for downloading model files |
| [`score()`](https://levante-framework.github.io/levantemodels/reference/score.md) | scores one (task, dataset) using the above |

End to end, scoring a task flows like this:

    get_trials(data_source)             # raw trial data
       |> recode_trials()               # make trials scoring-ready (see below)

    fetch_scoring_table()               # look up the model spec for (task, dataset)
    fetch_registry_dir()                # handle for downloading the fitted model

    score(task, dataset, trials, runs,  # -> downloads the ModelRecord, shapes the
          scoring_table, registry_dir)  #    data, rebuilds the model, scores it

## Pinning a dataset version

Every rlevante “get” function takes a `data_source` and a `version`. For
reproducible analyses you should pin a **fully qualified reference** of
the form `name:hash:version` rather than relying on
`version = "current"`, which silently tracks the latest release.

``` r

# tracks whatever the latest release happens to be
scores_current <- get_scores("levante_data_example:d0rt", version = "current")

# pinned: name + persistent dataset id (hash) + dataset version
scores_pinned <- get_scores("levante_data_example:d0rt:v1_0")
```

The persistent `hash` (e.g. `d0rt`) is stable even if the dataset is
renamed; the trailing `:v1_0` pins the version. You can find both on the
dataset’s Redivis page under **API Information** → **Qualified
references** (see the [“Reference identifiers” section of the
README](https://levante-framework.github.io/rlevante/index.html)). When
you call a “get” function with `version = "current"`, rlevante emits a
message reporting the qualified reference it resolved to, which you can
copy to pin future runs.

## Trials must be recoded before scoring

[`get_trials()`](https://levante-framework.github.io/levante-r/reference/get_trials.html)
returns a `correct` column, but that column is **not yet
scoring-ready**. The calibrated models were fit on data passed through
[`recode_trials()`](https://levante-framework.github.io/levantemodels/reference/recode_trials.md)
first, so you must apply it before scoring or you will get subtly wrong
numbers.

``` r

trials <- get_trials("levante_data_example:d0rt") |>
  recode_trials()
```

[`recode_trials()`](https://levante-framework.github.io/levantemodels/reference/recode_trials.md)
performs several task-specific corrections, including:

- **Slider items** (e.g. number line): marks a response correct when it
  falls within `slider_threshold` (default 0.15) of the target, and sets
  the chance level to `1 / slider_threshold / 100` accordingly.
- **Hearts & Flowers (`hf`)**: codes too-fast (\<200 ms) and too-slow
  (\>2000 ms) responses as incorrect and labels each trial
  start/stay/switch.
- **Same/Different Selection (`sds`)** and **Theory of Mind (`tom`)**:
  recodes correctness and disaggregates items.
- **Known item-key fixes**: e.g. `math_subtract_37_24` is rescored
  against the corrected answer.

## Rescoring a task yourself

Putting it together, here is how to reproduce the scores for one task
and dataset and compare them against the published values. The exported
[`score()`](https://levante-framework.github.io/levantemodels/reference/score.md)
function is the high-level entry point — it looks up the model spec,
downloads the fitted `ModelRecord`, shapes the data, and scores it.

``` r

scoring_table <- fetch_scoring_table()
registry_dir  <- fetch_registry_dir()

task <- "math"
dataset <- "pilot_uniandes_co_bogota"

# trial (and, for adaptive tasks, run) data for this task + dataset
trials_task <- get_trials("levante_data_example:d0rt") |>
  recode_trials() |>
  filter(item_task == task, dataset == !!dataset)

my_scores <- score(
  task = task, dataset = dataset,
  trials = trials_task, runs = NULL,
  scoring_table = scoring_table, registry_dir = registry_dir
)

# compare to the published scores
published <- get_scores("levante_data_example:d0rt") |>
  filter(task_id == task, dataset == !!dataset)

my_scores |>
  inner_join(published, by = "run_id", suffix = c("_mine", "_pub")) |>
  summarise(cor = cor(score_mine, score_pub))
```

## Working with a `ModelRecord`

[`score()`](https://levante-framework.github.io/levantemodels/reference/score.md)
downloads a fitted model as a `ModelRecord` object. If you want to
inspect a model directly — for example to look up item parameters, or to
validate a rescore — you can download one yourself from the registry.
The accessors and slots you will need:

``` r

# look up and download the ModelRecord for a (task, dataset)
spec <- fetch_scoring_table() |>
  filter(item_task == "math", dataset == "pilot_uniandes_co_bogota") |>
  as.list()
mod_rec <- get_registry_file(model_spec_filename(spec), fetch_registry_dir())
```

| Accessor | Returns |
|----|----|
| `items(mod_rec)` | the model’s item names, **in the order the model uses them** |
| `model_class(mod_rec)` | `"SingleGroupClass"` or `"MultipleGroupClass"` |
| `model_vals(mod_rec)` | the `mirt` parameter table for the fitted model |
| `scores(mod_rec)` | the **calibration-time** EAP scores (`run_id`, `ability`, `se`) |

Some details are exposed as S4 slots:

| Slot                  | Contents                                           |
|-----------------------|----------------------------------------------------|
| `mod_rec@scores`      | calibration EAP scores (same as `scores(mod_rec)`) |
| `mod_rec@data`        | the response matrix used to fit the model          |
| `mod_rec@groups`      | the group label of each calibration row            |
| `mod_rec@group_names` | the model’s group names (for multigroup models)    |

Two invariants are worth knowing. First, `mod_rec@data`’s columns are
guaranteed to be in `items(mod_rec)` order — scoring code must align new
data to this order before calling
[`mirt::fscores()`](https://philchalmers.github.io/mirt/reference/fscores.html),
which matches columns by position. Second, `scores(mod_rec)` holds the
EAP scores computed at calibration time on the full data matrix, so it
is a ground-truth reference you can validate a rescore against:

``` r

# rescoring the calibration runs should reproduce scores(mod_rec)$ability
scores(mod_rec)
```

## The guessing parameter (lower asymptote)

The deployed IRT models include a **fixed guessing lower asymptote** at
each item’s chance level (`g = chance`, held fixed rather than
estimated).
[`score_irt()`](https://levante-framework.github.io/levantemodels/reference/score_irt.md)
rebuilds the full model from `model_vals(mod_rec)` — which includes
these `g` rows — so the released ability scores already account for
guessing.

Two things are easy to get wrong here:

- The `item_parameters` table (and `levante::get_item_parameters()`)
  exposes only `difficulty` and `discrimination`; it **omits `g`**. Its
  `itemtype` label (`rasch` / `2pl`) describes the discrimination
  structure only, not the presence of guessing.
- The **authoritative, complete** parameter set is `model_vals(mod_rec)`
  (i.e.
  [`mirt::mod2values()`](https://philchalmers.github.io/mirt/reference/mod2values.html)),
  which includes the slope `a`, intercept/difficulty `d` / `b`, the
  guessing asymptote `g`, and the upper asymptote `u`.

To read each item’s fixed guessing value:

``` r

model_vals(mod_rec) |>
  filter(name == "g") |>
  select(item, value)   # e.g. 0.25 for a 4-AFC task such as matrix
```

If you reconstruct an item response function yourself — for person-fit,
test-information, simulation, or independent rescoring — you **must**
include the asymptote:

    P(theta) = g + (1 - g) * plogis(a * (theta - b))

Reconstructing a plain logistic without `g` silently mismatches the
fitted model and inflates residuals at low ability (lucky guesses on
hard items look “impossible”), which can corrupt person-fit and
reliability analyses even though the released scores themselves are
correct. The trial-level `chance` field from
[`get_trials()`](https://levante-framework.github.io/levante-r/reference/get_trials.html)
carries the same per-item `g` values, so it is an equivalent source for
the asymptote.
