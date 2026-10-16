# Package index

## Processing data

Clean and reshape raw LEVANTE data (participants, runs, trials, and
surveys) into analysis-ready tables.

- [`process_participants()`](https://levante-framework.github.io/levantemodels/reference/process_participants.md)
  : Process participants
- [`process_runs()`](https://levante-framework.github.io/levantemodels/reference/process_runs.md)
  : Process run data
- [`process_trials()`](https://levante-framework.github.io/levantemodels/reference/process_trials.md)
  : Process trial data
- [`process_trials_prelim()`](https://levante-framework.github.io/levantemodels/reference/process_trials_prelim.md)
  : Process trial data
- [`process_surveys()`](https://levante-framework.github.io/levantemodels/reference/process_surveys.md)
  : Process survey data
- [`link_surveys()`](https://levante-framework.github.io/levantemodels/reference/link_surveys.md)
  : Link survey data to participant data

## Recoding trials

Correct trial-level correctness values and item coding so that trials
are ready for scoring.

- [`recode_trials()`](https://levante-framework.github.io/levantemodels/reference/recode_trials.md)
  : Recode trial correctness for scoring
- [`recode_hf()`](https://levante-framework.github.io/levantemodels/reference/recode_hf.md)
  : Recode correctness + reclassify items for HF
- [`recode_sds()`](https://levante-framework.github.io/levantemodels/reference/recode_sds.md)
  : Recode correctness for SDS
- [`recode_slider()`](https://levante-framework.github.io/levantemodels/reference/recode_slider.md)
  : Recode correctness for slider
- [`recode_tom()`](https://levante-framework.github.io/levantemodels/reference/recode_tom.md)
  : Recode items for ToM
- [`recode_wrong_items()`](https://levante-framework.github.io/levantemodels/reference/recode_wrong_items.md)
  : Recode correctness for items with wrong answers

## Item metadata

Fetch item metadata and mappings between trials and item UIDs.

- [`fetch_corpus_items()`](https://levante-framework.github.io/levantemodels/reference/fetch_corpus_items.md)
  : Get metadata for corpus items
- [`fetch_survey_items()`](https://levante-framework.github.io/levantemodels/reference/fetch_survey_items.md)
  : Get metadata for survey items
- [`fetch_item_mapping_fields()`](https://levante-framework.github.io/levantemodels/reference/fetch_item_mapping_fields.md)
  : Get item UIDs mapped by fields
- [`fetch_item_mapping_id()`](https://levante-framework.github.io/levantemodels/reference/fetch_item_mapping_id.md)
  : Get item UIDs mapped by item_id
- [`fetch_item_mapping_trial()`](https://levante-framework.github.io/levantemodels/reference/fetch_item_mapping_trial.md)
  : Get item UIDs mapped by trial_id

## Scoring

Score participants’ task data, using IRT models from the model registry
or task-specific scoring methods.

- [`score()`](https://levante-framework.github.io/levantemodels/reference/score.md)
  : Score
- [`score_irt()`](https://levante-framework.github.io/levantemodels/reference/score_irt.md)
  : Given trial data and model record, score data from corresponding
  model
- [`score_cat()`](https://levante-framework.github.io/levantemodels/reference/score_cat.md)
  : Scores from CAT
- [`score_sre()`](https://levante-framework.github.io/levantemodels/reference/score_sre.md)
  : Scores for SRE

## Scoring specifications and model registry

Look up which model is used to score each task and dataset, and retrieve
fitted models from the model registry.

- [`fetch_scoring_table()`](https://levante-framework.github.io/levantemodels/reference/fetch_scoring_table.md)
  : Fetch scoring specification table
- [`fetch_scoring_parameters()`](https://levante-framework.github.io/levantemodels/reference/fetch_scoring_parameters.md)
  : Fetch scoring parameters table
- [`get_model_spec()`](https://levante-framework.github.io/levantemodels/reference/get_model_spec.md)
  : Get the scoring specification in scoring_table for a given task +
  dataset
- [`fetch_registry_dir()`](https://levante-framework.github.io/levantemodels/reference/fetch_registry_dir.md)
  : Fetch model registry file index table
- [`get_model_record()`](https://levante-framework.github.io/levantemodels/reference/get_model_record.md)
  : Get the model record indexed in registry_dir for a given scoring
  specification
- [`get_registry_file()`](https://levante-framework.github.io/levantemodels/reference/get_registry_file.md)
  : Fetch file from model registry
- [`model_spec_filename()`](https://levante-framework.github.io/levantemodels/reference/model_spec_filename.md)
  : Convert model spec to filename
- [`model_from_record()`](https://levante-framework.github.io/levantemodels/reference/model_from_record.md)
  : Given ModelRecord object, instantiate mirt model object

## Model records

The `ModelRecord` class stores a fitted IRT model with its data, items,
and scores, and provides accessors for them.

- [`ModelRecord-class`](https://levante-framework.github.io/levantemodels/reference/ModelRecord-class.md)
  [`ModelRecord`](https://levante-framework.github.io/levantemodels/reference/ModelRecord-class.md)
  : ModelRecord class definition
- [`modelrecord()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`extract_scores()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`show(`*`<ModelRecord>`*`)`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`AIC(`*`<ModelRecord>`*`)`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`BIC(`*`<ModelRecord>`*`)`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`logLik(`*`<ModelRecord>`*`)`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`model_class()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`model_vals()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`items()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`scores()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  [`tabdata()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)
  : Constructor helper
- [`mod_coefs()`](https://levante-framework.github.io/levantemodels/reference/mod_coefs.md)
  : Extract item parameters from a model record

## Preparing data for IRT models

Filter items and reshape trial data into the format expected by mirt,
and specify model structure and invariance constraints.

- [`count_items()`](https://levante-framework.github.io/levantemodels/reference/count_items.md)
  : Count number of items with non-NA responses for each run
- [`dedupe_items()`](https://levante-framework.github.io/levantemodels/reference/dedupe_items.md)
  : Add identifiers for each instance of each item
- [`remove_no_var_items()`](https://levante-framework.github.io/levantemodels/reference/remove_no_var_items.md)
  : Remove items with no variance
- [`remove_no_var_items_bygroup()`](https://levante-framework.github.io/levantemodels/reference/remove_no_var_items_bygroup.md)
  : Remove items with no invariance in any single group
- [`remove_nonshared_items()`](https://levante-framework.github.io/levantemodels/reference/remove_nonshared_items.md)
  : Remove items that aren't shared across all groups
- [`to_mirt_shape()`](https://levante-framework.github.io/levantemodels/reference/to_mirt_shape.md)
  : Format data for mirt
- [`to_mirt_shape_grouped()`](https://levante-framework.github.io/levantemodels/reference/to_mirt_shape_grouped.md)
  : Format data for mirt
- [`generate_model_str_numeric()`](https://levante-framework.github.io/levantemodels/reference/generate_model_str_numeric.md)
  : Generates mirt model strings
- [`invariances`](https://levante-framework.github.io/levantemodels/reference/invariances.md)
  : Mapping from invariance names to corresponding mirt argument

## Fitting IRT models

Fit and record sets of pooled or multigroup IRT models for a task.

- [`fit_task_models_pooled()`](https://levante-framework.github.io/levantemodels/reference/fit_task_models_pooled.md)
  : For a given task + subsetting variable/value, fit set of pooled
  models
- [`fit_task_models_multigroup()`](https://levante-framework.github.io/levantemodels/reference/fit_task_models_multigroup.md)
  : For a given task and group variable, fit and record set of
  multigroup models
- [`fit_bylanguage_task()`](https://levante-framework.github.io/levantemodels/reference/fit_bylanguage_task.md)
  : Wrapper: given task, fit by_language model for each language
- [`fit_bylanguage_lang()`](https://levante-framework.github.io/levantemodels/reference/fit_bylanguage_lang.md)
  : Wrapper: given language, fit by_language model for each task

## Inspecting fitted models

Extract fit statistics, item parameters, and scores from fitted mirt
models.

- [`mirt_aic()`](https://levante-framework.github.io/levantemodels/reference/mirt_aic.md)
  : Get AIC of fitted mirt model
- [`mirt_bic()`](https://levante-framework.github.io/levantemodels/reference/mirt_bic.md)
  : Get BIC of fitted mirt model
- [`mirt_coefs()`](https://levante-framework.github.io/levantemodels/reference/mirt_coefs.md)
  : Get item parameters of fitted mirt model
- [`mirt_scores()`](https://levante-framework.github.io/levantemodels/reference/mirt_scores.md)
  : Get participant scores of fitted mirt model
- [`multigroup_coefs()`](https://levante-framework.github.io/levantemodels/reference/multigroup_coefs.md)
  : Get coefficients for multigroup model
- [`multigroup_extract_groups()`](https://levante-framework.github.io/levantemodels/reference/multigroup_extract_groups.md)
  : Get groups out of multigroup model
- [`multigroup_itemfit()`](https://levante-framework.github.io/levantemodels/reference/multigroup_itemfit.md)
  : Get item fits out of multigroup model

## Utilities

- [`notebook_dataset()`](https://levante-framework.github.io/levantemodels/reference/notebook_dataset.md)
  : Determine dataset of current notebook on Redivis
