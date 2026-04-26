# Workflow guide

This guide explains the benchmark workflow at the level a maintainer needs for debugging and updates.

`R/workflow.R` remains the high-level orchestrator. Reusable helpers have been split into focused source files so the workflow file can read like the story of a benchmark run.

## Top-level workflow

The main function is:

```r
run_benchmark_workflow()
```

Its job is to:

1. prepare the selected assessment-grade data,
2. fit each benchmark model definition,
3. combine model outputs,
4. build selected-year app outputs, and
5. return the objects used by the Shiny server.

## Helper map

```text
run_benchmark_workflow()
  ├─ prepare_model_data()
  ├─ fit_benchmark_model()
  │    ├─ make_benchmark_design()
  │    ├─ fit_cv_repeat()
  │    └─ add_benchmark_performance()
  ├─ combine_benchmark_models()
  └─ build_selected_year_outputs()
```

## Source organization

The workflow is split across focused helper files:

| File | Responsibility |
|---|---|
| `R/metrics.R` | Weighted diagnostics, residual summaries, and safe summary helpers. |
| `R/cv_helpers.R` | Grouped cross-validation helpers and repeated-CV stability summaries. |
| `R/model_terms.R` | Model-matrix term parsing and coefficient metadata helpers. |
| `R/comparison_bands.R` | Selected baseline rank-band labeling, validation, and lookup construction. |
| `R/model_design.R` | Formula labels, right-hand-side terms, and model-matrix construction. |
| `R/coefficients.R` | Standardized coefficient and relative influence summaries. |
| `R/workflow.R` | Top-level workflow orchestration. |

Related app helpers live outside the modeling workflow:

| File | Responsibility |
|---|---|
| `R/result_table.R` | Full-results table and Excel workbook formatting. |
| `R/settings_summary.R` | UI summary of captured run settings. |
| `R/run_helpers.R` | Wrapper connecting workflow outputs to visualization objects. |
| `R/visual_theme.R` | Shared visual constants and plot theme helpers. |
| `R/visual_helper.R` | Benchmark comparison plots and visual summaries. |
| `R/year_coef.R` | Year-specific benchmark feature weight table. |

## Source order

`global.R` should source files in dependency order. A typical order is:

```r
source(file.path("R", "data.R"))
source(file.path("R", "config.R"))
source(file.path("R", "labels.R"))
source(file.path("R", "app_choices.R"))
source(file.path("R", "model_groups.R"))
source(file.path("R", "result_table.R"))
source(file.path("R", "settings_summary.R"))
source(file.path("R", "metrics.R"))
source(file.path("R", "cv_helpers.R"))
source(file.path("R", "model_terms.R"))
source(file.path("R", "comparison_bands.R"))
source(file.path("R", "model_design.R"))
source(file.path("R", "coefficients.R"))
source(file.path("R", "workflow.R"))
source(file.path("R", "visual_theme.R"))
source(file.path("R", "year_coef.R"))
source(file.path("R", "visual_helper.R"))
source(file.path("R", "run_helpers.R"))
```

The most important ordering rules are:

- `data.R` and `config.R` should load before helpers that depend on app settings or lookup objects.
- `labels.R` should load before helpers that call `format_grade()`, `get_group_label()`, or `pretty_feature_label()`.
- `metrics.R`, `cv_helpers.R`, `model_terms.R`, `comparison_bands.R`, `model_design.R`, and `coefficients.R` should load before `workflow.R`.
- `visual_theme.R` should load before `year_coef.R` and `visual_helper.R`.
- `run_helpers.R` should load after `workflow.R` and `visual_helper.R`.

## Function responsibilities

### `prepare_model_data()`

Inputs:

- `model_key`
- `min_students_tested`

Returns:

- filtered modeling data with `.row_id`
- number of schools
- number of CV folds to use
- predictor groups
- prep audit

Use this first when debugging data availability or threshold problems.

### `make_benchmark_design()`

Defined in `R/model_design.R`.

Inputs:

- filtered data
- predictor groups
- one model's feature toggles

Returns:

- selected benchmark groups
- formula label
- right-hand-side model terms
- model matrix `X`
- outcome vector `y`
- weight vector `w`
- coefficient metadata

Important note: interaction terms like `SchoolYear.2025:LowIncome.LOWINC` are formula terms. They are created by `model.matrix()` and do not need to exist as raw columns.

### `fit_cv_repeat()`

Inputs:

- one design matrix
- one model name
- one CV repeat index

Returns:

- row-level out-of-fold scores for that repeat
- audit row for that repeat
- coefficient table for that repeat

This is the best helper to debug if a model fit fails.

### `add_benchmark_performance()`

Adds:

- `.resid_cv_weighted_sd`
- `.neutral_band_cv`
- `.performance_z_cv`
- `.performance_direction`
- `.performance_rank_cv`

This keeps ranking and labeling logic in one place.

### `fit_benchmark_model()`

Runs all CV repeats for one model definition, averages repeated out-of-fold predictions, computes final benchmark gaps/ranks, joins school metadata, and builds model-level coefficient and audit outputs.

### `combine_benchmark_models()`

Binds the outputs from Baseline, Alt 1, Alt 2, and Alt 3 into combined app-ready tables.

### `build_selected_year_outputs()`

Builds the selected-year objects used by plots and tables:

- `model_comp_table`
- `model_comp_plot`
- `model_comp_plot_all`
- `comparison_focus_lookup`
- `post_run_audit`

The selected year controls reporting only. It does not change the full historical modeling sample.

### `rebuild_outputs_for_comparison_band()`

Rebuilds selected-year reporting objects for a different comparison-band index without refitting the benchmark models.

Use this when the user changes the tracked rank-band comparison after a successful model run.

### `run_benchmark_workflow()`

The top-level workflow. It should be the main entry point for the Shiny server and smoke test.

## Output contract

`run_benchmark_workflow()` should return a list with these names:

```r
c(
  "prep_audit",
  "predictor_groups",
  "score_all",
  "coef_table",
  "coef_audit",
  "coef_influence",
  "coef_influence_no_year",
  "model_audit",
  "model_comp_table",
  "model_comp_plot",
  "model_comp_plot_all",
  "comparison_focus_lookup",
  "score_all_repeats",
  "model_audit_repeats",
  "post_run_audit"
)
```

The Shiny server and visualization helpers assume these objects exist.

## Debugging order

When something fails, test in this order:

1. `source("global.R")`
2. `prepare_model_data()`
3. `make_benchmark_design()`
4. `fit_cv_repeat()`
5. `fit_benchmark_model()`
6. `combine_benchmark_models()`
7. `build_selected_year_outputs()`
8. `run_benchmark_workflow()`
9. `run_benchmark_and_visuals()`
10. Shiny app

This avoids debugging the whole app when only one small part is broken.

## Smoke-test checkpoints

The smoke test should verify both the data/modeling workflow and the refactored source map. Useful checks include:

- data layer loaded and non-empty
- shared label helpers loaded
- app choice helpers loaded
- model-group helpers loaded
- visual theme helpers loaded
- metric helpers loaded
- CV helpers loaded
- model-term helpers loaded
- comparison-band helpers loaded
- model-design helpers loaded
- coefficient helpers loaded
- `prepare_model_data()` completed
- `make_benchmark_design()` completed
- `fit_cv_repeat()` completed
- `run_benchmark_workflow()` completed
- `run_benchmark_and_visuals()` available
- full-results table helper completed

A passing smoke test should end with:

```text
Smoke test passed.
```

## Maintenance guidance

Keep `R/workflow.R` as the orchestrator unless selected-year reporting or model fitting grows large enough to justify another split. The current intended pattern is:

- helper files define reusable building blocks
- `workflow.R` coordinates the benchmark run
- `server.R` handles Shiny reactivity and rendering
- documentation describes the same structure the code uses

When adding a new helper, place it in the file that matches its responsibility. If a helper starts depending on Shiny inputs, outputs, or session state, it probably belongs in `server.R` or an app-output helper rather than in the workflow layer.
