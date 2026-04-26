# Model-Based Benchmarking

Historical assessment benchmarking app built in **R Shiny**.

## Overview

Model-Based Benchmarking is a historical benchmarking tool for school-level average scale scores. The app fits benchmark models to a selected assessment-and-grade history, then compares each observed school-year score with a model-based benchmark score under user-defined comparison frames.

The goal is transparent, benchmark-adjusted comparison. The app is **not** designed for causal inference, policy-effect estimation, future-year forecasting, or stand-alone accountability judgment.

## What the app does

Users can:

- choose an assessment and grade
- set a minimum tested-student threshold
- choose the year shown in the comparison views
- adjust the neutral-band width
- define one baseline benchmark model and up to three alternative benchmark models

The app then:

1. selects the matching historical assessment-and-grade dataset
2. removes school-years below the tested-student threshold
3. fits benchmark models using weighted ridge regression
4. generates out-of-fold benchmark scores using repeated grouped cross-validation by school
5. computes benchmark scores, benchmark gaps, scaled benchmark gaps, benchmark labels, and within-year ranks
6. compares results across benchmark definitions for the selected year

## Intended use

Use this app to support internal review and discussion of historical school-level assessment results. It is designed to help users ask questions such as:

- How does a school-year's observed score compare with its selected historical benchmark?
- Are conclusions stable when the comparison frame changes?
- Which schools move most across alternative benchmark definitions?
- How sensitive are ranks, benchmark gaps, and benchmark labels to the selected benchmark groups?

The app should not be used to claim that selected benchmark features caused school performance. It should also not be used to predict future-year outcomes or replace accountability decisions.

## Interface language

- **Benchmark score** = the model-based score for a school-year under the chosen comparison frame.
- **Benchmark gap** = observed score minus benchmark score.
- **Scaled benchmark gap** = benchmark gap divided by the model-level weighted residual standard deviation. This is used for comparison and ranking, not as a formal z-score or significance test.
- **Benchmark label** = above benchmark, near benchmark, or below benchmark, based on the user-selected neutral band.
- **Within-year rank** = the school's benchmark-adjusted position relative to other schools in the same year.
- **Benchmark feature weights** = year-specific standardized coefficient-weight diagnostics. These are model diagnostics, not causal effects or stand-alone importance measures.

Internal code objects still use names such as `.pred_cv`, `.resid_cv`, `.performance_z_cv`, and `.performance_direction`. Those names are implementation details; the user-facing app uses the terminology above.

## Main result views

- **Benchmarking overview**: summarizes sensitivity across benchmark definitions for the selected paired baseline rank bands.
- **Benchmark feature weights**: shows year-specific standardized coefficient-weight diagnostics for the selected year.
- **Split stability**: summarizes repeated grouped-CV fold-assignment stability across all schools in the selected year.
- **Rank comparison**: compares each school in the selected paired baseline rank bands with its rank under alternative benchmark definitions.
- **Rank movement**: shows rank shifts for schools in the selected paired baseline rank bands across benchmark definitions.
- **Average rank movement**: summarizes average absolute rank movement across all schools in the selected year.
- **Scores and benchmark gaps**: compares benchmark scores, benchmark gaps, scaled benchmark gaps, and within-year ranks across benchmark definitions for the selected paired baseline rank bands.
- **Full results**: lists all schools in the selected year for the fitted benchmark models and allows download to Excel.

Detailed school-level plots track the selected paired baseline rank bands in the selected year, such as 0-10% vs 90-100% or 40-50% vs 50-60%. Average rank movement, split stability, and the full results table use all schools in the selected year.

## Benchmark groups

Available benchmark groups currently include:

- percent eligible students with missing scores
- student unit count
- county
- ELL
- foster care
- gender
- Wilmington geography
- homelessness
- immersion
- low income
- migrant
- military-connected
- race
- SPED code

School year is always included in the model and is not shown as an optional benchmark group.

## Method summary

The unit of analysis is a **school-year**. The outcome is the school-year average scale score.

For each selected assessment and grade, the workflow:

1. filters school-years below the minimum tested-student threshold
2. builds a design matrix with school-year indicators, selected benchmark features, and year-by-feature interactions
3. fits weighted ridge regression models using `glmnet` with `alpha = 0` and `family = "gaussian"`
4. uses tested-student-count weights from the prepared data, following a `sqrt(n)` weighting convention
5. performs grouped cross-validation by school so all rows from the same school stay in the same fold
6. repeats fold assignments across multiple seeds
7. averages row-level out-of-fold predictions across repeats
8. uses the mean out-of-fold prediction as the official historical benchmark score for each school-year row
9. computes benchmark gap, scaled benchmark gap, benchmark label, and within-year rank

The out-of-fold benchmark scores are designed for historical comparison and sensitivity review. They are not an external validation study: the ridge penalty is selected within the repeated cross-validation workflow, and the full eligible historical assessment-grade dataset defines the modeling context. Treat model diagnostics as practical stability checks rather than proof of future predictive accuracy.

The selected comparison year controls the views and tables. It does **not** redefine the full historical modeling sample.

## Neutral band and benchmark labels

The neutral band is a practical reading aid, not a statistical significance test.

The app computes the neutral-band width from the weighted residual standard deviation across the full eligible historical modeling sample for the fitted benchmark model. A wider neutral band labels more school-years as near benchmark and fewer as above or below benchmark.

Benchmark labels are interpreted as:

- **Above benchmark**: benchmark gap is greater than the positive neutral band.
- **Near benchmark**: benchmark gap falls inside the neutral band.
- **Below benchmark**: benchmark gap is less than the negative neutral band.

## Project structure

```text
SynthBench/
├── global.R
├── ui.R
├── server.R
├── README.md
├── R/
│   ├── data.R
│   ├── config.R
│   ├── labels.R
│   ├── app_choices.R
│   ├── model_groups.R
│   ├── result_table.R
│   ├── settings_summary.R
│   ├── metrics.R
│   ├── cv_helpers.R
│   ├── model_terms.R
│   ├── comparison_bands.R
│   ├── model_design.R
│   ├── coefficients.R
│   ├── workflow.R
│   ├── visual_theme.R
│   ├── year_coef.R
│   ├── visual_helper.R
│   └── run_helpers.R
├── www/
│   └── style.css
├── input_data/
│   ├── APP_DATA_flat.csv
│   └── LEA_META.csv
├── prep/
│   ├── organize.R
│   └── data/
├── dev/
│   └── smoke_test.R
├── docs/
│   ├── DATA_CONTRACT.md
│   └── WORKFLOW_GUIDE.md
└── rsconnect/
```

## Source file map

The app sources shared setup from `global.R`. The source files are organized by responsibility so `server.R` stays focused on Shiny reactivity, rendering, and downloads.

### Core setup

- `R/data.R` loads and validates the flat app data.
- `R/config.R` defines app settings, model defaults, feature groups, and user-facing group choices.
- `R/labels.R` defines shared grade, group, and feature labels.
- `R/app_choices.R` builds UI choices for assessment, grade, year, and tested-student thresholds.
- `R/model_groups.R` discovers available benchmark feature groups and builds model-toggle vectors.

### Modeling workflow

- `R/metrics.R` contains weighted metric and residual-summary helpers.
- `R/cv_helpers.R` contains grouped cross-validation helpers.
- `R/model_terms.R` parses model-matrix terms into feature groups and coefficient components.
- `R/comparison_bands.R` builds selected baseline rank-band lookup tables.
- `R/model_design.R` builds formula labels, model terms, and design matrices.
- `R/coefficients.R` builds coefficient diagnostic summaries.
- `R/workflow.R` orchestrates model preparation, repeated grouped-CV fitting, output combining, selected-year reporting, and workflow-level audit objects.

### App output helpers

- `R/result_table.R` builds the full-results display table and Excel workbook.
- `R/settings_summary.R` builds the run-settings summary shown in the UI.
- `R/run_helpers.R` connects workflow outputs to app-ready visualization objects.

### Visuals

- `R/visual_theme.R` defines shared plot style constants and themes.
- `R/visual_helper.R` builds benchmark comparison visuals and summary tables.
- `R/year_coef.R` builds the year-specific benchmark feature weight table.

## Key app files

- **global.R** loads packages and sources the app modules in dependency order.
- **ui.R** defines the app interface and user-facing copy.
- **server.R** manages Shiny reactivity, model-run events, output rendering, and downloads.
- **www/style.css** controls the app styling.
- **prep/organize.R** builds the `input_data` files from prepared source data.
- **dev/smoke_test.R** verifies data loading, helper-source availability, workflow execution, and display-table preparation.

## Data requirements

The app expects these files in `input_data/`:

- `APP_DATA_flat.csv`
- `LEA_META.csv`

`APP_DATA_flat.csv` is the main modeling source. It contains one row per school-year-grade-assessment modeling unit.

`LEA_META.csv` provides district and school metadata used in final outputs.

The data-prep pipeline is expected to provide a stable modeling contract, including:

- one row per school-year-grade-assessment modeling unit
- required identifiers such as `model_key`, `SchoolYear`, `SchoolCode`, `ModelGrade`, and `AssessmentLabel`
- the outcome column specified by `SETTINGS$outcome`
- the tested-student count column `n`
- the weight column specified by `SETTINGS$weight_var`
- school-year indicator columns
- benchmark feature columns used by the configured benchmark groups

See `docs/DATA_CONTRACT.md` for details.

## Run locally

1. Clone or download the project.
2. Make sure `input_data/APP_DATA_flat.csv` and `input_data/LEA_META.csv` are present.
3. Install the required packages.
4. Open the project in RStudio or another R environment.
5. Run the app from the project root.

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "glmnet",
  "gt",
  "DT",
  "ggtext",
  "scales",
  "openxlsx"
))

shiny::runApp()
```

## Smoke test

Run the smoke test from the project root after refactors or data updates:

```r
source("dev/smoke_test.R")
```

The smoke test should confirm:

- data files load and validate
- refactored helper files are sourced correctly
- core workflow helpers run in sequence
- `run_benchmark_workflow()` completes
- app-ready full-results table helpers are available

## Current defaults

Current core defaults are stored in `R/config.R` and include:

- outcome: `ScaleScore.mean`
- school identifier: `SchoolCode`
- weight variable: `w_sqrt_n`
- mandatory group: `school_year`
- ridge lambda choice: `lambda.1se`
- grouped CV folds: `10`, with a smaller-fold fallback for smaller samples
- repeated CV runs: `5`
- neutral-band multiplier: `0.5`
- tracked baseline rank bands: default `0-10% vs 90-100%` using 10 equal baseline-rank bands

Default model definitions are:

- **Baseline**: low income + ELL + SPED
- **Alt 1**: low income + ELL
- **Alt 2**: low income + SPED
- **Alt 3**: low income

## Simplified workflow

The main workflow is intentionally split into focused helpers while `R/workflow.R` remains the orchestrator:

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

See `docs/WORKFLOW_GUIDE.md` for debugging instructions.

## Development notes

A few implementation choices are intentional:

- school year is always included in every benchmark model
- benchmark groups define the comparison frame rather than the true causes of performance
- year-by-feature interactions allow benchmark feature weights to vary by school year
- grouped cross-validation keeps all rows from the same school in the same fold
- repeated fold assignments reduce dependence on any one random split
- final benchmark scores are aggregated from out-of-fold predictions across repeats
- scaled benchmark gaps are for comparison and ranking, not inference
- `workflow.R` remains the high-level orchestrator, while reusable helpers live in focused source files

These choices are meant to support transparent, stable historical benchmarking rather than one final adjusted truth.
