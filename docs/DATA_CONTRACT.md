# Data contract

The app expects two flat CSV files in `input_data/`:

```text
input_data/APP_DATA_flat.csv
input_data/LEA_META.csv
```

These files are produced by `prep/organize.R`.

## Contract summary

The app uses a flat, school-year modeling contract:

- `APP_DATA_flat.csv` contains the model-ready school-year-grade-assessment records.
- `LEA_META.csv` contains school and district names used in final outputs.
- `R/data.R` loads and validates both files during app startup.
- `R/config.R` defines the feature-group patterns used to discover benchmark model inputs.

The app should fail early if required columns are missing or if duplicate key rows are present.

## `APP_DATA_flat.csv`

This is the main modeling table. It should contain one row per school-year-grade-assessment modeling unit.

### Required columns

| Column | Meaning |
|---|---|
| `model_key` | Stable assessment-grade key used by the app. |
| `SchoolYear` | Reporting year as an integer. |
| `SchoolCode` | School identifier. |
| `ModelGrade` | Grade or grade grouping used by the model. |
| `AssessmentLabel` | User-facing assessment label. |
| `ScaleScore.mean` | Outcome: school-year average scale score. |
| `n` | Tested-student count. |
| `w_sqrt_n` | Model weight, currently `sqrt(n)`. |
| `na` | Percent eligible students with missing scores. |
| `units` | Student unit count from unit-count data. |

### Required feature-column patterns

The app finds benchmark features using `FEATURE_GROUP_PATTERNS` in `R/config.R`. These group names should stay aligned with `DEFAULT_GROUP_TOGGLES`, `GROUP_CHOICES`, and any shared label maps in `R/labels.R`.

| Group | Column pattern | Notes |
|---|---|---|
| `school_year` | `SchoolYear.*` | Mandatory; always included in every model. |
| `na` | `na` | Percent eligible students with missing scores. |
| `units` | `units` | Student unit count. |
| `county` | `County_Name.*` | County composition indicators. |
| `ell` | `ELL.*` | English learner indicators. |
| `foster_care` | `FosterCare.*` | Foster care indicator. |
| `gender` | `Gender.*` | Gender indicators. |
| `geography` | `Geography.*` | Wilmington geography indicator. |
| `homeless` | `Homeless.*` | Homelessness indicator. |
| `immersion` | `Immersion.*` | Immersion indicator. |
| `low_income` | `LowIncome.*` | Low income indicator. |
| `migrant` | `Migrant.*` | Migrant indicator. |
| `military_dep` | `MilitaryDep.*` | Military-connected indicator. |
| `race` | `RaceReportTitle.*` | Race/ethnicity indicators. |
| `sped` | `SPEDCode.*` | SPED code indicators. |

Some groups may have only one column. The `school_year` group must have at least one column.

### Uniqueness rule

There should be no duplicate rows by:

```r
SchoolYear, SchoolCode, ModelGrade, AssessmentLabel
```

`R/data.R` checks this during app startup.

### Modeling notes

- The app uses `SETTINGS$outcome` as the outcome column. The current expected outcome is `ScaleScore.mean`.
- The app uses `SETTINGS$weight_var` as the model weight column. The current expected weight is `w_sqrt_n`.
- The workflow filters rows below the selected tested-student threshold before model fitting.
- `model_key` should be stable across data refreshes so UI selections remain predictable.

## `LEA_META.csv`

This table supplies school and district names for final outputs.

### Required columns

| Column | Meaning |
|---|---|
| `SchoolYear` | Reporting year as an integer. |
| `SchoolCode` | School identifier. |
| `ModelGrade` | Grade or grade grouping used by the model. |
| `DistrictName` | District name displayed in outputs. |
| `SchoolName` | School name displayed in outputs. |

### Join rule

The workflow joins metadata to modeled rows by:

```r
SchoolYear, SchoolCode, ModelGrade
```

There should be no duplicate rows by that key.

## Data-prep source objects

The final flat CSV files come from these objects inside `prep/organize.R`:

| Object | Purpose |
|---|---|
| `training_matrix` | Main model-ready school-year table. |
| `schools_meta` / `lea_meta_flat` | School and district metadata. |

## Validation checks to keep

Do not remove these checks from `R/data.R`:

- required columns in `APP_DATA_flat.csv`
- duplicate rows in `APP_DATA_flat.csv`
- required columns in `LEA_META.csv`
- duplicate metadata keys in `LEA_META.csv`, if present in the current validation layer
- non-empty loaded data objects

These checks make startup errors much easier to interpret.

## Helper files that depend on this contract

The refactored app uses this contract across several files:

- `R/data.R` loads the flat files and builds lookup objects such as `APP_DATA`, `APP_DATA_FLAT`, `LEA_META`, `OPTIONS`, and `SCOPE_1_CHOICES`.
- `R/app_choices.R` uses `OPTIONS` to build assessment, grade, year, and threshold UI choices.
- `R/model_groups.R` uses `FEATURE_GROUP_PATTERNS` to discover available model columns.
- `R/model_design.R` builds model terms and design matrices from discovered predictor groups.
- `R/workflow.R` filters data, fits benchmark models, joins metadata, and creates app-ready output tables.
- `R/result_table.R` formats the final selected-year full-results table and Excel workbook.

## Data-refresh checklist

After regenerating `input_data/APP_DATA_flat.csv` and `input_data/LEA_META.csv`:

1. Confirm both files exist in `input_data/`.
2. Confirm `APP_DATA_flat.csv` contains all required columns.
3. Confirm `LEA_META.csv` contains all required columns.
4. Confirm no duplicate rows exist by the required modeling and metadata keys.
5. Confirm at least one `model_key` is available.
6. Run the project smoke test:

```r
source("dev/smoke_test.R")
```

The smoke test should pass before running or deploying the app.
