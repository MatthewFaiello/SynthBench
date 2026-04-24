# Model-Based Benchmarking

Historical assessment benchmarking app built in **R Shiny**.

## Overview

Model-Based Benchmarking is a historical benchmarking tool for school-level average scale scores. The app fits benchmark models to a selected assessment-and-grade history, then compares each observed school-year score with a model-based benchmark score under user-defined comparison frames.

The goal is transparent, benchmark-adjusted comparison. The app is **not** designed for causal inference, policy-effect estimation, future-year forecasting, or stand-alone accountability judgment.

## What the app does

Users can:

- choose an **assessment** and **grade**
- set a **minimum tested-student threshold**
- choose the **year shown in the comparison views**
- adjust the **neutral-band width**
- define one **baseline** benchmark model and up to three **alternative** benchmark models

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

The app uses plain-language labels where possible:

- **Benchmark score** = the model-based score for a school-year under the chosen comparison frame.
- **Benchmark gap** = observed score minus benchmark score.
- **Scaled benchmark gap** = benchmark gap divided by the model-level weighted residual standard deviation. This is used for comparison and ranking, not as a formal z-score or significance test.
- **Benchmark label** = above benchmark, near benchmark, or below benchmark, based on the user-selected neutral band.
- **Within-year rank** = the school's benchmark-adjusted position relative to other schools in the same year.
- **Benchmark feature weights** = year-specific standardized coefficient-weight diagnostics. These are model diagnostics, not causal effects or stand-alone importance measures.

Some internal code objects still use legacy names such as `.pred_cv`, `.resid_cv`, `.performance_z_cv`, and `.performance_direction`. Those names are implementation details; the user-facing app uses the terminology above.

## Main result views

The app currently includes these result views:

- **Benchmarking overview**: summarizes tracked baseline-school sensitivity across benchmark definitions.
- **Benchmark feature weights**: shows year-specific standardized coefficient-weight diagnostics for the selected year.
- **Rank comparison**: compares each tracked baseline school's baseline rank with its rank under alternative benchmark definitions.
- **Rank movement**: shows rank shifts for tracked baseline schools across benchmark definitions.
- **Average rank movement**: summarizes average rank movement among tracked baseline schools.
- **Scores and benchmark gaps**: compares benchmark scores, benchmark gaps, scaled benchmark gaps, and within-year ranks across benchmark definitions.
- **Full results**: lists all schools in the selected year for the fitted benchmark models and allows download to Excel.

Most plots track the top and bottom baseline schools in the selected year. The full results table includes all schools in the selected year.

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
│   ├── config.R
│   ├── data.R
│   ├── helpers.R
│   ├── workflow.R
│   ├── year_coef.R
│   └── visual_helper.R
├── www/
│   └── style.css
├── input_data/
│   ├── APP_DATA.rds
│   └── LEA_META.rds
├── prep/
│   ├── organize.R
│   └── data/
└── rsconnect/
```

## Key files

- **global.R** loads packages and sources the app modules.
- **ui.R** defines the app interface and user-facing copy.
- **server.R** manages reactivity, model runs, outputs, and downloads.
- **R/config.R** stores app settings and default model definitions.
- **R/data.R** loads the app datasets and builds scope lookup tables.
- **R/helpers.R** contains helper functions for UI choices and benchmark-group toggles.
- **R/workflow.R** runs the main benchmark modeling workflow.
- **R/year_coef.R** builds the benchmark feature weight table.
- **R/visual_helper.R** builds comparison plots and summary tables.
- **www/style.css** controls the app styling.
- **prep/organize.R** builds the `input_data` files from prepared source data.

## Data requirements

The app expects these files in `input_data/`:

- `APP_DATA.rds`
- `LEA_META.rds`

`APP_DATA` is the main modeling source. It should be a named list of assessment-and-grade datasets. Each element should contain the school-year rows and prepared model inputs used by the workflow.

`LEA_META` provides district and school metadata used in final outputs.

The data-prep pipeline is expected to provide a stable modeling contract, including:

- one row per school-year-grade-assessment modeling unit
- required identifiers such as `SchoolYear`, `SchoolCode`, `ModelGrade`, and assessment label fields
- the outcome column specified by `SETTINGS$outcome`
- the tested-student count column `n`
- the weight column specified by `SETTINGS$weight_var`
- school-year indicator columns
- benchmark feature columns used by the configured benchmark groups

## Run locally

1. Clone or download the project.
2. Make sure `input_data/APP_DATA.rds` and `input_data/LEA_META.rds` are present.
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

## Current defaults

Current core defaults are stored in `R/config.R` and include:

- outcome: `ScaleScore.mean`
- school identifier: `SchoolCode`
- weight variable: `w_sqrt_n`
- mandatory group: `school_year`
- ridge lambda choice: `lambda.1se`
- grouped CV folds: `10`, with a smaller-fold fallback for smaller samples
- repeated CV runs: `5`
- neutral-band multiplier: `1/3`
- comparison top/bottom count: `10`

Default model definitions are:

- **Baseline**: low income
- **Alt 1**: low income + ELL
- **Alt 2**: low income + SPED
- **Alt 3**: low income + ELL + SPED

## Development notes

A few implementation choices are intentional:

- school year is always included in every benchmark model
- benchmark groups define the comparison frame rather than the true causes of performance
- year-by-feature interactions allow benchmark feature weights to vary by school year
- grouped cross-validation keeps all rows from the same school in the same fold
- repeated fold assignments reduce dependence on any one random split
- final benchmark scores are aggregated from out-of-fold predictions across repeats
- scaled benchmark gaps are for comparison and ranking, not inference

These choices are meant to support transparent, stable historical benchmarking rather than one final adjusted truth.

## Pre-deployment checks

Before deploying, run a local smoke test:

```r
source("global.R")

result <- run_benchmark_workflow(
  model_key = names(APP_DATA)[1],
  selection_year = max(APP_DATA[[1]]$SchoolYear, na.rm = TRUE),
  min_students_tested = 10
)

names(result)
head(result$model_comp_table)
```

Also confirm that:

- the app starts from the project root
- `global.R` sources `R/workflow.R`, not an old workflow filename
- `www/style.css` is included in the deployment bundle
- `input_data/APP_DATA.rds` and `input_data/LEA_META.rds` are included in the deployment bundle
- the full results table downloads successfully as an `.xlsx` file
- repeated runs with the same inputs produce stable results
- unusual input combinations fail with clear messages rather than silent output changes

## Interpretation limits

Model-Based Benchmarking is a descriptive comparison tool. It helps users interpret historical school-year average scale scores in context, but it does not establish causality.

Important limits:

- A benchmark gap is not a causal effect.
- A scaled benchmark gap is not a formal z-score or significance test.
- Benchmark labels are practical comparison labels, not inferential decisions.
- Benchmark feature weights are model diagnostics, not evidence that a feature caused performance.
- The selected comparison year controls reporting, not the full historical modeling sample.
- Tracked-school visuals follow baseline-selected top and bottom schools; they do not always summarize all schools.
- The app should not be used as a stand-alone accountability system.

## License and distribution

Add project-specific license, ownership, and distribution notes here before public or external release.
