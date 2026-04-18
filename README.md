# SynthBench

Synthetic assessment benchmarking app built in **R Shiny**.

## Overview

SynthBench is a historical benchmarking tool for comparing school-level average scale scores under user-defined comparison frames. The app fits benchmark models to a selected assessment-and-grade dataset, then compares observed school-year performance to model-based benchmark predictions.

The goal is transparent, benchmark-adjusted comparison. It is **not** designed for causal inference, policy-effect estimation, or future-year forecasting.

## What the app does

Users can:

- choose an **assessment** and **grade**
- set a **minimum tested-student threshold**
- choose the **year shown in the comparison views**
- adjust the **neutral-band width**
- define one **baseline** model and up to three **alternative** benchmark models

The app then:

1. selects the matching historical dataset
2. filters out school-years below the tested-student threshold
3. fits benchmark models using ridge regression
4. generates out-of-fold benchmark predictions using repeated grouped cross-validation by school
5. computes benchmark-adjusted scores, residuals, ranks, and direction labels
6. compares results across benchmark definitions for the selected year

## Main outputs

The app currently includes six main result views:

- **Benchmarking overview**
- **Rank comparison**
- **Rank movement**
- **Average rank movement**
- **Scores and residuals**
- **Full results** table with download option

The comparison plots focus on tracked schools from the selected year, while the full results table includes all schools in that year.

## Current benchmark groups

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
│   └── visual_helper.R
├── www/
│   └── style.css
├── input_data/
│   ├── APP_DATA.rds
│   └── LEA_META.rds
├── documentation/
├── prep/
├── data/
└── rsconnect/
```

## Key files

- **global.R** loads packages and sources the app modules
- **ui.R** defines the app interface
- **server.R** manages reactivity, model runs, and outputs
- **R/config.R** stores app settings and default model definitions
- **R/data.R** loads the app datasets and builds the scope lookup table
- **R/helpers.R** contains helper functions for UI selections and model toggles
- **R/workflow.R** runs the main benchmark modeling workflow
- **R/visual_helper.R** builds the plots and summary tables
- **www/style.css** controls the app styling

## Data requirements

The app expects the following files in `input_data/`:

- `APP_DATA.rds`
- `LEA_META.rds`

`APP_DATA` is the main modeling source. `LEA_META` is used to join district and school metadata into the final outputs.

## Run locally

1. Clone or download the project
2. Make sure `input_data/APP_DATA.rds` and `input_data/LEA_META.rds` are present
3. Install the required packages
4. Open the project in RStudio
5. Run the app from the project root

```r
install.packages(c("shiny", "tidyverse", "glmnet", "gt", "DT", "ggtext"))
shiny::runApp()
```

## Method notes

SynthBench is built for **historical benchmarking only**.

A few important interpretation points:

- benchmark definitions are intentionally user-controlled
- the selected year controls the comparison views, not the full model-training sample
- the neutral band is a practical reading aid, not a significance test
- the app is meant to show whether conclusions are sensitive to the chosen comparison frame

## Current defaults

Current core defaults include:

- ridge regression via `glmnet`
- `lambda_choice = "lambda.1se"`
- `nfolds = 10`
- `cv_repeats = 20`
- `neutral_band_multiplier = 1/3`
- `comparison_top_n = 10`

Default model definitions are:

- **Baseline**: low income
- **Alt 1**: low income + ELL
- **Alt 2**: low income + SPED
- **Alt 3**: low income + ELL + SPED

## Beta status

This app should still be treated as a working prototype. The interface and core workflow are in place, but the app is still being documented and refined.

## Deployment notes

If you deploy the app, keep the project root structure intact so it can find:

- `R/`
- `www/`
- `input_data/`
- `global.R`
- `ui.R`
- `server.R`

## Contact

Questions, feedback, and bug reports are welcome.
