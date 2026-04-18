# =========================== #
# Synthetic Benchmark Workflow
# =========================== #
# Purpose:
#   Estimate a school's expected ScaleScore.mean in a given SchoolYear,
#   conditional on a user-selected feature set. Selected feature effects
#   are allowed to vary by year through SchoolYear interactions.
#
# Core interpretation:
#   observed score - predicted benchmark score
#   > neutral band  => above_expected
#   within band     => near_expected
#   < -neutral band => below_expected
#
# Weighting choice:
#   sqrt(n)
#   This gives school-years with more tested students more influence,
#   but with diminishing returns relative to raw n.

# --------------------------- packages & data --------------------------- #
library(tidyverse)
library(glmnet)

LEA_META = read_rds(file.path("input_data", "LEA_META.rds"))
APP_DATA = read_rds(file.path("input_data", "APP_DATA.rds"))

# --------------------------- user settings --------------------------- #
outcome <- "ScaleScore.mean"
weight_var <- "w_sqrt_n"
school_id <- "SchoolCode"

lambda_grid <- exp(seq(log(1e-4), log(10), length.out = 400))
nfolds <- 10
seed <- 123

# Global neutral band used to classify residual direction.
neutral_band_multiplier <- 1 / 3

# ------------------- target selection & data prep -------------------------- #
df <- APP_DATA$grade_03__DeSSA.ELA %>%
  mutate(
    .row_id = row_number(),
    w_sqrt_n = sqrt(n)
  )
write_csv(df, "~/Desktop/df.csv")
# --------------------------- user predictor settings --------------------------- #
# SchoolYear is always included.
mandatory_groups <- c("school_year")

# Toggleable benchmark components selected by the user.
group_toggles <- c(
  na           = FALSE,
  units        = FALSE,
  county       = FALSE,
  ell          = FALSE,
  foster_care  = FALSE,
  gender       = FALSE,
  geography    = FALSE,
  homeless     = FALSE,
  immersion    = FALSE,
  low_income   = FALSE,
  migrant      = FALSE,
  military_dep = FALSE,
  race         = FALSE,
  sped         = FALSE
)

# Group definitions based on columns already present in df.
named_predictor_groups <- list(
  school_year  = names(df)[startsWith(names(df), "SchoolYear.")],
  na           = "na", # number of eligible students with missing test scores
  units        = "units", # number of unit the school was given by unit count (proxy for school size)
  county       = names(df)[startsWith(names(df), "County_Name.")],
  ell          = names(df)[startsWith(names(df), "ELL.")],
  foster_care  = names(df)[startsWith(names(df), "FosterCare.")],
  gender       = names(df)[startsWith(names(df), "Gender.")],
  geography    = names(df)[startsWith(names(df), "Geography.")],
  homeless     = names(df)[startsWith(names(df), "Homeless.")],
  immersion    = names(df)[startsWith(names(df), "Immersion.")],
  low_income   = names(df)[startsWith(names(df), "LowIncome.")],
  migrant      = names(df)[startsWith(names(df), "Migrant.")],
  military_dep = names(df)[startsWith(names(df), "MilitaryDep.")],
  race         = names(df)[startsWith(names(df), "RaceReportTitle.")],
  sped         = names(df)[startsWith(names(df), "SPEDCode.")]
)

# Variables that should not enter the model as predictors.
non_predictor_columns <- c(
  "SchoolYear",
  "SchoolCode",
  "ModelGrade",
  "AssessmentLabel",
  "n",
  ".row_id",
  "w_sqrt_n"
)

# --------------------------- helper functions --------------------------- #

weighted_metrics <- function(y, pred, w) {
  resid <- y - pred
  ybar <- weighted.mean(y, w)
  sse <- sum(w * resid^2)
  sst <- sum(w * (y - ybar)^2)
  
  tibble(
    wrmse = sqrt(weighted.mean(resid^2, w)),
    wmae  = weighted.mean(abs(resid), w),
    wr2   = ifelse(sst > 0, 1 - sse / sst, NA_real_)
  )
}

weighted_sd <- function(x, w) {
  mu <- weighted.mean(x, w)
  sqrt(weighted.mean((x - mu)^2, w))
}

make_school_foldid <- function(dat, school_id = "SchoolCode", nfolds = 10, seed = 123) {
  set.seed(seed)
  schools <- unique(dat[[school_id]])
  k <- min(nfolds, length(schools))
  school_fold <- sample(rep(seq_len(k), length.out = length(schools)))
  names(school_fold) <- schools
  as.integer(school_fold[match(dat[[school_id]], schools)])
}

add_performance_direction <- function(dat, residual_col, neutral_band) {
  dat %>%
    mutate(
      .performance_direction = case_when(
        .data[[residual_col]] >  neutral_band ~ "above_expected",
        .data[[residual_col]] < -neutral_band ~ "below_expected",
        TRUE                                  ~ "near_expected"
      )
    )
}

# Quick summary of performance direction.
summarise_performance_direction <- function(dat, weight_var = "w_sqrt_n") {
  dat %>%
    count(.performance_direction, name = "n_rows") %>%
    left_join(
      dat %>%
        group_by(.performance_direction) %>%
        summarise(weighted_total = sum(.data[[weight_var]]), .groups = "drop"),
      by = ".performance_direction"
    ) %>%
    mutate(
      pct_rows = n_rows / sum(n_rows),
      pct_weighted = weighted_total / sum(weighted_total)
    ) %>%
    arrange(match(.performance_direction, c("above_expected", "near_expected", "below_expected")))
}

summarise_weighted_performance <- function(dat, outcome, pred_col, weight_var) {
  weighted_metrics(
    y = dat[[outcome]],
    pred = dat[[pred_col]],
    w = dat[[weight_var]]
  )
}

get_group_columns <- function(group_names, named_predictor_groups) {
  cols <- unlist(named_predictor_groups[group_names], use.names = FALSE)
  unique(cols[!is.na(cols) & nzchar(cols)])
}

# Build:
#   outcome ~ SchoolYear + selected features + SchoolYear:selected features
build_benchmark_formula <- function(
    outcome,
    weight_var,
    school_id,
    mandatory_groups,
    group_toggles,
    named_predictor_groups,
    non_predictor_columns
) {
  year_cols <- get_group_columns(mandatory_groups, named_predictor_groups)
  
  if (length(year_cols) == 0) {
    stop("No SchoolYear.* columns found. SchoolYear main effects are required.")
  }
  
  selected_groups <- names(group_toggles)[group_toggles]
  selected_groups <- setdiff(selected_groups, mandatory_groups)
  
  selected_cols <- get_group_columns(selected_groups, named_predictor_groups)
  
  year_cols <- setdiff(year_cols, c(outcome, weight_var, school_id, non_predictor_columns))
  selected_cols <- setdiff(selected_cols, c(outcome, weight_var, school_id, non_predictor_columns))
  
  main_terms <- c(year_cols, selected_cols)
  
  if (length(main_terms) == 0) {
    stop("No predictors available after filtering.")
  }
  
  interaction_terms <- character(0)
  if (length(selected_cols) > 0) {
    interaction_terms <- as.vector(outer(year_cols, selected_cols, paste, sep = ":"))
  }
  
  rhs_terms <- unique(c(main_terms, interaction_terms))
  as.formula(paste(outcome, "~", paste(rhs_terms, collapse = " + ")))
}

build_design_matrix <- function(formula_obj, dat) {
  mm <- model.matrix(formula_obj, data = dat)
  
  if ("(Intercept)" %in% colnames(mm)) {
    mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  }
  
  keep_cols <- apply(mm, 2, sd, na.rm = TRUE) > 0
  mm[, keep_cols, drop = FALSE]
}

align_new_matrix <- function(X_new, x_columns) {
  missing_cols <- setdiff(x_columns, colnames(X_new))
  if (length(missing_cols) > 0) {
    X_new <- cbind(
      X_new,
      matrix(
        0,
        nrow = nrow(X_new),
        ncol = length(missing_cols),
        dimnames = list(NULL, missing_cols)
      )
    )
  }
  
  extra_cols <- setdiff(colnames(X_new), x_columns)
  if (length(extra_cols) > 0) {
    X_new <- X_new[, setdiff(colnames(X_new), extra_cols), drop = FALSE]
  }
  
  X_new[, x_columns, drop = FALSE]
}

describe_benchmark_spec <- function(group_toggles, mandatory_groups = "school_year") {
  selected_groups <- names(group_toggles)[group_toggles]
  tibble(
    component = c("mandatory", rep("toggle_on", length(selected_groups))),
    group = c(mandatory_groups, selected_groups)
  )
}

# --------------------------- fit model --------------------------- #
fit_ridge_benchmark_model <- function(
    df,
    outcome,
    weight_var,
    school_id,
    mandatory_groups,
    group_toggles,
    named_predictor_groups,
    non_predictor_columns,
    lambda_grid,
    nfolds = 10,
    seed = 123,
    neutral_band_multiplier = 1 / 3
) {
  y <- df[[outcome]]
  w <- df[[weight_var]]
  
  formula_obj <- build_benchmark_formula(
    outcome = outcome,
    weight_var = weight_var,
    school_id = school_id,
    mandatory_groups = mandatory_groups,
    group_toggles = group_toggles,
    named_predictor_groups = named_predictor_groups,
    non_predictor_columns = non_predictor_columns
  )
  
  X <- build_design_matrix(formula_obj, df)
  foldid <- make_school_foldid(df, school_id, nfolds, seed)
  
  cv_fit <- cv.glmnet(
    x = X,
    y = y,
    weights = w,
    alpha = 0,
    lambda = lambda_grid,
    family = "gaussian",
    standardize = TRUE,
    foldid = foldid,
    keep = TRUE
  )
  
  idx_1se <- which.min(abs(cv_fit$lambda - cv_fit$lambda.1se))
  
  # Out-of-fold predictions for honest scoring of observed rows
  pred_cv_1se <- as.numeric(cv_fit$fit.preval[, idx_1se])
  
  # In-sample predictions for diagnostics only
  pred_in_1se <- as.numeric(predict(cv_fit, newx = X, s = "lambda.1se"))
  
  scored_data <- df %>%
    mutate(
      .foldid = foldid,
      .pred_cv_1se = pred_cv_1se,
      .pred_in_1se = pred_in_1se,
      .resid_cv_1se = .data[[outcome]] - .pred_cv_1se,
      .resid_in_1se = .data[[outcome]] - .pred_in_1se
    )
  
  residual_weighted_sd_cv <- weighted_sd(scored_data$.resid_cv_1se, scored_data[[weight_var]])
  neutral_band_cv <- residual_weighted_sd_cv * neutral_band_multiplier
  
  scored_data <- scored_data %>%
    mutate(
      .resid_cv_weighted_sd = residual_weighted_sd_cv,
      .neutral_band_cv = neutral_band_cv,
      .performance_z_cv = .resid_cv_1se / .resid_cv_weighted_sd,
      .performance_z_in = .resid_in_1se / .resid_cv_weighted_sd
    ) %>%
    add_performance_direction(
      residual_col = ".resid_cv_1se",
      neutral_band = neutral_band_cv
    ) %>%
    group_by(
      SchoolYear
    ) %>% 
    arrange(desc(.performance_z_cv)) %>%
    mutate(.performance_rank_cv = min_rank(desc(.performance_z_cv))) %>% 
    ungroup()
  
  coef_1se <- as.matrix(coef(cv_fit, s = "lambda.1se"))
  coef_table_1se <- tibble(
    term = rownames(coef_1se),
    estimate = as.numeric(coef_1se[, 1])
  ) %>%
    filter(term != "(Intercept)") %>%
    arrange(desc(abs(estimate)))
  
  model_summary <- tibble(
    model = "ridge_benchmark",
    n_obs = nrow(df),
    n_schools = n_distinct(df[[school_id]]),
    n_features = ncol(X),
    nfolds = length(unique(foldid)),
    lambda_1se = cv_fit$lambda.1se,
    wrmse_cv_1se = weighted_metrics(y, pred_cv_1se, w)$wrmse,
    wmae_cv_1se = weighted_metrics(y, pred_cv_1se, w)$wmae,
    wr2_cv_1se = weighted_metrics(y, pred_cv_1se, w)$wr2,
    wrmse_in_1se = weighted_metrics(y, pred_in_1se, w)$wrmse,
    wmae_in_1se = weighted_metrics(y, pred_in_1se, w)$wmae,
    wr2_in_1se = weighted_metrics(y, pred_in_1se, w)$wr2,
    resid_cv_weighted_sd = residual_weighted_sd_cv,
    neutral_band_multiplier = neutral_band_multiplier,
    neutral_band_cv = neutral_band_cv
  )
  
  direction_summary <- summarise_performance_direction(scored_data, weight_var)
  
  year_performance <- scored_data %>%
    group_by(SchoolYear) %>%
    group_modify(~ summarise_weighted_performance(.x, outcome, ".pred_cv_1se", weight_var)) %>%
    ungroup()
  
  school_performance <- scored_data %>%
    group_by(.data[[school_id]]) %>%
    group_modify(~ summarise_weighted_performance(.x, outcome, ".pred_cv_1se", weight_var)) %>%
    ungroup()
  
  final_fit <- glmnet(
    x = X,
    y = y,
    weights = w,
    alpha = 0,
    lambda = cv_fit$lambda,
    family = "gaussian",
    standardize = TRUE
  )
  
  list(
    final_fit = final_fit,
    scored_data = scored_data,
    x_columns = colnames(X),
    formula_obj = formula_obj,
    lambda_1se = cv_fit$lambda.1se,
    benchmark_spec = describe_benchmark_spec(group_toggles, mandatory_groups),
    model_summary = model_summary,
    coef_table_1se = coef_table_1se,
    direction_summary = direction_summary,
    year_performance = year_performance,
    school_performance = school_performance
  )
}


# --------------------------- example toggle specification --------------------------- #
# Example:
#   benchmark always includes SchoolYear
#   benchmark also conditions on na, county, low_income, and race
group_toggles <- c(
  na           = TRUE,
  units        = FALSE,
  county       = TRUE,
  ell          = FALSE,
  foster_care  = FALSE,
  gender       = FALSE,
  geography    = FALSE,
  homeless     = FALSE,
  immersion    = FALSE,
  low_income   = TRUE,
  migrant      = FALSE,
  military_dep = FALSE,
  race         = TRUE,
  sped         = FALSE
)

fit_main <- fit_ridge_benchmark_model(
  df = df,
  outcome = outcome,
  weight_var = weight_var,
  school_id = school_id,
  mandatory_groups = mandatory_groups,
  group_toggles = group_toggles,
  named_predictor_groups = named_predictor_groups,
  non_predictor_columns = non_predictor_columns,
  lambda_grid = lambda_grid,
  nfolds = nfolds,
  seed = seed,
  neutral_band_multiplier = neutral_band_multiplier
)

# --------------------------- outputs --------------------------- #
fit_main$benchmark_spec
fit_main$model_summary
fit_main$coef_table_1se
fit_main$direction_summary
fit_main$year_performance
fit_main$school_performance

pred <- fit_main$scored_data %>%
  left_join(LEA_META %>% distinct(DistrictName,
                                  SchoolName,
                                  SchoolCode,
                                  ModelGrade)) %>% 
  select(
    .row_id,
    DistrictName,
    SchoolName,
    SchoolCode,
    ModelGrade,
    SchoolYear,
    all_of(outcome),
    .pred_cv_1se,
    .resid_cv_1se,
    .resid_cv_weighted_sd,
    .neutral_band_cv,
    .performance_z_cv,
    .performance_direction,
    .performance_rank_cv
  )

sum(is.na(pred$SchoolName))


