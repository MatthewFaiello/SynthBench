# =========================================================
# R/app_choices.R
# UI choice helpers for assessment, grade, year, and thresholds
# =========================================================
# Purpose:
#   Build user-facing choices for app controls from the OPTIONS
#   lookup table created by R/data.R.
#
# Main helpers:
#   - get_scope_2_choices()
#   - get_year_choices()
#   - get_range_n()
#   - get_model_keys()
# =========================================================


get_scope_2_choices <- function(scope_1) {
  vals <- OPTIONS %>%
    filter(AssessmentLabel == scope_1) %>%
    distinct(ModelGrade) %>%
    pull(ModelGrade) %>%
    sort()
  
  setNames(vals, grade_labels[vals])
}


get_year_choices <- function(scope_1, scope_2) {
  vals <- OPTIONS %>%
    filter(
      AssessmentLabel == scope_1,
      ModelGrade == scope_2
    ) %>%
    distinct(SchoolYear) %>%
    pull(SchoolYear) %>%
    sort()
  
  labels <- paste0(as.integer(vals) - 1, "-", substr(vals, 3, 4))
  
  setNames(vals, labels)
}


get_range_n <- function(scope_1, scope_2) {
  OPTIONS %>%
    filter(
      AssessmentLabel == scope_1,
      ModelGrade == scope_2
    ) %>%
    summarise(
      n_min = min(n_min, na.rm = TRUE),
      n_max = max(n_max, na.rm = TRUE)
    )
}


get_model_keys <- function(scope_1, scope_2) {
  OPTIONS %>%
    filter(
      AssessmentLabel == scope_1,
      ModelGrade == scope_2
    ) %>%
    distinct(model_key) %>%
    pull(model_key)
}