# =========================================================
# R/helpers.R
# Small helper functions used by the app
# =========================================================

grade_labels <- c(
  "03"    = "3rd",
  "04"    = "4th",
  "05"    = "5th",
  "06"    = "6th",
  "07"    = "7th",
  "08"    = "8th",
  "09_12" = "9th–12th",
  "11"    = "11th"
)

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
    summarise(n_min = min(n_min, na.rm = TRUE),
              n_max = max(n_max, na.rm = TRUE))
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

build_toggle_vector <- function(selected_groups) {
  toggles <- DEFAULT_GROUP_TOGGLES
  toggles[] <- FALSE

  if (!is.null(selected_groups) && length(selected_groups) > 0) {
    valid_groups <- intersect(selected_groups, names(toggles))
    toggles[valid_groups] <- TRUE
  }

  toggles
}
