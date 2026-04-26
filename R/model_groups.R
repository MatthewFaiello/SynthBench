# =========================================================
# R/model_groups.R
# Benchmark feature-group helpers
# =========================================================
# Purpose:
#   Discover available predictor columns and build model-toggle
#   vectors used by the benchmark workflow.
#
# Main helpers:
#   - get_predictor_groups()
#   - build_toggle_vector()
# =========================================================

get_predictor_groups <- function(dat) {
  groups <- lapply(FEATURE_GROUP_PATTERNS, function(pattern) {
    names(dat)[grepl(pattern, names(dat))]
  })
  
  # Make sure the mandatory group exists.
  mandatory_group <- SETTINGS$mandatory_group
  
  if (!mandatory_group %in% names(groups)) {
    stop(
      "SETTINGS$mandatory_group is not listed in FEATURE_GROUP_PATTERNS: ",
      mandatory_group,
      call. = FALSE
    )
  }
  
  if (length(groups[[mandatory_group]]) == 0) {
    stop(
      "No columns found for mandatory predictor group: ",
      mandatory_group,
      call. = FALSE
    )
  }
  
  # Make sure every user-toggleable group has a pattern.
  missing_toggle_patterns <- setdiff(
    names(DEFAULT_GROUP_TOGGLES),
    names(groups)
  )
  
  if (length(missing_toggle_patterns) > 0) {
    stop(
      "DEFAULT_GROUP_TOGGLES contains groups missing from FEATURE_GROUP_PATTERNS: ",
      paste(missing_toggle_patterns, collapse = ", "),
      call. = FALSE
    )
  }
  
  groups
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