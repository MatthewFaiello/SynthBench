# =========================================================
# R/settings_summary.R
# Helpers for displaying captured benchmark run settings
# =========================================================
# Purpose:
#   Build the plain-language settings summary shown after a
#   benchmark run is completed.
#
# Main helpers:
#   - label_selected_groups()
#   - format_school_year()
#   - format_comparison_band()
#   - settings_item()
#   - settings_summary_ui()
# =========================================================

label_selected_groups <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("School year only")
  }
  
  group_values <- unname(GROUP_CHOICES)
  group_labels <- names(GROUP_CHOICES)
  
  if (is.null(group_labels)) {
    group_labels <- group_values
  }
  
  matched <- group_labels[match(x, group_values)]
  matched <- matched[!is.na(matched)]
  
  if (length(matched) == 0) {
    "School year only"
  } else {
    paste(matched, collapse = ", ")
  }
}


format_school_year <- function(x) {
  yr <- suppressWarnings(as.integer(x))
  
  if (is.na(yr)) {
    return(as.character(x))
  }
  
  paste0(yr - 1, "-", substr(as.character(yr), 3, 4))
}

format_comparison_band <- function(band_index,
                                   band_count = SETTINGS$comparison_band_count) {
  band_index <- suppressWarnings(as.integer(band_index))
  band_count <- suppressWarnings(as.integer(band_count))
  max_band_index <- floor(band_count / 2)
  
  if (!is.finite(band_index) || band_index < 1 || band_index > max_band_index) {
    return("Not selected")
  }
  
  labels <- get_comparison_band_labels(
    band_index = band_index,
    band_count = band_count
  )
  
  paste0(labels$lower_label, " vs ", labels$upper_label)
}


settings_item <- function(label, value, wide = FALSE) {
  value <- if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    "None selected"
  } else {
    paste(value, collapse = ", ")
  }
  
  div(
    class = paste(
      "sb-settings-item",
      if (isTRUE(wide)) "sb-settings-item-wide" else NULL
    ),
    span(class = "sb-settings-label", label),
    span(class = "sb-settings-value", value)
  )
}


settings_summary_ui <- function(settings, run_ok = TRUE) {
  title <- if (isTRUE(run_ok)) {
    "Settings used for the displayed results"
  } else {
    "Settings used for the attempted run"
  }
  
  div(
    class = "sb-settings-summary",
    
    div(class = "sb-settings-summary-title", title),
    
    div(
      class = "sb-settings-grid",
      
      settings_item("Assessment", settings$assessment),
      settings_item("Grade", settings$grade),
      settings_item("Comparison year", settings$comparison_year),
      settings_item("Tracked rank bands", settings$tracked_rank_bands),
      settings_item("Minimum tested students", settings$min_students),
      settings_item("Neutral band", settings$neutral_band),
      
      settings_item("Baseline", settings$baseline, wide = TRUE),
      settings_item("Alt 1", settings$alt1, wide = TRUE),
      settings_item("Alt 2", settings$alt2, wide = TRUE),
      settings_item("Alt 3", settings$alt3, wide = TRUE)
    )
  )
}