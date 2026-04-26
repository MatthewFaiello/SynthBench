# =========================================================
# R/comparison_bands.R
# Comparison-band selection helpers
# =========================================================
# Purpose:
#   Define, validate, label, and build selected baseline
#   rank-band lookup tables used in selected-year reporting.
#
# Main helpers:
#   - format_band_pct()
#   - get_comparison_band_labels()
#   - validate_comparison_band()
#   - build_comparison_focus_lookup()
# =========================================================

format_band_pct <- function(x) {
  scales::percent(x, accuracy = 1)
}

get_comparison_band_labels <- function(band_index,
                                       band_count = SETTINGS$comparison_band_count) {
  lower_start <- (band_index - 1) / band_count
  lower_end <- band_index / band_count
  upper_start <- (band_count - band_index) / band_count
  upper_end <- (band_count - band_index + 1) / band_count
  
  list(
    lower_label = paste0(format_band_pct(lower_start), "–", format_band_pct(lower_end)),
    upper_label = paste0(format_band_pct(upper_start), "–", format_band_pct(upper_end))
  )
}

validate_comparison_band <- function(band_index,
                                     band_count = SETTINGS$comparison_band_count) {
  band_index <- suppressWarnings(as.integer(band_index))
  band_count <- suppressWarnings(as.integer(band_count))
  max_band_index <- floor(band_count / 2)
  
  if (!is.finite(band_count) || band_count < 2) {
    stop("comparison_band_count must be at least 2.", call. = FALSE)
  }
  
  if (!is.finite(band_index) ||
      band_index < 1 ||
      band_index > max_band_index) {
    stop(
      paste0(
        "comparison_band_index must be an integer from 1 to ",
        max_band_index,
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

build_comparison_focus_lookup <- function(baseline_rows,
                                          band_index = SETTINGS$comparison_band_index,
                                          band_count = SETTINGS$comparison_band_count) {
  validate_comparison_band(band_index, band_count)
  
  band_index <- as.integer(band_index)
  band_count <- as.integer(band_count)
  lower_band <- band_index
  upper_band <- band_count - band_index + 1
  band_labels <- get_comparison_band_labels(band_index, band_count)
  
  baseline_ranked <- baseline_rows %>%
    arrange(.performance_rank_cv, SchoolName, SchoolCode) %>%
    mutate(
      baseline_order = row_number(),
      baseline_band = dplyr::ntile(baseline_order, band_count),
      baseline_group = case_when(
        baseline_band == lower_band ~ paste0("Baseline band ", band_labels$lower_label),
        baseline_band == upper_band ~ paste0("Baseline band ", band_labels$upper_label),
        TRUE ~ NA_character_
      ),
      baseline_group_order = case_when(
        baseline_band == lower_band ~ 1L,
        baseline_band == upper_band ~ 2L,
        TRUE ~ NA_integer_
      )
    )
  
  focus_lookup <- baseline_ranked %>%
    filter(!is.na(baseline_group)) %>%
    transmute(
      SchoolCode = as.character(SchoolCode),
      baseline_rank = .performance_rank_cv,
      baseline_order,
      baseline_band,
      baseline_group,
      baseline_group_order
    )
  
  if (nrow(focus_lookup) < 2) {
    stop("Not enough baseline schools to create the selected comparison rank bands.", call. = FALSE)
  }
  
  list(
    focus_lookup = focus_lookup,
    band_index = band_index,
    band_count = band_count,
    lower_band = lower_band,
    upper_band = upper_band,
    lower_label = band_labels$lower_label,
    upper_label = band_labels$upper_label,
    lower_n = sum(focus_lookup$baseline_group_order == 1L),
    upper_n = sum(focus_lookup$baseline_group_order == 2L),
    focus_n = nrow(focus_lookup),
    n_baseline_schools = nrow(baseline_ranked)
  )
}