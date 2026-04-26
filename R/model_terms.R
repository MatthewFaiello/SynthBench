# =========================================================
# R/model_terms.R
# Model-term parsing and coefficient metadata helpers
# =========================================================
# Purpose:
#   Parse model-matrix term names into benchmark feature groups
#   and coefficient components used by coefficient diagnostics.
#
# Main helpers:
#   - get_term_group()
#   - parse_coef_term()
#   - build_term_meta()
#   - build_x_term_stats()
#   - make_relative_influence_label()
# =========================================================

get_term_group <- function(term_part) {
  dplyr::case_when(
    grepl("^SchoolYear\\.", term_part) ~ "school_year",
    term_part == "na" ~ "na",
    term_part == "units" ~ "units",
    grepl("^County_Name\\.", term_part) ~ "county",
    grepl("^ELL\\.", term_part) ~ "ell",
    grepl("^FosterCare\\.", term_part) ~ "foster_care",
    grepl("^Gender\\.", term_part) ~ "gender",
    grepl("^Geography\\.", term_part) ~ "geography",
    grepl("^Homeless\\.", term_part) ~ "homeless",
    grepl("^Immersion\\.", term_part) ~ "immersion",
    grepl("^LowIncome\\.", term_part) ~ "low_income",
    grepl("^Migrant\\.", term_part) ~ "migrant",
    grepl("^MilitaryDep\\.", term_part) ~ "military_dep",
    grepl("^RaceReportTitle\\.", term_part) ~ "race",
    grepl("^SPEDCode\\.", term_part) ~ "sped",
    TRUE ~ "other"
  )
}

parse_coef_term <- function(term) {
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  part_groups <- vapply(parts, get_term_group, character(1))
  
  has_year <- any(part_groups == "school_year")
  non_year_groups <- unique(part_groups[part_groups != "school_year"])
  
  if (length(non_year_groups) == 0) {
    group_key <- "school_year"
  } else {
    group_key <- non_year_groups[1]
  }
  
  if (length(parts) == 1 && group_key == "school_year") {
    component <- "year_main"
  } else if (length(parts) == 1) {
    component <- "feature_main"
  } else if (has_year) {
    component <- "year_interaction"
  } else {
    component <- "other_interaction"
  }
  
  tibble(
    term = term,
    group_key = group_key,
    group_label = get_group_label(group_key),
    component = component
  )
}

build_term_meta <- function(term_names) {
  bind_rows(lapply(term_names, parse_coef_term))
}

build_x_term_stats <- function(X, w) {
  tibble(
    term = colnames(X),
    x_w_sd = vapply(seq_len(ncol(X)), function(j) weighted_sd_pop(X[, j], w), numeric(1))
  )
}

make_relative_influence_label <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x >= 0.80 ~ "dominant",
    x >= 0.50 ~ "strong",
    x >= 0.25 ~ "moderate",
    x >= 0.10 ~ "light",
    x > 0 ~ "trace",
    TRUE ~ "none"
  )
}