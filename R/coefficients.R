# =========================================================
# R/coefficients.R
# Coefficient standardization and influence-summary helpers
# =========================================================
# Purpose:
#   Build standardized coefficient tables and relative
#   coefficient-weight summaries used by benchmark diagnostics.
#
# Main helpers:
#   - standardize_coef_table()
#   - standardize_coef_audit()
#   - build_coef_influence()
# =========================================================

standardize_coef_table <- function(coef_table_one, x_term_stats, term_meta, y, w) {
  y_w_sd <- weighted_sd_pop(y, w)
  
  coef_table_one %>%
    left_join(x_term_stats, by = "term") %>%
    left_join(term_meta, by = "term") %>%
    mutate(
      b_raw = estimate,
      y_w_sd = y_w_sd,
      b_std = if_else(
        is.finite(x_w_sd) & is.finite(y_w_sd) & y_w_sd > 0,
        b_raw * x_w_sd / y_w_sd,
        NA_real_
      ),
      abs_b_raw = abs(b_raw),
      abs_b_std = abs(b_std)
    ) %>%
    arrange(desc(abs_b_std), desc(abs_b_raw))
}

standardize_coef_audit <- function(model_coef_repeats_one_model, x_term_stats, term_meta, y, w) {
  y_w_sd <- weighted_sd_pop(y, w)
  
  model_coef_repeats_one_model %>%
    group_by(model, formula_label, term) %>%
    summarise(
      n_cv_repeats = n(),
      b_raw_mean = mean(estimate, na.rm = TRUE),
      b_raw_sd = safe_sd(estimate),
      .groups = "drop"
    ) %>%
    left_join(x_term_stats, by = "term") %>%
    left_join(term_meta, by = "term") %>%
    mutate(
      y_w_sd = y_w_sd,
      b_std_mean = if_else(
        is.finite(x_w_sd) & is.finite(y_w_sd) & y_w_sd > 0,
        b_raw_mean * x_w_sd / y_w_sd,
        NA_real_
      ),
      b_std_sd = if_else(
        is.finite(x_w_sd) & is.finite(y_w_sd) & y_w_sd > 0,
        b_raw_sd * x_w_sd / y_w_sd,
        NA_real_
      ),
      abs_b_raw = abs(b_raw_mean),
      abs_b_std = abs(b_std_mean),
      
      b_cv_mean = b_raw_mean,
      b_cv_sd = b_raw_sd
    ) %>%
    arrange(desc(abs_b_std), desc(abs_b_raw))
}

build_coef_influence <- function(coef_audit_tbl) {
  coef_audit_tbl %>%
    group_by(model, formula_label, group_key, group_label) %>%
    summarise(
      n_terms = n(),
      feature_main_abs = sum(abs_b_std[component == "feature_main"], na.rm = TRUE),
      year_main_abs = sum(abs_b_std[component == "year_main"], na.rm = TRUE),
      year_interaction_abs = sum(abs_b_std[component == "year_interaction"], na.rm = TRUE),
      total_abs = sum(abs_b_std, na.rm = TRUE),
      mean_abs_per_term = mean(abs_b_std, na.rm = TRUE),
      signed_net = sum(b_std_mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(model) %>%
    mutate(
      max_total_abs = suppressWarnings(max(total_abs, na.rm = TRUE)),
      sum_total_abs = suppressWarnings(sum(total_abs, na.rm = TRUE)),
      relative_to_top = if_else(
        is.finite(max_total_abs) & max_total_abs > 0,
        total_abs / max_total_abs,
        NA_real_
      ),
      pct_of_top = 100 * relative_to_top,
      share_of_total = if_else(
        is.finite(sum_total_abs) & sum_total_abs > 0,
        total_abs / sum_total_abs,
        NA_real_
      ),
      influence_label = make_relative_influence_label(relative_to_top),
      pattern_label = case_when(
        year_interaction_abs > (feature_main_abs + year_main_abs) * 1.25 ~ "mostly year-varying",
        (feature_main_abs + year_main_abs) > year_interaction_abs * 1.25 ~ "mostly direct",
        TRUE ~ "mixed direct + year-varying"
      )
    ) %>%
    ungroup() %>%
    select(-max_total_abs, -sum_total_abs) %>%
    arrange(model, desc(total_abs), group_label)
}