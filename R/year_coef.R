# =========================================================
# year_coef.R
# Year-specific benchmark feature weight table + gt table
# =========================================================
# Depends on:
#   - R/visual_theme.R for dde_* style constants
# =========================================================

# --------------------------- data prep --------------------------- #
make_year_coef_display_table <- function(run,
                                         year = 2025,
                                         mixed_threshold = 0.20) {
  
  coef_audit <- run$coef_audit
  coef_col <- "b_std_mean"
  year_term <- paste0("SchoolYear.", year)
  
  # feature main coefficients
  main_tbl <- coef_audit %>%
    filter(component == "feature_main") %>%
    transmute(
      model,
      formula_label,
      group_key,
      group_label,
      feature_term = term,
      main_coef = .data[[coef_col]]
    )
  
  # selected-year interaction coefficients
  interaction_tbl <- coef_audit %>%
    filter(component == "year_interaction") %>%
    mutate(
      feature_term = case_when(
        str_starts(term, paste0(year_term, ":")) ~ str_remove(term, paste0("^", year_term, ":")),
        str_ends(term, paste0(":", year_term)) ~ str_remove(term, paste0(":", year_term, "$")),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(feature_term)) %>%
    transmute(
      model,
      formula_label,
      feature_term,
      year_interaction_coef = .data[[coef_col]]
    )
  
  # pure year main coefficient
  year_main_tbl <- coef_audit %>%
    filter(term == year_term) %>%
    transmute(
      model,
      formula_label,
      year_main = .data[[coef_col]]
    )
  
  # feature-level year-specific coefficients
  term_table <- main_tbl %>%
    left_join(
      interaction_tbl,
      by = c("model", "formula_label", "feature_term")
    ) %>%
    mutate(
      year = year,
      year_interaction_coef = coalesce(year_interaction_coef, 0),
      year_specific_coef = main_coef + year_interaction_coef,
      total_abs_year_specific = abs(year_specific_coef),
      coef_sign = case_when(
        year_specific_coef > 0 ~ "positive",
        year_specific_coef < 0 ~ "negative",
        TRUE ~ "neutral"
      ),
      display_label = pretty_feature_label(feature_term)
    ) %>%
    arrange(model, group_label, desc(total_abs_year_specific), feature_term)
  
  # group-level year-specific coefficient-weight summary
  group_table <- term_table %>%
    group_by(model, formula_label, year, group_key, group_label) %>%
    summarise(
      n_terms = n(),
      total_abs_main = sum(abs(main_coef), na.rm = TRUE),
      total_abs_year_interaction = sum(abs(year_interaction_coef), na.rm = TRUE),
      total_abs_year_specific = sum(total_abs_year_specific, na.rm = TRUE),
      mean_abs_year_specific = mean(total_abs_year_specific, na.rm = TRUE),
      signed_net_year_specific = sum(year_specific_coef, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      net_share = ifelse(
        total_abs_year_specific > 0,
        abs(signed_net_year_specific) / total_abs_year_specific,
        0
      ),
      coef_sign = case_when(
        total_abs_year_specific == 0 ~ "neutral",
        net_share < mixed_threshold ~ "mixed",
        signed_net_year_specific > 0 ~ "positive",
        signed_net_year_specific < 0 ~ "negative",
        TRUE ~ "neutral"
      ),
      display_label = group_label
    ) %>%
    group_by(model) %>%
    mutate(
      pct_of_top = 100 * total_abs_year_specific / max(total_abs_year_specific, na.rm = TRUE),
      pct_of_top = ifelse(is.finite(pct_of_top), pct_of_top, NA_real_),
      group_rank = min_rank(desc(total_abs_year_specific))
    ) %>%
    ungroup() %>%
    arrange(model, desc(total_abs_year_specific), group_label)
  
  # one display-ready table
  group_display <- group_table %>%
    transmute(
      model,
      formula_label,
      year,
      level = "feature_group",
      group_key,
      group_label,
      display_label,
      n_terms,
      total_abs_year_specific,
      coef_sign,
      pct_of_top,
      sort_group = group_rank,
      sort_within_group = 0
    )
  
  term_display <- term_table %>%
    left_join(
      group_table %>%
        select(model, formula_label, group_key, group_rank),
      by = c("model", "formula_label", "group_key")
    ) %>%
    group_by(model, formula_label, group_key) %>%
    arrange(desc(total_abs_year_specific), feature_term, .by_group = TRUE) %>%
    mutate(sort_within_group = row_number()) %>%
    ungroup() %>%
    transmute(
      model,
      formula_label,
      year,
      level = "feature",
      group_key,
      group_label,
      display_label,
      n_terms = 1L,
      total_abs_year_specific,
      coef_sign,
      pct_of_top = NA_real_,
      sort_group = group_rank,
      sort_within_group
    )
  
  display_table <- bind_rows(group_display, term_display) %>%
    arrange(model, sort_group, level, sort_within_group, desc(total_abs_year_specific), display_label) %>%
    select(
      model,
      formula_label,
      year,
      level,
      group_label,
      display_label,
      total_abs_year_specific,
      coef_sign,
      pct_of_top,
      n_terms
    )
  
  list(
    term_table = term_table,
    group_table = group_table,
    year_main = year_main_tbl,
    display_table = display_table
  )
}

# --------------------------- gt output --------------------------- #
make_year_coef_gt <- function(run,
                              year = 2025,
                              mixed_threshold = 0.20) {
  
  yr <- make_year_coef_display_table(
    run = run,
    year = year,
    mixed_threshold = mixed_threshold
  )
  
  year_note <- if (nrow(yr$year_main) > 0) {
    yr$year_main %>%
      mutate(txt = paste0(model, ": ", number(year_main, accuracy = 0.001))) %>%
      pull(txt) %>%
      paste(collapse = " | ")
  } else {
    "not available"
  }
  
  gt_data <- yr$display_table %>%
    mutate(
      level_order = if_else(level == "feature_group", 0L, 1L),
      level = if_else(level == "feature_group", "Group", "Feature"),
      coef_sign = case_when(
        coef_sign == "positive" ~ "Positive",
        coef_sign == "negative" ~ "Negative",
        coef_sign == "mixed" ~ "Mixed",
        TRUE ~ "Neutral"
      ),
      display_label = if_else(
        level == "Feature",
        paste0("• ", display_label),
        display_label
      ),
      pct_of_top = if_else(level == "Feature", NA_real_, pct_of_top)
    ) %>%
    arrange(model, group_label, level_order, desc(total_abs_year_specific), display_label) %>%
    select(
      model,
      group_label,
      display_label,
      level,
      total_abs_year_specific,
      coef_sign,
      pct_of_top
    )
  
  gt_data %>%
    gt(
      groupname_col = "model",
      rowname_col = "display_label"
    ) %>%
    tab_header(
      title = md(paste0("**Year-specific benchmark feature weights: ", year, "**")),
      subtitle = md(
        paste(
          "Grouped benchmark features and individual feature coefficients are shown together.",
          "Larger values indicate larger relative standardized coefficient weight in the selected year.",
          "These are model diagnostics, not causal effects."
        )
      )
    ) %>%
    cols_label(
      group_label = "Benchmark group",
      level = "Level",
      total_abs_year_specific = "Relative weight",
      coef_sign = "Sign",
      pct_of_top = "% of top group"
    ) %>%
    fmt_number(
      columns = c(total_abs_year_specific, pct_of_top),
      decimals = 2
    ) %>%
    sub_missing(
      columns = pct_of_top,
      missing_text = ""
    ) %>%
    cols_align(
      align = "left",
      columns = c(group_label)
    ) %>%
    cols_align(
      align = "center",
      columns = c(level, coef_sign)
    ) %>%
    cols_align(
      align = "right",
      columns = c(total_abs_year_specific, pct_of_top)
    ) %>%
    tab_spanner(
      label = "Structure",
      columns = c(group_label, level)
    ) %>%
    tab_spanner(
      label = "Year-specific coefficient summary",
      columns = c(total_abs_year_specific, coef_sign, pct_of_top)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = dde_blue),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = dde_blue_dark),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = dde_blue_dark),
      locations = cells_body(rows = level == "Group")
    ) %>%
    opt_row_striping() %>%
    tab_options(
      table.background.color = dde_bg,
      heading.background.color = dde_surface,
      row.striping.background_color = dde_surface_soft,
      table.border.top.color = dde_border_strong,
      table.border.bottom.color = dde_border_strong,
      column_labels.border.top.color = dde_border_strong,
      column_labels.border.bottom.color = dde_border_strong,
      heading.border.bottom.color = dde_border_strong,
      row_group.border.top.color = dde_border_strong,
      row_group.border.bottom.color = dde_border,
      table.font.color = dde_text,
      source_notes.font.size = px(11),
      data_row.padding = px(6)
    ) %>%
    tab_source_note(
      source_note = md(
        paste0(
          "**Note.** Feature rows use the selected year's standardized coefficient, ",
          "computed as the feature main coefficient plus the selected-year interaction coefficient when present. ",
          "Group rows sum the absolute values of those year-specific standardized coefficients, ",
          "so they should be read as relative coefficient-weight scores, not as causal effects or stand-alone importance measures. ",
          "`% of top group` is shown only for group rows. ",
          "Pure `SchoolYear.", year, "` coefficient by model: ",
          year_note,
          "."
        )
      )
    )
}

# make_year_coef_gt(run = test, year = 2025)