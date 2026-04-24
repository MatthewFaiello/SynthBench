# =========================================================
# year_coef.R
# Year-specific coefficient display table + gt table
# Harmonized to visual_helper.R and using explicit feature labels
# =========================================================

# --------------------------- visual style --------------------------- #
PLOT_SETTINGS <- list(
  base_size = 12
)

dde_blue <- "#194a78"
dde_blue_dark <- "#123758"
dde_orange <- "#d98b00"
dde_orange_soft <- "#fff7ea"
dde_bg <- "#f5f7fb"
dde_surface <- "#ffffff"
dde_surface_soft <- "#fbfcfe"
dde_border <- "#d8e2ec"
dde_border_strong <- "#c7d5e2"
dde_text <- "#1f2937"
dde_muted <- "#5b6875"

# --------------------------- label helper --------------------------- #
pretty_feature_label <- function(x) {
  label_map <- c(
    "County_Name.New_Castle" = "New Castle County",
    "County_Name.Sussex" = "Sussex County",
    "na" = "Percent missing scores",
    "units" = "Student units",

    "ELL.ELL" = "ELL",
    "ELL.ELM" = "ELM",
    "ELL.ELX" = "ELX",

    "FosterCare.FOSTR" = "Foster care",
    "Gender.M" = "Male",
    "Geography.W" = "Wilmington",
    "Homeless.HOMLES" = "Homeless",
    "Immersion.IMM" = "Immersion",
    "LowIncome.LOWINC" = "Low income",
    "Migrant.MIGRNT" = "Migrant",
    "MilitaryDep.MILTRY" = "Military connected",

    "RaceReportTitle.African_American" = "African American",
    "RaceReportTitle.American_Indian" = "American Indian",
    "RaceReportTitle.Asian" = "Asian",
    "RaceReportTitle.Hawaiian" = "Hawaiian",
    "RaceReportTitle.Hispanic_Latino" = "Hispanic / Latino",
    "RaceReportTitle.Multi_Racial" = "Multi-racial",

    "SPEDCode.100" = "SPED 100",
    "SPEDCode.200" = "SPED 200",
    "SPEDCode.300" = "SPED 300",
    "SPEDCode.400" = "SPED 400",
    "SPEDCode.500" = "SPED 500",
    "SPEDCode.601" = "SPED 601",
    "SPEDCode.602" = "SPED 602",
    "SPEDCode.700" = "SPED 700",
    "SPEDCode.800" = "SPED 800",
    "SPEDCode.900" = "SPED 900",
    "SPEDCode.1000" = "SPED 1000",
    "SPEDCode.1100" = "SPED 1100",
    "SPEDCode.1200" = "SPED 1200",
    "SPEDCode.1300" = "SPED 1300",
    "SPEDCode.1400" = "SPED 1400"
  )

  dplyr::recode(x, !!!label_map, .default = x)
}

# --------------------------- data prep --------------------------- #
make_year_coef_display_table <- function(run,
                                         year = 2025,
                                         mixed_threshold = 0.20) {

  coef_audit <- run$coef_audit
  coef_col <- "b_std_mean"
  year_term <- paste0("SchoolYear.", year)

  # feature main effects
  main_tbl <- coef_audit %>%
    filter(component == "feature_main") %>%
    transmute(
      model,
      formula_label,
      group_key,
      group_label,
      feature_term = term,
      main_effect = .data[[coef_col]]
    )

  # selected-year interactions
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
      year_interaction = .data[[coef_col]]
    )

  # pure year main effect
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
      year_interaction = coalesce(year_interaction, 0),
      year_specific_coef = main_effect + year_interaction,
      total_abs_year_specific = abs(year_specific_coef),
      direction = case_when(
        year_specific_coef > 0 ~ "positive",
        year_specific_coef < 0 ~ "negative",
        TRUE ~ "neutral"
      ),
      display_label = pretty_feature_label(feature_term)
    ) %>%
    arrange(model, group_label, desc(total_abs_year_specific), feature_term)

  # group-level year-specific summary
  group_table <- term_table %>%
    group_by(model, formula_label, year, group_key, group_label) %>%
    summarise(
      n_terms = n(),
      total_abs_main = sum(abs(main_effect), na.rm = TRUE),
      total_abs_year_interaction = sum(abs(year_interaction), na.rm = TRUE),
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
      direction = case_when(
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
      direction,
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
      direction,
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
      direction,
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

  year_note <- yr$year_main %>%
    mutate(txt = paste0(model, ": ", number(year_main, accuracy = 0.001))) %>%
    pull(txt) %>%
    paste(collapse = " | ")

  gt_data <- yr$display_table %>%
    mutate(
      level_order = if_else(level == "feature_group", 0L, 1L),
      level = if_else(level == "feature_group", "Group", "Feature"),
      direction = case_when(
        direction == "positive" ~ "Positive",
        direction == "negative" ~ "Negative",
        direction == "mixed" ~ "Mixed",
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
      direction,
      pct_of_top
    )

  gt_data %>%
    gt(
      groupname_col = "model",
      rowname_col = "display_label"
    ) %>%
    tab_header(
      title = md(paste0("**Year-specific coefficient influence: ", year, "**")),
      subtitle = md(
        "Grouped benchmark effects and individual feature effects shown together. Larger values indicate more coefficient influence in the selected year."
      )
    ) %>%
    cols_label(
      group_label = "Benchmark group",
      level = "Level",
      total_abs_year_specific = "Year-specific influence",
      direction = "Direction",
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
      columns = c(level, direction)
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
      label = "Year-specific summary",
      columns = c(total_abs_year_specific, direction, pct_of_top)
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
          "computed as the feature main effect plus the selected year interaction when present. ",
          "These feature-level values are on a standardized-coefficient scale. ",
          "Group rows sum the absolute values of those year-specific standardized coefficients, ",
          "so they should be read as relative influence scores rather than as single standardized effects. ",
          "`% of top group` is shown only for group rows. ",
          "Pure `SchoolYear.", year, "` coefficient by model: ",
          year_note,
          "."
        )
      )
    )
}

# make_year_coef_gt(run = test, year = 2025)

