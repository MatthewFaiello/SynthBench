# =========================================================
# visual_helper.R — benchmark comparison visuals
# =========================================================
# Purpose:
#   Turn model_comp_plot/model_comp_plot_all into the main comparison plots and
#   summary tables used to review how school rankings and
#   benchmark results change across alternative benchmark
#   model definitions.
#
# Depends on:
#   - R/visual_theme.R for PLOT_SETTINGS, dde_* constants,
#     theme_modelcomp(), and empty_plot()
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(gt)
library(stringr)
library(grid)
library(ggtext)


# --------------------------- 1) data prep helpers --------------------------- #

baseline_caption <- function(viz) {
  viz$plot_dat %>%
    filter(model == viz$baseline_model) %>%
    distinct(formula_label) %>%
    slice(1) %>%
    pull(formula_label)
}

prepare_model_comp_wide <- function(model_comp_plot) {
  
  required_cols <- c(
    "model",
    "formula_label",
    "SchoolYear",
    "DistrictName",
    "SchoolName",
    "SchoolCode",
    "ModelGrade",
    "ScaleScore.mean",
    "n",
    "metric",
    "value"
  )
  
  missing_cols <- setdiff(required_cols, names(model_comp_plot))
  
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "model_comp_plot is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  model_comp_plot %>%
    mutate(
      model = as.character(model),
      SchoolYear = as.character(SchoolYear),
      SchoolName = as.character(SchoolName),
      SchoolCode = as.character(SchoolCode)
    ) %>%
    pivot_wider(
      id_cols = c(
        model,
        formula_label,
        SchoolYear,
        DistrictName,
        SchoolName,
        SchoolCode,
        ModelGrade,
        ScaleScore.mean,
        n
      ),
      names_from = metric,
      values_from = value
    )
}


prepare_baseline_reference <- function(plot_dat_wide,
                                       baseline_model,
                                       comparison_focus_lookup = NULL,
                                       comparison_lower_label = NULL,
                                       comparison_upper_label = NULL) {
  
  if (!baseline_model %in% unique(plot_dat_wide$model)) {
    stop("baseline_model was not found in model_comp_plot.", call. = FALSE)
  }
  
  baseline_rows <- plot_dat_wide %>%
    filter(model == baseline_model) %>%
    mutate(SchoolCode = as.character(SchoolCode)) %>%
    arrange(.performance_rank_cv, SchoolName, SchoolCode)
  
  if (nrow(baseline_rows) < 2) {
    stop("Not enough baseline rows to build comparison visuals.", call. = FALSE)
  }
  
  if (!is.null(comparison_focus_lookup)) {
    comparison_focus_lookup <- comparison_focus_lookup %>%
      mutate(SchoolCode = as.character(SchoolCode))
    
    lookup_required <- c(
      "SchoolCode",
      "baseline_rank",
      "baseline_group",
      "baseline_group_order"
    )
    missing_lookup_cols <- setdiff(lookup_required, names(comparison_focus_lookup))
    
    if (length(missing_lookup_cols) > 0) {
      stop(
        paste0(
          "comparison_focus_lookup is missing required columns: ",
          paste(missing_lookup_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    
    baseline_reference <- baseline_rows %>%
      select(SchoolCode) %>%
      left_join(
        comparison_focus_lookup %>%
          select(
            SchoolCode,
            baseline_rank,
            baseline_group,
            baseline_group_order
          ),
        by = "SchoolCode"
      ) %>%
      filter(!is.na(baseline_group)) %>%
      arrange(baseline_group_order, baseline_rank)
  } else {
    split_n <- floor(nrow(baseline_rows) / 2)
    
    if (split_n < 1) {
      stop("Could not create paired baseline rank bands.", call. = FALSE)
    }
    
    lower_label <- if (is.null(comparison_lower_label)) {
      "selected higher baseline band"
    } else {
      comparison_lower_label
    }
    
    upper_label <- if (is.null(comparison_upper_label)) {
      "selected lower baseline band"
    } else {
      comparison_upper_label
    }
    
    baseline_reference <- baseline_rows %>%
      mutate(
        baseline_rank = .performance_rank_cv,
        baseline_group = case_when(
          row_number() <= split_n ~ paste0("Baseline band ", lower_label),
          TRUE ~ paste0("Baseline band ", upper_label)
        ),
        baseline_group_order = case_when(
          row_number() <= split_n ~ 1L,
          TRUE ~ 2L
        )
      ) %>%
      select(SchoolCode, baseline_rank, baseline_group, baseline_group_order)
  }
  
  if (nrow(baseline_reference) < 2) {
    stop("Not enough tracked schools to build comparison visuals.", call. = FALSE)
  }
  
  group_counts <- baseline_reference %>%
    distinct(SchoolCode, baseline_group, baseline_group_order) %>%
    count(baseline_group, baseline_group_order, name = "n_schools") %>%
    arrange(baseline_group_order)
  
  top_label <- group_counts$baseline_group[1]
  bottom_label <- group_counts$baseline_group[min(2, nrow(group_counts))]
  lower_n <- group_counts$n_schools[1]
  upper_n <- group_counts$n_schools[min(2, nrow(group_counts))]
  
  tracked_school_note <- paste0(
    "Tracked schools only: ",
    top_label,
    " (", lower_n, " schools) and ",
    bottom_label,
    " (", upper_n, " schools)."
  )
  
  baseline_rows <- baseline_rows %>%
    inner_join(
      baseline_reference %>% select(SchoolCode, baseline_rank, baseline_group, baseline_group_order),
      by = "SchoolCode"
    ) %>%
    arrange(baseline_group_order, baseline_rank)
  
  list(
    baseline_rows = baseline_rows,
    baseline_reference = baseline_reference,
    top_n = max(lower_n, upper_n),
    lower_n = lower_n,
    upper_n = upper_n,
    top_label = top_label,
    bottom_label = bottom_label,
    tracked_school_note = tracked_school_note
  )
}


make_school_label_order <- function(baseline_rows, baseline_reference) {
  baseline_rows %>%
    arrange(baseline_group_order, baseline_rank) %>%
    transmute(
      school_label = paste0(
        "#",
        round(baseline_rank),
        ": ",
        str_wrap(SchoolName, width = 24)
      )
    ) %>%
    pull(school_label)
}


add_baseline_tracking <- function(plot_dat_wide, baseline_info) {
  
  school_label_order <- make_school_label_order(
    baseline_rows = baseline_info$baseline_rows,
    baseline_reference = baseline_info$baseline_reference
  )
  
  plot_dat <- plot_dat_wide %>%
    left_join(
      baseline_info$baseline_reference %>%
        select(SchoolCode, baseline_rank, baseline_group, baseline_group_order),
      by = "SchoolCode"
    ) %>%
    mutate(
      model = factor(model, levels = unique(plot_dat_wide$model)),
      baseline_group = factor(
        baseline_group,
        levels = c(baseline_info$top_label, baseline_info$bottom_label)
      ),
      rank_change = .performance_rank_cv - baseline_rank,
      abs_rank_change = abs(rank_change),
      rank_arrow = case_when(
        rank_change < 0 ~ "↑",
        rank_change > 0 ~ "↓",
        TRUE ~ "→"
      ),
      rank_label = paste0(rank_arrow, "\n", round(.performance_rank_cv)),
      school_label = paste0(
        "#",
        round(baseline_rank),
        ": ",
        str_wrap(SchoolName, width = 24)
      ),
      school_label = factor(school_label, levels = school_label_order)
    )
  
  list(
    plot_dat = plot_dat,
    school_label_order = school_label_order
  )
}


make_baseline_group_colors <- function(top_label, bottom_label) {
  c(
    setNames(dde_blue, top_label),
    setNames(dde_orange, bottom_label)
  )
}


add_all_school_baseline_tracking <- function(plot_dat_wide_all, baseline_model) {
  if (!baseline_model %in% unique(plot_dat_wide_all$model)) {
    stop("baseline_model was not found in model_comp_plot_all.", call. = FALSE)
  }
  
  baseline_all <- plot_dat_wide_all %>%
    filter(model == baseline_model) %>%
    transmute(
      SchoolCode = as.character(SchoolCode),
      baseline_rank_all = .performance_rank_cv
    )
  
  plot_dat_wide_all %>%
    left_join(baseline_all, by = "SchoolCode") %>%
    mutate(
      model = factor(model, levels = unique(plot_dat_wide_all$model)),
      rank_change_all = .performance_rank_cv - baseline_rank_all,
      abs_rank_change_all = abs(rank_change_all)
    )
}


# --------------------------- 4) build visual data --------------------------- #

build_visual_data <- function(model_comp_plot,
                              model_comp_plot_all = model_comp_plot,
                              baseline_model,
                              score_all = NULL,
                              comparison_focus_lookup = NULL,
                              comparison_lower_label = NULL,
                              comparison_upper_label = NULL) {
  
  # Step 1: prepare both visual scopes
  # - plot_dat_wide is the tracked top/bottom baseline-school sample.
  # - plot_dat_wide_all is the full selected-year sample.
  plot_dat_wide <- prepare_model_comp_wide(model_comp_plot)
  plot_dat_wide_all <- prepare_model_comp_wide(model_comp_plot_all)
  
  # Step 2: identify the selected paired baseline rank bands
  baseline_info <- prepare_baseline_reference(
    plot_dat_wide = plot_dat_wide,
    baseline_model = baseline_model,
    comparison_focus_lookup = comparison_focus_lookup,
    comparison_lower_label = comparison_lower_label,
    comparison_upper_label = comparison_upper_label
  )
  
  # Step 3: attach baseline rank and baseline group labels
  tracking_info <- add_baseline_tracking(
    plot_dat_wide = plot_dat_wide,
    baseline_info = baseline_info
  )
  
  plot_dat <- tracking_info$plot_dat
  school_label_order <- tracking_info$school_label_order
  
  plot_dat_all <- add_all_school_baseline_tracking(
    plot_dat_wide_all = plot_dat_wide_all,
    baseline_model = baseline_model
  )
  
  all_school_note <- paste0(
    "All schools in the selected year: ",
    n_distinct(plot_dat_all$SchoolCode),
    " schools."
  )
  
  top_n <- baseline_info$top_n
  top_label <- baseline_info$top_label
  bottom_label <- baseline_info$bottom_label
  tracked_school_note <- baseline_info$tracked_school_note
  
  comparison_models <- setdiff(unique(as.character(plot_dat$model)), baseline_model)
  
  baseline_group_colors <- make_baseline_group_colors(
    top_label = top_label,
    bottom_label = bottom_label
  )
  
  
  # Step 4: summarize rank movement across alternative models
  # This aggregate diagnostic intentionally uses the full selected-year sample,
  # not just the tracked top/bottom baseline schools.
  model_sensitivity_summary <- plot_dat_all %>%
    filter(model != baseline_model) %>%
    group_by(model) %>%
    summarise(
      n_schools = n_distinct(SchoolCode),
      mean_abs_rank_change = mean(abs_rank_change_all, na.rm = TRUE),
      median_abs_rank_change = median(abs_rank_change_all, na.rm = TRUE),
      p90_abs_rank_change = as.numeric(
        quantile(abs_rank_change_all, 0.90, na.rm = TRUE)
      ),
      max_abs_rank_change = max(abs_rank_change_all, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_abs_rank_change))
  
  headline_takeaway <- if (nrow(model_sensitivity_summary) > 0) {
    paste0(
      as.character(model_sensitivity_summary$model[1]),
      " shows the largest average rank movement across all schools in the selected year (",
      number(model_sensitivity_summary$mean_abs_rank_change[1], accuracy = 0.1),
      " ranks across ",
      model_sensitivity_summary$n_schools[1],
      " schools)."
    )
  } else {
    "No comparison models were found."
  }
  
  
  # Step 5: build dumbbell-plot data
  dumbbell_dat <- plot_dat %>%
    filter(model != baseline_model) %>%
    transmute(
      SchoolCode,
      SchoolName,
      school_label,
      baseline_group,
      comparison_model = factor(as.character(model), levels = comparison_models),
      baseline_rank,
      comparison_rank = .performance_rank_cv
    )
  
  if (nrow(dumbbell_dat) > 0) {
    max_rank <- max(
      c(dumbbell_dat$baseline_rank, dumbbell_dat$comparison_rank),
      na.rm = TRUE
    )
    
    x_breaks <- sort(unique(c(
      1,
      pretty(c(dumbbell_dat$baseline_rank, dumbbell_dat$comparison_rank), n = 6)
    )))
    
    x_breaks <- x_breaks[x_breaks >= 1 & x_breaks <= ceiling(max_rank)]
  } else {
    x_breaks <- pretty(plot_dat$baseline_rank, n = 6)
    x_breaks <- x_breaks[x_breaks >= 1]
  }
  
  
  # Step 6: build metric-facet data
  metric_long <- plot_dat %>%
    select(
      SchoolCode,
      SchoolName,
      school_label,
      baseline_group,
      model,
      .performance_rank_cv,
      .performance_z_cv,
      .resid_cv,
      .pred_cv
    ) %>%
    pivot_longer(
      cols = c(.performance_rank_cv, .performance_z_cv, .resid_cv, .pred_cv),
      names_to = "metric_name",
      values_to = "metric_value"
    ) %>%
    mutate(
      metric_name = recode(
        metric_name,
        .performance_rank_cv = "Within-year rank",
        .performance_z_cv = "Scaled benchmark gap",
        .resid_cv = "Benchmark gap",
        .pred_cv = "Benchmark"
      )
    )
  
  
  # Step 7: build rank summary table
  rank_summary <- plot_dat %>%
    group_by(
      SchoolCode,
      SchoolName,
      baseline_rank,
      baseline_group,
      ScaleScore.mean,
      n
    ) %>%
    summarise(
      best_rank = min(.performance_rank_cv, na.rm = TRUE),
      worst_rank = max(.performance_rank_cv, na.rm = TRUE),
      largest_rank_shift = max(abs(.performance_rank_cv - baseline_rank), na.rm = TRUE),
      largest_benchmark_score_shift = max(.pred_cv, na.rm = TRUE) - min(.pred_cv, na.rm = TRUE),
      largest_scaled_benchmark_gap_shift = max(.performance_z_cv, na.rm = TRUE) - min(.performance_z_cv, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(baseline_group, baseline_rank) %>%
    mutate(
      `Baseline group` = as.character(baseline_group),
      `Baseline rank` = round(baseline_rank),
      School = SchoolName,
      `Mean observed score` = ScaleScore.mean,
      `Best rank` = round(best_rank),
      `Worst rank` = round(worst_rank),
      `Largest rank shift` = round(largest_rank_shift, 1),
      `Largest benchmark-score shift` = round(largest_benchmark_score_shift, 1),
      `Largest scaled benchmark-gap shift` = round(largest_scaled_benchmark_gap_shift, 2)
    ) %>%
    select(
      `Baseline group`,
      `Baseline rank`,
      School,
      n,
      `Mean observed score`,
      `Best rank`,
      `Worst rank`,
      `Largest rank shift`,
      `Largest benchmark-score shift`,
      `Largest scaled benchmark-gap shift`
    )
  
  
  # Step 8: optionally add split-stability diagnostics
  # This aggregate diagnostic intentionally uses the full selected-year sample,
  # not just the tracked top/bottom baseline schools.
  #
  # The plotted stability tab intentionally shows only SD-style diagnostics:
  #   - benchmark-score SD across repeated CV splits
  #   - rank SD across repeated CV splits
  # Benchmark-label consistency is retained in score_all for deeper review,
  # but is not plotted in the main Split stability tab.
  stability_plot_cols <- c(
    ".pred_cv_sd",
    ".performance_rank_cv_sd"
  )
  
  stability_optional_cols <- c(
    ".performance_direction_mode",
    ".performance_direction_consistency",
    "n_cv_repeats_used"
  )
  
  stability_source <- NULL
  
  if (!is.null(score_all)) {
    score_all_required <- c(
      "model",
      "SchoolYear",
      "SchoolCode",
      stability_plot_cols
    )
    
    if (all(score_all_required %in% names(score_all))) {
      comparison_years <- unique(plot_dat_wide_all$SchoolYear)
      comparison_schools <- unique(plot_dat_wide_all$SchoolCode)
      comparison_models_all <- unique(as.character(plot_dat_wide_all$model))
      stability_keep_cols <- intersect(
        c(stability_plot_cols, stability_optional_cols),
        names(score_all)
      )
      
      stability_source <- score_all %>%
        mutate(
          model = as.character(model),
          SchoolYear = as.character(SchoolYear),
          SchoolCode = as.character(SchoolCode)
        ) %>%
        filter(
          SchoolYear %in% comparison_years,
          SchoolCode %in% comparison_schools,
          model %in% comparison_models_all
        ) %>%
        select(
          model,
          SchoolYear,
          SchoolCode,
          all_of(stability_keep_cols)
        ) %>%
        left_join(
          plot_dat_wide_all %>%
            distinct(
              model,
              SchoolYear,
              SchoolCode,
              SchoolName
            ) %>%
            mutate(
              model = as.character(model),
              SchoolYear = as.character(SchoolYear),
              SchoolCode = as.character(SchoolCode)
            ),
          by = c("model", "SchoolYear", "SchoolCode")
        )
    }
  }
  
  has_stability_data <- !is.null(stability_source) &&
    all(stability_plot_cols %in% names(stability_source)) &&
    nrow(stability_source) > 0
  
  if (has_stability_data) {
    stability_long <- stability_source %>%
      select(
        SchoolCode,
        SchoolName,
        model,
        all_of(stability_plot_cols),
        any_of(stability_optional_cols)
      ) %>%
      pivot_longer(
        cols = all_of(stability_plot_cols),
        names_to = "stability_metric",
        values_to = "stability_value"
      ) %>%
      mutate(
        stability_metric = recode(
          stability_metric,
          .pred_cv_sd = "Benchmark-score SD across CV repeats",
          .performance_rank_cv_sd = "Rank SD across CV repeats"
        )
      )
    
    stability_summary <- stability_long %>%
      group_by(model, stability_metric) %>%
      summarise(
        n_schools = n_distinct(SchoolCode),
        mean_value = mean(stability_value, na.rm = TRUE),
        median_value = median(stability_value, na.rm = TRUE),
        p90_value = as.numeric(quantile(stability_value, 0.90, na.rm = TRUE)),
        max_value = max(stability_value, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    stability_long <- tibble()
    stability_summary <- tibble()
  }
  # Step 9: return all visual inputs
  list(
    plot_dat = plot_dat,
    plot_dat_all = plot_dat_all,
    baseline_model = baseline_model,
    baseline_group_colors = baseline_group_colors,
    model_sensitivity_summary = model_sensitivity_summary,
    headline_takeaway = headline_takeaway,
    dumbbell_dat = dumbbell_dat,
    x_breaks = x_breaks,
    metric_long = metric_long,
    rank_summary = rank_summary,
    top_n = top_n,
    lower_n = baseline_info$lower_n,
    upper_n = baseline_info$upper_n,
    tracked_school_note = tracked_school_note,
    all_school_note = all_school_note,
    has_stability_data = has_stability_data,
    stability_long = stability_long,
    stability_summary = stability_summary
  )
}


# --------------------------- 5) outputs --------------------------- #

p_rank_heat <- function(viz) {
  ggplot(viz$plot_dat, aes(x = model, y = fct_rev(school_label))) +
    geom_tile(fill = dde_surface_soft, color = dde_border, linewidth = 0.6) +
    geom_text(
      aes(label = rank_label),
      size = 3.4,
      fontface = "bold",
      lineheight = 0.9,
      color = dde_text
    ) +
    facet_grid(baseline_group ~ ., scales = "free_y", space = "free_y") +
    labs(
      title = "How selected baseline rank-band schools shift across benchmark definitions",
      subtitle = paste(
        "↑ better rank, ↓ worse rank, → unchanged. Smaller rank numbers are better.",
        viz$tracked_school_note
      ),
      x = NULL,
      y = NULL,
      caption = paste0("Baseline = ", baseline_caption(viz))
    ) +
    theme_modelcomp() +
    theme(
      panel.spacing.y = unit(1.05, "lines")
    )
}


p_dumbbell_all <- function(viz) {
  if (nrow(viz$dumbbell_dat) == 0) {
    return(empty_plot("No comparison models available."))
  }
  
  ggplot(viz$dumbbell_dat, aes(y = fct_rev(school_label))) +
    geom_segment(
      aes(
        x = baseline_rank,
        xend = comparison_rank,
        yend = fct_rev(school_label),
        color = baseline_group
      ),
      linewidth = 0.9,
      alpha = 0.85
    ) +
    geom_point(
      aes(x = baseline_rank),
      size = 2.3,
      shape = 21,
      fill = dde_surface,
      color = dde_blue_dark,
      stroke = 0.9
    ) +
    geom_point(
      aes(x = comparison_rank, color = baseline_group),
      size = 2.4
    ) +
    scale_color_manual(values = viz$baseline_group_colors) +
    scale_x_reverse(breaks = viz$x_breaks) +
    facet_grid(baseline_group ~ comparison_model, scales = "free_y", space = "free_y") +
    labs(
      title = "How far selected baseline rank-band schools move under alternative models",
      subtitle = paste(
        "Open circle = baseline rank. Filled circle = comparison-model rank. Smaller rank numbers are better.",
        viz$tracked_school_note
      ),
      x = "\nWithin-year rank relative to other schools",
      y = NULL,
      caption = paste0("Baseline = ", baseline_caption(viz))
    ) +
    theme_modelcomp() +
    theme(
      legend.position = "none",
      panel.spacing = unit(0.95, "lines")
    )
}


p_rank_shift_summary <- function(viz) {
  if (nrow(viz$model_sensitivity_summary) == 0) {
    return(empty_plot("No comparison models available."))
  }
  
  ggplot(viz$model_sensitivity_summary, aes(x = model, y = mean_abs_rank_change)) +
    geom_col(fill = dde_blue, width = 0.7) +
    geom_text(
      aes(label = number(mean_abs_rank_change, accuracy = 0.1)),
      vjust = -0.35,
      color = dde_blue_dark,
      fontface = "bold",
      size = 3.8
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Average absolute rank change across all schools",
      subtitle = paste(
        viz$headline_takeaway,
        "Calculated across the full selected-year sample, not just the tracked top and bottom baseline schools."
      ),
      x = NULL,
      y = "Average |rank change|",
      caption = paste0("Baseline = ", baseline_caption(viz))
    ) +
    theme_modelcomp()
}


p_metric_facets <- function(viz) {
  ggplot(
    viz$metric_long,
    aes(
      x = model,
      y = metric_value,
      group = SchoolCode,
      color = baseline_group
    )
  ) +
    geom_line(linewidth = 0.7, alpha = 0.65) +
    geom_point(size = 1.8) +
    scale_color_manual(values = viz$baseline_group_colors) +
    facet_wrap(~ metric_name, scales = "free_y", ncol = 2) +
    labs(
      title = "How tracked baseline-school results change across models",
      subtitle = paste(
        "The same selected baseline rank-band schools are followed across the baseline and alternative models.",
        viz$tracked_school_note
      ),
      x = NULL,
      y = NULL,
      color = NULL,
      caption = paste0("Baseline = ", baseline_caption(viz))
    ) +
    theme_modelcomp()
}


p_stability_summary <- function(viz) {
  if (!isTRUE(viz$has_stability_data) || nrow(viz$stability_summary) == 0) {
    return(
      empty_plot(
        "No split-stability data were supplied. Pass score_all to build_visual_data() to enable this plot."
      )
    )
  }
  
  plot_dat <- viz$stability_summary %>%
    mutate(
      model = factor(model, levels = unique(as.character(viz$plot_dat_all$model)))
    )
  
  ggplot(plot_dat, aes(x = model, y = mean_value)) +
    geom_col(fill = dde_blue, width = 0.7) +
    geom_text(
      aes(label = number(mean_value, accuracy = 0.01)),
      vjust = -0.35,
      color = dde_blue_dark,
      fontface = "bold",
      size = 3.4
    ) +
    facet_wrap(~ stability_metric, scales = "free_y", ncol = 1) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Split-stability diagnostics across all schools",
      subtitle = paste(
        "These summaries describe fold-assignment stability across repeated grouped CV for the full selected-year sample.",
        "They do not describe movement caused by changing the benchmark definition."
      ),
      x = NULL,
      y = NULL,
      caption = paste0("Baseline = ", baseline_caption(viz))
    ) +
    theme_modelcomp()
}


tbl_rank_summary <- function(viz) {
  viz$rank_summary %>%
    gt(groupname_col = "Baseline group") %>%
    tab_header(
      title = md("**Tracked-school sensitivity to benchmark definition**"),
      subtitle = md(
        paste0(
          viz$tracked_school_note,
          " Tracked across the comparison models."
        )
      )
    ) %>%
    cols_label(n = "Tested n") %>%
    fmt_number(
      columns = c(
        `Mean observed score`,
        `Largest rank shift`,
        `Largest benchmark-score shift`
      ),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(`Largest scaled benchmark-gap shift`),
      decimals = 2
    ) %>%
    cols_align(
      align = "left",
      columns = c(School)
    ) %>%
    cols_align(
      align = "center",
      columns = c(`Baseline rank`, `Best rank`, `Worst rank`)
    ) %>%
    cols_align(
      align = "right",
      columns = c(
        n,
        `Mean observed score`,
        `Largest rank shift`,
        `Largest benchmark-score shift`,
        `Largest scaled benchmark-gap shift`
      )
    ) %>%
    tab_spanner(
      label = "Baseline",
      columns = c(`Baseline rank`, School, n, `Mean observed score`)
    ) %>%
    tab_spanner(
      label = "Sensitivity across benchmark definitions",
      columns = c(
        `Best rank`,
        `Worst rank`,
        `Largest rank shift`,
        `Largest benchmark-score shift`,
        `Largest scaled benchmark-gap shift`
      )
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
    opt_row_striping() %>%
    tab_options(
      table.background.color = dde_bg,
      heading.background.color = dde_surface,
      row.striping.background_color = dde_surface_soft,
      table.border.top.color = dde_border_strong,
      table.border.bottom.color = dde_border_strong,
      column_labels.border.top.color = dde_border_strong,
      column_labels.border.bottom.color = dde_border_strong,
      table.font.color = dde_text,
      source_notes.font.size = px(11),
      data_row.padding = px(6)
    ) %>%
    tab_source_note(
      source_note = md(
        paste0(
          "**Note.** Baseline = `",
          baseline_caption(viz),
          "`. Smaller rank numbers are better. `n` is the number of tested students contributing to the observed school mean. The summary covers only the selected paired baseline rank-band schools."
        )
      )
    )
}
