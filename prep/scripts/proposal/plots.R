# =========================================================
# Synthetic benchmark model comparison plots — report-ready
#
# What this version does:
#   1) Leads with the rank heatmap for the headline story
#   2) Keeps the dumbbell plot as the supporting proof figure
#   3) Treats metric facets as a backup / appendix figure
#   4) Adds a compact benchmark-sensitivity summary for fast takeaways
#   5) Applies the DDE theme consistently
#
# Outputs:
#   - plot_01_rank_heatmap_report.png
#   - plot_02_rank_dumbbell_report.png
#   - plot_03_rank_shift_summary.png
#   - plot_99_metric_facets_backup.png
#   - rank_summary.csv
#   - model_sensitivity_summary.csv
# =========================================================

library(tidyverse)
library(forcats)
library(scales)
library(glue)
library(grid)
library(gt)

# -----------------------------
# 0) Theme palette (DDE)
# -----------------------------
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

# Accent colors used only for semantic group highlighting
baseline_group_colors <- c(
  "Top 10 in baseline" = dde_blue,
  "Bottom 10 in baseline" = dde_orange
)

# -----------------------------
# 1) Read data
# -----------------------------
input_candidates <- c(
  "/mnt/data/modelComp_plot.csv",
  "~/Desktop/modelComp_plot.csv",
  "modelComp_plot.csv"
)
input_file <- input_candidates[file.exists(path.expand(input_candidates))][1]

if (is.na(input_file)) {
  stop("Could not find modelComp_plot.csv in /mnt/data, ~/Desktop, or the working directory.")
}

df_plot <- read_csv(path.expand(input_file), show_col_types = FALSE)

# -----------------------------
# 2) Model order and aliases
# -----------------------------
model_levels <- c(
  "school_year + low_income",
  "school_year + gender + low_income",
  "school_year + low_income + race",
  "school_year + low_income + sped",
  "school_year + ell + low_income",
  "school_year + county + low_income",
  "school_year + county + ell + gender + low_income + race + sped"
)

model_alias_map <- c(
  "school_year + low_income" = "Baseline",
  "school_year + gender + low_income" = "+ Gender",
  "school_year + low_income + race" = "+ Race",
  "school_year + low_income + sped" = "+ SPED",
  "school_year + ell + low_income" = "+ ELL",
  "school_year + county + low_income" = "+ County",
  "school_year + county + ell + gender + low_income + race + sped" = "All Covariates"
)

baseline_model <- "school_year + low_income"
model_levels <- model_levels[model_levels %in% unique(df_plot$model)]
model_alias_levels <- unname(model_alias_map[model_levels])
comparison_alias_levels <- setdiff(model_alias_levels, model_alias_map[[baseline_model]])

# -----------------------------
# 3) Choose school identifier
# -----------------------------
id_cols <- intersect(c("SchoolName", "SchoolCode", "ScaleScore.mean", "n"), names(df_plot))

if (!"SchoolName" %in% names(df_plot)) {
  stop("Expected a SchoolName column in modelComp_plot.csv.")
}

# -----------------------------
# 4) Reshape to one row per school x model
# -----------------------------
plot_dat <- df_plot %>%
  mutate(
    model = factor(model, levels = model_levels),
    SchoolName = as.character(SchoolName),
    SchoolCode = if ("SchoolCode" %in% names(.)) as.character(SchoolCode) else NA_character_
  ) %>%
  pivot_wider(
    id_cols = c(any_of(id_cols), model),
    names_from = metric,
    values_from = value
  ) %>%
  mutate(
    model_alias = recode(as.character(model), !!!model_alias_map),
    model_alias = factor(model_alias, levels = model_alias_levels),
    SchoolName = stringr::str_wrap(SchoolName, width = 24)
  )

# -----------------------------
# 5) Baseline ordering and school set
# -----------------------------
baseline_dat <- plot_dat %>%
  filter(model == baseline_model) %>%
  arrange(.performance_rank_cv) %>%
  mutate(
    baseline_group = case_when(
      row_number() <= 10 ~ "Top 10 in baseline",
      row_number() > n() - 10 ~ "Bottom 10 in baseline",
      TRUE ~ "Other"
    )
  ) %>%
  transmute(
    SchoolName,
    SchoolCode,
    baseline_rank = .performance_rank_cv,
    baseline_group
  )

plot_dat <- plot_dat %>%
  left_join(baseline_dat, by = c("SchoolName", "SchoolCode")) %>%
  mutate(
    rank_change = .performance_rank_cv - baseline_rank,
    abs_rank_change = abs(rank_change),
    rank_dir_arrow = case_when(
      rank_change < 0 ~ "↑",
      rank_change > 0 ~ "↓",
      TRUE ~ "→"
    ),
    rank_label = paste0(rank_dir_arrow, "\n", round(.performance_rank_cv, 0)),
    baseline_group = factor(
      baseline_group,
      levels = c("Top 10 in baseline", "Bottom 10 in baseline", "Other")
    )
  )

school_order <- baseline_dat %>%
  arrange(baseline_rank) %>%
  pull(SchoolName)

plot_dat <- plot_dat %>%
  mutate(
    school_label = glue("#{baseline_rank}: {SchoolName}"),
    school_label = factor(
      school_label,
      levels = baseline_dat %>%
        arrange(baseline_rank) %>%
        transmute(lbl = glue("#{baseline_rank}: {SchoolName}")) %>%
        pull(lbl)
    ),
    school_label_rev = fct_rev(school_label),
    school_label_simple = factor(SchoolName, levels = school_order),
    school_label_simple_rev = fct_rev(school_label_simple)
  )

plot_dat_20 <- plot_dat %>%
  filter(baseline_group %in% c("Top 10 in baseline", "Bottom 10 in baseline"))

# -----------------------------
# 6) DDE theme helper
# -----------------------------
theme_modelcomp <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = dde_bg, color = NA),
      panel.background = element_rect(fill = dde_surface, color = NA),
      panel.grid.major.x = element_line(color = dde_border, linewidth = 0.35),
      panel.grid.major.y = element_line(color = dde_border, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", color = dde_blue_dark, size = rel(1.25)),
      plot.subtitle = element_text(color = dde_muted, size = rel(0.98), margin = margin(b = 8)),
      plot.caption = element_text(color = dde_muted, hjust = 0, size = rel(0.9)),
      axis.title = element_text(color = dde_text, face = "bold"),
      axis.text = element_text(color = dde_text),
      #axis.text.x = element_text(angle = 35, hjust = 1),
      strip.background = element_rect(fill = dde_orange_soft, color = dde_border_strong, linewidth = 0.6),
      strip.text = element_text(face = "bold", color = dde_blue_dark),
      legend.position = "bottom",
      legend.background = element_rect(fill = dde_bg, color = NA),
      legend.key = element_rect(fill = dde_bg, color = NA),
      panel.border = element_rect(color = dde_border, fill = NA, linewidth = 0.6)
    )
}

# -----------------------------
# 7) Compact model sensitivity summary
# -----------------------------
model_sensitivity_summary <- plot_dat_20 %>%
  filter(model != baseline_model) %>%
  group_by(model_alias) %>%
  summarise(
    mean_abs_rank_change = mean(abs_rank_change, na.rm = TRUE),
    median_abs_rank_change = median(abs_rank_change, na.rm = TRUE),
    max_abs_rank_change = max(abs_rank_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    model_alias = factor(model_alias, levels = comparison_alias_levels),
    takeaway = paste0("Avg |Δ rank| = ", number(mean_abs_rank_change, accuracy = 0.1))
  ) %>%
  arrange(model_alias)

headline_takeaway <- model_sensitivity_summary %>%
  arrange(desc(mean_abs_rank_change)) %>%
  slice_head(n = 1) %>%
  transmute(txt = paste0(as.character(model_alias), " shows the largest average shift (",
                         number(mean_abs_rank_change, accuracy = 0.1), " ranks).")) %>%
  pull(txt)

# -----------------------------
# 8) Plot 1: Headline rank heatmap
# -----------------------------
p_rank_heat <- ggplot(
  plot_dat_20,
  aes(x = model_alias, y = school_label_simple_rev)
) +
  geom_tile(fill = dde_surface_soft, color = dde_border, linewidth = 0.6) +
  geom_text(aes(label = rank_label), size = 3.4, fontface = "bold", lineheight = 0.9, color = dde_text) +
  facet_grid(baseline_group ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "How ranks shift when the benchmark definition changes",
    subtitle = "Same 20 schools tracked across benchmark definitions. ↑ better rank, ↓ worse rank, → unchanged. Smaller rank numbers are better.",
    x = NULL,
    y = NULL,
    caption = "Baseline = school_year + low_income"
  ) +
  theme_modelcomp() +
  theme(
    strip.text.y = element_text(face = "bold"),
    panel.spacing.y = unit(1.1, "lines")
  )

# -----------------------------
# 9) Plot 2: Dumbbell plot (proof figure)
# -----------------------------
dumbbell_dat_all <- plot_dat_20 %>%
  filter(model %in% model_levels) %>%
  select(
    SchoolName, school_label, baseline_group,
    model, model_alias, baseline_rank, .performance_rank_cv
  ) %>%
  filter(model != baseline_model) %>%
  mutate(
    comparison_model = factor(model_alias, levels = comparison_alias_levels),
    comparison_rank = .performance_rank_cv
  ) %>%
  select(
    SchoolName, school_label, baseline_group,
    comparison_model, baseline_rank, comparison_rank
  )

x_breaks <- sort(unique(c(
  1,
  seq(10, 100, by = 15),
  max(c(dumbbell_dat_all$baseline_rank, dumbbell_dat_all$comparison_rank), na.rm = TRUE)
)))

p_dumbbell_all <- ggplot(
  dumbbell_dat_all,
  aes(y = fct_rev(school_label))
) +
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
  scale_color_manual(values = baseline_group_colors, drop = FALSE) +
  scale_x_reverse(breaks = x_breaks) +
  facet_grid(baseline_group ~ comparison_model, scales = "free_y", space = "free_y") +
  labs(
    title = "How far schools move under alternative benchmark definitions",
    subtitle = "Open circle = baseline rank; filled circle = rank under the comparison model. Smaller rank numbers are better.",
    x = "Performance rank relative to other schools",
    y = NULL,
    color = NULL,
    caption = "Baseline = school_year + low_income"
  ) +
  theme_modelcomp() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(0.95, "lines"),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# -----------------------------
# 10) Plot 3: Compact sensitivity summary
# -----------------------------
p_rank_shift_summary <- ggplot(
  model_sensitivity_summary,
  aes(x = model_alias, y = mean_abs_rank_change)
) +
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
    title = "Average absolute rank change by benchmark definition",
    subtitle = headline_takeaway,
    x = NULL,
    y = "Average |rank change|",
    caption = "Computed on the same 20 schools selected from the baseline model"
  ) +
  theme_modelcomp()

# -----------------------------
# 11) Backup plot: Metric trajectories
#     Keep as appendix / technical follow-up
# -----------------------------
metric_long <- plot_dat_20 %>%
  select(
    SchoolName, school_label, baseline_group, model, model_alias,
    .performance_rank_cv, .performance_z_cv, .resid_cv_1se, .pred_cv_1se
  ) %>%
  pivot_longer(
    cols = c(.performance_rank_cv, .performance_z_cv, .resid_cv_1se, .pred_cv_1se),
    names_to = "metric_name",
    values_to = "metric_value"
  ) %>%
  mutate(
    metric_name = recode(
      metric_name,
      .performance_rank_cv = "Overall rank",
      .performance_z_cv = "Performance z-score",
      .resid_cv_1se = "CV residual",
      .pred_cv_1se = "CV prediction"
    )
  )

p_metric_facets <- ggplot(
  metric_long,
  aes(
    x = model_alias,
    y = metric_value,
    group = SchoolName,
    color = baseline_group
  )
) +
  geom_line(alpha = 0.65, linewidth = 0.7) +
  geom_point(size = 1.8) +
  scale_color_manual(values = baseline_group_colors, drop = FALSE) +
  facet_wrap(~ metric_name, scales = "free_y", ncol = 2) +
  labs(
    title = "How model metrics change across benchmark definitions",
    subtitle = "Cross-validation results across benchmark definitions and outcome metrics.",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Same 20 schools tracked across the baseline and comparison models"
  ) +
  theme_modelcomp() +
  theme(legend.position = "bottom")

# -----------------------------
# 12) School-level summary table
# -----------------------------
rank_summary <- plot_dat_20 %>%
  group_by(SchoolName, baseline_rank, baseline_group, ScaleScore.mean, n) %>%
  summarise(
    best_rank = min(.performance_rank_cv, na.rm = TRUE),
    worst_rank = max(.performance_rank_cv, na.rm = TRUE),
    rank_range = paste0(round(best_rank), "–", round(worst_rank)),
    largest_rank_shift = max(abs(.performance_rank_cv - baseline_rank), na.rm = TRUE),
    
    min_pred = min(.pred_cv_1se, na.rm = TRUE),
    max_pred = max(.pred_cv_1se, na.rm = TRUE),
    largest_expected_score_shift = max(.pred_cv_1se, na.rm = TRUE) - min(.pred_cv_1se, na.rm = TRUE),
    
    min_z = min(.performance_z_cv, na.rm = TRUE),
    max_z = max(.performance_z_cv, na.rm = TRUE),
    largest_z_shift = max(.performance_z_cv, na.rm = TRUE) - min(.performance_z_cv, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    baseline_group = factor(
      baseline_group,
      levels = c("Top 10 in baseline", "Bottom 10 in baseline")
    )
  ) %>%
  arrange(baseline_group, baseline_rank)

rank_summary_table <- rank_summary %>%
  transmute(
    `Baseline group` = baseline_group,
    `Baseline rank` = round(baseline_rank),
    School = SchoolName,
    n = n,
    `Mean observed score` = ScaleScore.mean,
    `Best rank` = round(best_rank),
    `Worst rank` = round(worst_rank),
    `Largest rank shift` = round(largest_rank_shift, 1),
    `Largest expected-score shift` = round(largest_expected_score_shift, 1),
    `Largest z-score shift` = round(largest_z_shift, 2)
  )

tbl_rank_summary <- rank_summary_table %>%
  gt(groupname_col = "Baseline group") %>%
  tab_header(
    title = md("**School-level sensitivity to benchmark definition**"),
    subtitle = md("Top 10 and bottom 10 schools from the baseline model, tracked across alternative benchmark definitions")
  ) %>%
  cols_label(
    n = "Tested n"
  ) %>%
  fmt_number(
    columns = c(`Mean observed score`, `Largest rank shift`, `Largest expected-score shift`),
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(`Largest z-score shift`),
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Baseline rank`, School, n, `Mean observed score`, `Best rank`, `Worst rank`, 
                `Largest rank shift`, `Largest expected-score shift`, `Largest z-score shift`)
  ) %>%
  tab_spanner(
    label = "Baseline",
    columns = c(`Baseline rank`, School, n, `Mean observed score`)
  ) %>%
  tab_spanner(
    label = "Sensitivity across benchmark definitions",
    columns = c(`Best rank`, `Worst rank`, 
                `Largest rank shift`, `Largest expected-score shift`, `Largest z-score shift`)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#194a78"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "#123758"),
    locations = cells_row_groups()
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.background.color = "#f5f7fb",
    heading.background.color = "#ffffff",
    row.striping.background_color = "#fbfcfe",
    table.border.top.color = "#c7d5e2",
    table.border.bottom.color = "#c7d5e2",
    column_labels.border.top.color = "#c7d5e2",
    column_labels.border.bottom.color = "#c7d5e2",
    table.font.color = "#1f2937",
    source_notes.font.size = px(11),
    data_row.padding = px(6)
  ) %>%
  tab_source_note(
    source_note = md("**Note.** Baseline = `school_year + low_income`. Smaller rank numbers are better. `n` is the number of tested students contributing to the school mean.")
  )

tbl_rank_summary

gtsave(tbl_rank_summary, "table_rank_summary.html")
gtsave(tbl_rank_summary, "table_rank_summary.png")

# -----------------------------
# 13) Print only the main figures by default
# -----------------------------
print(p_rank_heat)
print(p_dumbbell_all)
print(p_rank_shift_summary)
print(p_metric_facets)

# -----------------------------
# 14) Save outputs
# -----------------------------
ggsave("plot_01_rank_heatmap_report.png", p_rank_heat, width = 11.5, height = 8.5, dpi = 320, bg = dde_bg)
ggsave("plot_02_rank_dumbbell_report.png", p_dumbbell_all, width = 14.5, height = 10.2, dpi = 320, bg = dde_bg)
ggsave("plot_03_rank_shift_summary.png", p_rank_shift_summary, width = 9.5, height = 5.8, dpi = 320, bg = dde_bg)
ggsave("plot_99_metric_facets_backup.png", p_metric_facets, width = 12.5, height = 8.2, dpi = 320, bg = dde_bg)

write_csv(rank_summary, "rank_summary.csv")
write_csv(model_sensitivity_summary, "model_sensitivity_summary.csv")

message("Saved report-ready figures and summaries to the working directory.")
