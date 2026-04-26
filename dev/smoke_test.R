# =========================================================
# dev/smoke_test.R
# Basic project smoke test.
#
# Verifies:
#   - data loading
#   - refactored helper-source availability
#   - selected helper behavior
#   - core workflow steps
#   - workflow output contract
#   - display-table export preparation
#
# Run from the project root:
#   source("dev/smoke_test.R")
# =========================================================

rm(list = ls())

source("global.R")

message("Loaded global.R")


# =========================================================
# Small smoke-test helpers
# =========================================================

require_objects <- function(object_names, label = "required objects") {
  missing_objects <- object_names[
    !vapply(object_names, exists, logical(1), inherits = TRUE)
  ]
  
  if (length(missing_objects) > 0) {
    stop(
      "Missing ", label, ": ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


require_functions <- function(function_names, label = "required functions") {
  missing_functions <- function_names[
    !vapply(
      function_names,
      exists,
      logical(1),
      mode = "function",
      inherits = TRUE
    )
  ]
  
  if (length(missing_functions) > 0) {
    stop(
      "Missing ", label, ": ",
      paste(missing_functions, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


require_columns <- function(dat, cols, object_name = deparse(substitute(dat))) {
  missing_cols <- setdiff(cols, names(dat))
  
  if (length(missing_cols) > 0) {
    stop(
      object_name,
      " is missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


require_nonempty_data_frame <- function(dat, object_name = deparse(substitute(dat))) {
  if (!is.data.frame(dat)) {
    stop(object_name, " is not a data frame.", call. = FALSE)
  }
  
  if (nrow(dat) == 0) {
    stop(object_name, " has zero rows.", call. = FALSE)
  }
  
  invisible(TRUE)
}


# =========================================================
# Data layer checks
# =========================================================

require_objects(
  c(
    "APP_DATA_FLAT",
    "LEA_META",
    "APP_DATA",
    "OPTIONS",
    "SCOPE_1_CHOICES",
    "SETTINGS",
    "DEFAULT_GROUP_TOGGLES",
    "DEFAULT_MODEL_TOGGLES"
  ),
  label = "data/config objects"
)

require_functions(
  c("get_model_data"),
  label = "data helper functions"
)

require_nonempty_data_frame(APP_DATA_FLAT, "APP_DATA_FLAT")
require_nonempty_data_frame(LEA_META, "LEA_META")
require_nonempty_data_frame(OPTIONS, "OPTIONS")

if (length(APP_DATA) == 0) {
  stop("APP_DATA has no model-key entries.", call. = FALSE)
}

if (length(SCOPE_1_CHOICES) == 0) {
  stop("SCOPE_1_CHOICES has no assessment choices.", call. = FALSE)
}

message("Data layer check completed")


# =========================================================
# Choose test scope
# =========================================================

model_key <- names(APP_DATA)[1]

if (is.null(model_key) || length(model_key) == 0 || is.na(model_key)) {
  stop("APP_DATA has no usable model keys.", call. = FALSE)
}

model_dat <- get_model_data(model_key)

require_nonempty_data_frame(model_dat, "model_dat")

require_columns(
  model_dat,
  c("SchoolYear", "SchoolCode", "ModelGrade", "AssessmentLabel", "n"),
  "model_dat"
)

selection_year <- max(model_dat$SchoolYear, na.rm = TRUE)

min_n <- min(model_dat$n, na.rm = TRUE)
max_n <- max(model_dat$n, na.rm = TRUE)
min_students_tested <- min(max(10, min_n), max_n)

if (!is.finite(selection_year)) {
  stop("selection_year is not finite.", call. = FALSE)
}

if (!is.finite(min_students_tested)) {
  stop("min_students_tested is not finite.", call. = FALSE)
}

message("Testing model key: ", model_key)
message("Testing selected year: ", selection_year)
message("Testing min_students_tested: ", min_students_tested)


# =========================================================
# Refactored helper availability checks
# =========================================================

required_helper_functions <- c(
  # labels.R
  "format_grade",
  "get_group_label",
  "pretty_feature_label",
  
  # app_choices.R
  "get_scope_2_choices",
  "get_year_choices",
  "get_range_n",
  "get_model_keys",
  
  # model_groups.R
  "get_predictor_groups",
  "build_toggle_vector",
  
  # visual_theme.R
  "theme_modelcomp",
  "empty_plot",
  
  # metrics.R
  "weighted_metrics",
  "safe_sd",
  "weighted_residual_sd",
  "weighted_sd_pop",
  
  # cv_helpers.R
  "make_school_foldid",
  "choose_nfolds",
  "most_common",
  "consistency_rate",
  
  # model_terms.R
  "get_term_group",
  "parse_coef_term",
  "build_term_meta",
  "build_x_term_stats",
  "make_relative_influence_label",
  
  # comparison_bands.R
  "format_band_pct",
  "get_comparison_band_labels",
  "validate_comparison_band",
  "build_comparison_focus_lookup",
  
  # model_design.R
  "make_formula_label",
  "make_rhs_terms",
  "make_benchmark_design",
  
  # coefficients.R
  "standardize_coef_table",
  "standardize_coef_audit",
  "build_coef_influence",
  
  # workflow.R
  "prepare_model_data",
  "fit_cv_repeat",
  "add_benchmark_performance",
  "fit_benchmark_model",
  "combine_benchmark_models",
  "build_selected_year_outputs",
  "rebuild_outputs_for_comparison_band",
  "run_benchmark_workflow",
  
  # result_table.R
  "ensure_columns",
  "make_model_comp_table",
  "write_model_comp_workbook",
  
  # settings_summary.R
  "label_selected_groups",
  "format_school_year",
  "format_comparison_band",
  "settings_item",
  "settings_summary_ui",
  
  # run_helpers.R
  "run_benchmark_and_visuals"
)

require_functions(
  required_helper_functions,
  label = "refactored helper functions"
)

require_objects(
  c("PLOT_SETTINGS"),
  label = "visual theme objects"
)

message("refactored helper availability check completed")


# =========================================================
# Selected helper behavior checks
# =========================================================

# ---------------- shared labels ---------------- #

if (!identical(format_grade("03"), "3rd")) {
  stop("format_grade('03') did not return the expected label.", call. = FALSE)
}

if (!identical(get_group_label("low_income"), "Low income")) {
  stop("get_group_label('low_income') did not return the expected label.", call. = FALSE)
}

if (!identical(pretty_feature_label("LowIncome.LOWINC"), "Low income")) {
  stop("pretty_feature_label('LowIncome.LOWINC') did not return the expected label.", call. = FALSE)
}

message("shared label helper check completed")


# ---------------- app choices ---------------- #

test_scope_1 <- unname(SCOPE_1_CHOICES[1])
test_scope_2_choices <- get_scope_2_choices(test_scope_1)

if (length(test_scope_2_choices) == 0) {
  stop("get_scope_2_choices() returned no choices.", call. = FALSE)
}

test_scope_2 <- unname(test_scope_2_choices[1])
test_year_choices <- get_year_choices(test_scope_1, test_scope_2)
test_range_n <- get_range_n(test_scope_1, test_scope_2)
test_model_keys <- get_model_keys(test_scope_1, test_scope_2)

if (length(test_year_choices) == 0) {
  stop("get_year_choices() returned no choices.", call. = FALSE)
}

if (!all(c("n_min", "n_max") %in% names(test_range_n))) {
  stop("get_range_n() did not return n_min and n_max.", call. = FALSE)
}

if (length(test_model_keys) == 0) {
  stop("get_model_keys() returned no model keys.", call. = FALSE)
}

message("app choice helper check completed")


# ---------------- model groups ---------------- #

test_groups <- get_predictor_groups(model_dat)

if (!SETTINGS$mandatory_group %in% names(test_groups)) {
  stop(
    "Mandatory predictor group was not returned by get_predictor_groups().",
    call. = FALSE
  )
}

test_toggle_vector <- build_toggle_vector(c("low_income", "ell"))

if (!is.logical(test_toggle_vector)) {
  stop("build_toggle_vector() did not return a logical vector.", call. = FALSE)
}

if (!isTRUE(test_toggle_vector[["low_income"]]) ||
    !isTRUE(test_toggle_vector[["ell"]])) {
  stop("build_toggle_vector() did not mark selected groups as TRUE.", call. = FALSE)
}

if (isTRUE(test_toggle_vector[["sped"]])) {
  stop("build_toggle_vector() marked an unselected group as TRUE.", call. = FALSE)
}

message("model-group helper check completed")


# ---------------- visual theme ---------------- #

test_empty_plot <- empty_plot("Smoke test")

if (!inherits(test_empty_plot, "ggplot")) {
  stop("empty_plot() did not return a ggplot object.", call. = FALSE)
}

if (!is.list(PLOT_SETTINGS) || is.null(PLOT_SETTINGS$base_size)) {
  stop("PLOT_SETTINGS does not contain base_size.", call. = FALSE)
}

message("shared visual theme helper check completed")


# ---------------- metrics ---------------- #

test_metric <- weighted_metrics(
  y = c(1, 2, 3),
  pred = c(1, 2, 4),
  w = c(1, 1, 1)
)

require_columns(test_metric, c("wrmse", "wmae", "wr2"), "test_metric")

test_resid_sd <- weighted_residual_sd(
  resid = c(-1, 0, 1),
  w = c(1, 1, 1)
)

if (!is.finite(test_resid_sd) || test_resid_sd <= 0) {
  stop("weighted_residual_sd() did not return a positive finite value.", call. = FALSE)
}

message("metric helper check completed")


# ---------------- CV helpers ---------------- #

test_foldid <- make_school_foldid(
  dat = model_dat,
  seed = 1000,
  nfolds = 5
)

if (length(test_foldid) != nrow(model_dat)) {
  stop("make_school_foldid() did not return one fold id per row.", call. = FALSE)
}

if (!all(is.finite(test_foldid))) {
  stop("make_school_foldid() returned non-finite fold ids.", call. = FALSE)
}

if (!identical(choose_nfolds(100), min(SETTINGS$nfolds, 100))) {
  stop(
    "choose_nfolds() did not return the expected fold count for a large sample.",
    call. = FALSE
  )
}

if (!identical(most_common(c("a", "b", "a")), "a")) {
  stop("most_common() did not return the expected value.", call. = FALSE)
}

if (!isTRUE(all.equal(consistency_rate(c("a", "a", "b")), 2 / 3))) {
  stop("consistency_rate() did not return the expected value.", call. = FALSE)
}

message("CV helper check completed")


# ---------------- model-term helpers ---------------- #

if (!identical(get_term_group("LowIncome.LOWINC"), "low_income")) {
  stop(
    "get_term_group() did not classify LowIncome.LOWINC as low_income.",
    call. = FALSE
  )
}

test_term_meta <- parse_coef_term("SchoolYear.2025:LowIncome.LOWINC")

if (!identical(test_term_meta$group_key[[1]], "low_income") ||
    !identical(test_term_meta$component[[1]], "year_interaction")) {
  stop(
    "parse_coef_term() did not parse a year-by-feature term as expected.",
    call. = FALSE
  )
}

test_relative_label <- make_relative_influence_label(0.60)

if (!identical(test_relative_label, "strong")) {
  stop("make_relative_influence_label(0.60) did not return 'strong'.", call. = FALSE)
}

message("model-term helper check completed")


# ---------------- comparison-band helpers ---------------- #

test_band_labels <- get_comparison_band_labels(
  band_index = 1,
  band_count = SETTINGS$comparison_band_count
)

if (!all(c("lower_label", "upper_label") %in% names(test_band_labels))) {
  stop("get_comparison_band_labels() did not return expected labels.", call. = FALSE)
}

validate_comparison_band(
  band_index = SETTINGS$comparison_band_index,
  band_count = SETTINGS$comparison_band_count
)

message("comparison-band helper check completed")


# ---------------- model-design helpers ---------------- #

test_formula_label <- make_formula_label(c("na", "low_income"))

if (!is.character(test_formula_label) || length(test_formula_label) != 1) {
  stop("make_formula_label() did not return a single character label.", call. = FALSE)
}

message("model-design helper check completed")


# ---------------- coefficient helpers ---------------- #

message("coefficient helper availability check completed")


# =========================================================
# Core workflow helper checks
# =========================================================

prepared <- prepare_model_data(
  model_key = model_key,
  min_students_tested = min_students_tested
)

required_prepared_objects <- c(
  "df",
  "n_schools",
  "nfolds_use",
  "predictor_groups",
  "prep_audit"
)

missing_prepared_objects <- setdiff(required_prepared_objects, names(prepared))

if (length(missing_prepared_objects) > 0) {
  stop(
    "prepare_model_data() result is missing objects: ",
    paste(missing_prepared_objects, collapse = ", "),
    call. = FALSE
  )
}

require_nonempty_data_frame(prepared$df, "prepared$df")

if (prepared$n_schools < SETTINGS$min_schools_for_cv) {
  stop("Not enough schools remain after filtering for CV.", call. = FALSE)
}

message("prepare_model_data() completed")


design <- make_benchmark_design(
  df = prepared$df,
  predictor_groups = prepared$predictor_groups,
  group_toggles = DEFAULT_MODEL_TOGGLES$Baseline
)

required_design_objects <- c(
  "selected_groups",
  "formula_label",
  "rhs_terms",
  "X",
  "y",
  "w",
  "x_term_stats",
  "term_meta"
)

missing_design_objects <- setdiff(required_design_objects, names(design))

if (length(missing_design_objects) > 0) {
  stop(
    "make_benchmark_design() result is missing objects: ",
    paste(missing_design_objects, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(design$X) != nrow(prepared$df)) {
  stop("Design matrix row count does not match prepared data row count.", call. = FALSE)
}

if (ncol(design$X) == 0) {
  stop("Design matrix has zero columns.", call. = FALSE)
}

if (length(design$y) != nrow(prepared$df)) {
  stop("Outcome vector length does not match prepared row count.", call. = FALSE)
}

if (length(design$w) != nrow(prepared$df)) {
  stop("Weight vector length does not match prepared row count.", call. = FALSE)
}

require_nonempty_data_frame(design$x_term_stats, "design$x_term_stats")
require_nonempty_data_frame(design$term_meta, "design$term_meta")

require_columns(
  design$x_term_stats,
  c("term", "x_w_sd"),
  "design$x_term_stats"
)

require_columns(
  design$term_meta,
  c("term", "group_key", "group_label", "component"),
  "design$term_meta"
)

message("make_benchmark_design() completed")


repeat_result <- fit_cv_repeat(
  df = prepared$df,
  X = design$X,
  y = design$y,
  w = design$w,
  model_name = "Baseline",
  formula_label = design$formula_label,
  selected_groups = design$selected_groups,
  cv_repeat = 1,
  n_schools = prepared$n_schools,
  nfolds_use = prepared$nfolds_use,
  neutral_band_multiplier = SETTINGS$neutral_band_multiplier
)

required_repeat_objects <- c("scored", "audit", "coef_table")
missing_repeat_objects <- setdiff(required_repeat_objects, names(repeat_result))

if (length(missing_repeat_objects) > 0) {
  stop(
    "fit_cv_repeat() result is missing objects: ",
    paste(missing_repeat_objects, collapse = ", "),
    call. = FALSE
  )
}

require_nonempty_data_frame(repeat_result$scored, "repeat_result$scored")
require_nonempty_data_frame(repeat_result$audit, "repeat_result$audit")
require_nonempty_data_frame(repeat_result$coef_table, "repeat_result$coef_table")

if (nrow(repeat_result$scored) != nrow(prepared$df)) {
  stop("fit_cv_repeat() scored output does not match prepared row count.", call. = FALSE)
}

require_columns(
  repeat_result$scored,
  c(".pred_cv", ".resid_cv", ".performance_z_cv"),
  "repeat_result$scored"
)

message("fit_cv_repeat() completed")


# =========================================================
# Full workflow check
# =========================================================

result <- run_benchmark_workflow(
  model_key = model_key,
  model_toggles = DEFAULT_MODEL_TOGGLES,
  selection_year = selection_year,
  comparison_band_index = SETTINGS$comparison_band_index,
  min_students_tested = min_students_tested,
  neutral_band_multiplier = SETTINGS$neutral_band_multiplier
)

required_result_objects <- c(
  "prep_audit",
  "predictor_groups",
  "score_all",
  "coef_table",
  "coef_audit",
  "coef_influence",
  "coef_influence_no_year",
  "model_audit",
  "model_comp_table",
  "model_comp_plot",
  "model_comp_plot_all",
  "comparison_focus_lookup",
  "score_all_repeats",
  "model_audit_repeats",
  "post_run_audit"
)

missing_result_objects <- setdiff(required_result_objects, names(result))

if (length(missing_result_objects) > 0) {
  stop(
    "Workflow result is missing objects: ",
    paste(missing_result_objects, collapse = ", "),
    call. = FALSE
  )
}

require_nonempty_data_frame(result$score_all, "result$score_all")
require_nonempty_data_frame(result$model_comp_table, "result$model_comp_table")
require_nonempty_data_frame(result$model_comp_plot, "result$model_comp_plot")
require_nonempty_data_frame(result$model_comp_plot_all, "result$model_comp_plot_all")
require_nonempty_data_frame(result$comparison_focus_lookup, "result$comparison_focus_lookup")


# ---------------- post-run audit list contract ---------------- #

if (!is.list(result$post_run_audit)) {
  stop("result$post_run_audit is not a list.", call. = FALSE)
}

required_post_run_audit_fields <- c(
  "n_models",
  "models",
  "baseline_model",
  "selection_year",
  "min_students_tested",
  "neutral_band_multiplier",
  "nfolds_used",
  "comparison_band_count",
  "comparison_band_index",
  "comparison_lower_band",
  "comparison_upper_band",
  "comparison_lower_label",
  "comparison_upper_label",
  "comparison_lower_n",
  "comparison_upper_n",
  "comparison_focus_n",
  "comparison_n_baseline_schools",
  "score_all_rows",
  "score_all_repeats_rows",
  "coef_table_rows",
  "coef_audit_rows",
  "coef_influence_rows",
  "coef_influence_no_year_rows",
  "model_audit_rows",
  "model_audit_repeats_rows",
  "model_comp_plot_rows",
  "model_comp_plot_all_rows"
)

missing_post_run_audit_fields <- setdiff(
  required_post_run_audit_fields,
  names(result$post_run_audit)
)

if (length(missing_post_run_audit_fields) > 0) {
  stop(
    "result$post_run_audit is missing fields: ",
    paste(missing_post_run_audit_fields, collapse = ", "),
    call. = FALSE
  )
}

if (!identical(
  as.character(result$post_run_audit$selection_year),
  as.character(selection_year)
)) {
  stop(
    "post_run_audit selection_year does not match the requested selection year.",
    call. = FALSE
  )
}

if (result$post_run_audit$score_all_rows <= 0) {
  stop("post_run_audit reports zero score_all rows.", call. = FALSE)
}

if (result$post_run_audit$model_comp_plot_rows <= 0) {
  stop("post_run_audit reports zero model_comp_plot rows.", call. = FALSE)
}

if (result$post_run_audit$model_comp_plot_all_rows <= 0) {
  stop("post_run_audit reports zero model_comp_plot_all rows.", call. = FALSE)
}

message("post-run audit check completed")


# ---------------- selected-year table contract ---------------- #

required_table_cols <- c(
  "model",
  "formula_label",
  "SchoolYear",
  "DistrictName",
  "SchoolName",
  "SchoolCode",
  "AssessmentLabel",
  "ModelGrade",
  "n",
  "ScaleScore.mean",
  ".pred_cv",
  ".resid_cv",
  ".performance_z_cv",
  ".performance_rank_cv",
  ".performance_direction"
)

require_columns(
  result$model_comp_table,
  required_table_cols,
  "result$model_comp_table"
)

if (!as.character(selection_year) %in% as.character(result$model_comp_table$SchoolYear)) {
  stop(
    "Selected year was not found in result$model_comp_table.",
    call. = FALSE
  )
}

message("run_benchmark_workflow() completed")


# =========================================================
# App-output helper checks
# =========================================================

message("run_benchmark_and_visuals() availability check completed")


display_table <- make_model_comp_table(
  dat = result$model_comp_table,
  format_for_display = FALSE
)

require_nonempty_data_frame(display_table, "display_table")

required_display_cols <- c(
  "Benchmark model",
  "Benchmark definition",
  "School year",
  "District name",
  "School name",
  "School code",
  "Assessment",
  "Grade",
  "Tested students",
  "Observed score",
  "Benchmark score",
  "Benchmark gap",
  "Scaled benchmark gap",
  "Within-year rank",
  "Benchmark label",
  "Percent missing scores"
)

require_columns(
  display_table,
  required_display_cols,
  "display_table"
)

if (!is.numeric(display_table[["Percent missing scores"]])) {
  stop(
    "Percent missing scores should be numeric when format_for_display = FALSE.",
    call. = FALSE
  )
}

if (!is.numeric(display_table[["Observed score"]])) {
  stop(
    "Observed score should be numeric in the unformatted display table.",
    call. = FALSE
  )
}

message("display-table helper check completed")


# Optional workbook smoke check using a temporary file.
tmp_xlsx <- tempfile(fileext = ".xlsx")

write_model_comp_workbook(
  dat = display_table,
  file = tmp_xlsx
)

if (!file.exists(tmp_xlsx)) {
  stop("write_model_comp_workbook() did not create an Excel file.", call. = FALSE)
}

tmp_xlsx_size <- file.info(tmp_xlsx)$size

if (is.na(tmp_xlsx_size) || tmp_xlsx_size <= 0) {
  stop("write_model_comp_workbook() created an empty Excel file.", call. = FALSE)
}

unlink(tmp_xlsx)

message("workbook export helper check completed")


message("Smoke test passed.")