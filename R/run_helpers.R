# =========================================================
# R/run_helpers.R
# Helpers that connect workflow results to app-ready outputs
# =========================================================
# Purpose:
#   Keep server.R focused on Shiny reactivity by moving small
#   non-reactive wrappers into this file.
#
# Main helpers:
#   - run_benchmark_and_visuals()
#
# Expected inputs:
#   User-selected model key, model toggles, selected year,
#   comparison-band index, minimum tested-student threshold,
#   and neutral-band multiplier.
# =========================================================

run_benchmark_and_visuals <- function(model_key,
                                      model_toggles,
                                      selection_year,
                                      comparison_band_index,
                                      min_students_tested,
                                      neutral_band_multiplier) {
  run_result <- run_benchmark_workflow(
    model_key = model_key,
    model_toggles = model_toggles,
    selection_year = selection_year,
    comparison_band_index = comparison_band_index,
    min_students_tested = min_students_tested,
    neutral_band_multiplier = neutral_band_multiplier
  )
  
  viz <- build_visual_data(
    model_comp_plot = run_result$model_comp_plot,
    model_comp_plot_all = run_result$model_comp_plot_all,
    baseline_model = run_result$post_run_audit$baseline_model,
    score_all = run_result$score_all,
    comparison_focus_lookup = run_result$comparison_focus_lookup,
    comparison_lower_label = run_result$post_run_audit$comparison_lower_label,
    comparison_upper_label = run_result$post_run_audit$comparison_upper_label
  )
  
  list(
    run_result = run_result,
    viz = viz
  )
}