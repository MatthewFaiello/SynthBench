# =========================================================
# R/workflow.R
# Historical benchmarking workflow
# =========================================================

add_benchmark_performance <- function(dat,
                                      neutral_band_multiplier,
                                      weight_var = SETTINGS$weight_var) {
  required_cols <- c(
    "SchoolYear",
    ".resid_cv",
    weight_var
  )
  
  missing_cols <- setdiff(required_cols, names(dat))
  
  if (length(missing_cols) > 0) {
    stop(
      "Cannot compute benchmark performance. Missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  resid_sd <- weighted_residual_sd(
    resid = dat$.resid_cv,
    w = dat[[weight_var]]
  )
  
  neutral_band <- resid_sd * neutral_band_multiplier
  
  dat %>%
    mutate(
      .resid_cv_weighted_sd = resid_sd,
      .neutral_band_cv = neutral_band,
      .performance_z_cv = .resid_cv / .resid_cv_weighted_sd,
      .performance_direction = case_when(
        .resid_cv > .neutral_band_cv ~ "above_expected",
        .resid_cv < -.neutral_band_cv ~ "below_expected",
        TRUE ~ "near_expected"
      )
    ) %>%
    group_by(SchoolYear) %>%
    arrange(desc(.performance_z_cv), .by_group = TRUE) %>%
    mutate(.performance_rank_cv = min_rank(desc(.performance_z_cv))) %>%
    ungroup()
}

DEFAULT_MODEL_TOGGLES <- lapply(DEFAULT_MODEL_SELECTIONS, build_toggle_vector)

prepare_model_data <- function(model_key,
                               min_students_tested) {
  df <- get_model_data(model_key) %>%
    mutate(
      SchoolYear = as.integer(SchoolYear),
      SchoolCode = as.character(SchoolCode),
      ModelGrade = as.character(ModelGrade)
    ) %>%
    filter(n >= min_students_tested) %>%
    mutate(.row_id = row_number())
  
  if (nrow(df) == 0) {
    stop("No rows remain after filtering by min_students_tested.", call. = FALSE)
  }
  
  n_schools <- n_distinct(df[[SETTINGS$school_id]])
  
  if (n_schools < SETTINGS$min_schools_for_cv) {
    stop(
      paste0(
        "At least ", SETTINGS$min_schools_for_cv,
        " schools are required after filtering to run grouped cross-validation. ",
        "Only ", n_schools, " schools remain. Lower the minimum tested-student ",
        "threshold or choose a broader assessment/grade scope."
      ),
      call. = FALSE
    )
  }
  
  nfolds_use <- min(choose_nfolds(n_schools), n_schools)
  
  predictor_groups <- get_predictor_groups(df)
  
  prep_audit <- list(
    model_key = model_key,
    n_rows = nrow(df),
    n_schools = n_schools,
    nfolds_used = nfolds_use,
    school_years = sort(unique(df$SchoolYear)),
    model_grade = unique(df$ModelGrade),
    assessment_label = unique(df$AssessmentLabel),
    predictor_counts = sapply(predictor_groups, length)
  )
  
  list(
    df = df,
    n_schools = n_schools,
    nfolds_use = nfolds_use,
    predictor_groups = predictor_groups,
    prep_audit = prep_audit
  )
}

fit_cv_repeat <- function(df,
                          X,
                          y,
                          w,
                          model_name,
                          formula_label,
                          selected_groups,
                          cv_repeat,
                          n_schools,
                          nfolds_use,
                          neutral_band_multiplier) {
  cv_seed <- SETTINGS$cv_seed_start + cv_repeat - 1
  
  foldid <- make_school_foldid(
    dat = df,
    seed = cv_seed,
    nfolds = nfolds_use
  )
  
  cv_fit <- cv.glmnet(
    x = X,
    y = y,
    weights = w,
    alpha = 0,
    family = "gaussian",
    type.measure = "mse",
    standardize = TRUE,
    foldid = foldid,
    keep = TRUE
  )
  
  lambda_selected <- cv_fit[[SETTINGS$lambda_choice]]
  idx_selected <- which.min(abs(cv_fit$lambda - lambda_selected))
  pred_cv <- as.numeric(cv_fit$fit.preval[, idx_selected])
  
  scored <- df %>%
    transmute(
      .row_id,
      SchoolYear,
      SchoolCode,
      ModelGrade,
      n,
      !!SETTINGS$outcome := .data[[SETTINGS$outcome]],
      !!SETTINGS$weight_var := .data[[SETTINGS$weight_var]],
      model = model_name,
      formula_label = formula_label,
      cv_repeat = cv_repeat,
      cv_seed = cv_seed,
      .pred_cv = pred_cv,
      .resid_cv = .data[[SETTINGS$outcome]] - .pred_cv
    ) %>%
    add_benchmark_performance(
      neutral_band_multiplier = neutral_band_multiplier
    )
  
  resid_sd <- first(scored$.resid_cv_weighted_sd)
  neutral_band <- first(scored$.neutral_band_cv)
  
  metrics_cv <- weighted_metrics(
    y = y,
    pred = pred_cv,
    w = w
  )
  
  audit <- tibble(
    model = model_name,
    formula_label = formula_label,
    selected_groups = ifelse(
      length(selected_groups) == 0,
      "none",
      paste(selected_groups, collapse = ", ")
    ),
    cv_repeat = cv_repeat,
    cv_seed = cv_seed,
    n_obs = nrow(df),
    n_schools = n_schools,
    nfolds_used = nfolds_use,
    n_features = ncol(X),
    lambda_choice = SETTINGS$lambda_choice,
    lambda_selected = lambda_selected,
    wrmse_cv = metrics_cv$wrmse,
    wmae_cv = metrics_cv$wmae,
    wr2_cv = metrics_cv$wr2,
    resid_cv_weighted_sd = resid_sd,
    neutral_band_cv = neutral_band
  )
  
  coef_mat <- as.matrix(coef(cv_fit, s = SETTINGS$lambda_choice))
  
  coef_table <- tibble(
    cv_repeat = cv_repeat,
    term = rownames(coef_mat),
    estimate = as.numeric(coef_mat[, 1])
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      model = model_name,
      formula_label = formula_label
    ) %>%
    arrange(desc(abs(estimate)))
  
  list(
    scored = scored,
    audit = audit,
    coef_table = coef_table
  )
}

fit_benchmark_model <- function(df,
                                predictor_groups,
                                model_name,
                                group_toggles,
                                n_schools,
                                nfolds_use,
                                neutral_band_multiplier) {
  # ---------------- design matrix ---------------- #
  
  design <- make_benchmark_design(
    df = df,
    predictor_groups = predictor_groups,
    group_toggles = group_toggles
  )
  
  selected_groups <- design$selected_groups
  formula_label <- design$formula_label
  X <- design$X
  y <- design$y
  w <- design$w
  x_term_stats <- design$x_term_stats
  term_meta <- design$term_meta
  
  scored_repeats_one_model <- list()
  audit_repeats_one_model <- list()
  coef_repeats_one_model <- list()
  coef_table_one <- NULL
  
  # ---------------- repeated grouped CV ---------------- #
  
  for (cv_repeat in seq_len(SETTINGS$cv_repeats)) {
    repeat_result <- fit_cv_repeat(
      df = df,
      X = X,
      y = y,
      w = w,
      model_name = model_name,
      formula_label = formula_label,
      selected_groups = selected_groups,
      cv_repeat = cv_repeat,
      n_schools = n_schools,
      nfolds_use = nfolds_use,
      neutral_band_multiplier = neutral_band_multiplier
    )
    
    scored_repeats_one_model[[cv_repeat]] <- repeat_result$scored
    audit_repeats_one_model[[cv_repeat]] <- repeat_result$audit
    coef_repeats_one_model[[cv_repeat]] <- repeat_result$coef_table
    
    # Preserve existing behavior: use the first repeat's coefficient
    # table for the single displayed coefficient table.
    if (is.null(coef_table_one)) {
      coef_table_one <- repeat_result$coef_table
    }
  }
  
  # ---------------- combine repeated CV results ---------------- #
  
  score_all_repeats_one_model <- bind_rows(scored_repeats_one_model)
  model_audit_repeats_one_model <- bind_rows(audit_repeats_one_model)
  model_coef_repeats_one_model <- bind_rows(coef_repeats_one_model)
  
  score_summary <- score_all_repeats_one_model %>%
    group_by(.row_id) %>%
    summarise(
      .pred_cv_mean = mean(.pred_cv, na.rm = TRUE),
      .pred_cv_sd = safe_sd(.pred_cv),
      .performance_rank_cv_sd = safe_sd(.performance_rank_cv),
      .performance_direction_mode = most_common(.performance_direction),
      .performance_direction_consistency = consistency_rate(.performance_direction),
      n_cv_repeats_used = sum(is.finite(.pred_cv)),
      .groups = "drop"
    )
  
  scored_data <- df %>%
    mutate(
      model = model_name,
      formula_label = formula_label
    ) %>%
    left_join(score_summary, by = ".row_id") %>%
    mutate(
      .pred_cv = .pred_cv_mean,
      .resid_cv = .data[[SETTINGS$outcome]] - .pred_cv
    ) %>%
    select(-.pred_cv_mean)
  
  scored_data <- add_benchmark_performance(
    dat = scored_data,
    neutral_band_multiplier = neutral_band_multiplier
  )
  
  resid_sd <- first(scored_data$.resid_cv_weighted_sd)
  neutral_band <- first(scored_data$.neutral_band_cv)
  
  scored_data <- scored_data %>%
    left_join(
      LEA_META %>%
        mutate(
          SchoolYear = as.integer(SchoolYear),
          SchoolCode = as.character(SchoolCode),
          ModelGrade = as.character(ModelGrade)
        ),
      by = c("SchoolYear", "SchoolCode", "ModelGrade")
    )
  
  # ---------------- audit outputs ---------------- #
  
  model_audit_one <- model_audit_repeats_one_model %>%
    summarise(
      model = first(model),
      formula_label = first(formula_label),
      selected_groups = first(selected_groups),
      n_cv_repeats = n(),
      n_obs = first(n_obs),
      n_schools = first(n_schools),
      nfolds_used = first(nfolds_used),
      n_features = first(n_features),
      lambda_choice = first(lambda_choice),
      lambda_selected_median = median(lambda_selected, na.rm = TRUE),
      lambda_selected_iqr = IQR(lambda_selected, na.rm = TRUE),
      wrmse_cv_mean = mean(wrmse_cv, na.rm = TRUE),
      wrmse_cv_sd = safe_sd(wrmse_cv),
      wmae_cv_mean = mean(wmae_cv, na.rm = TRUE),
      wmae_cv_sd = safe_sd(wmae_cv),
      wr2_cv_mean = mean(wr2_cv, na.rm = TRUE),
      wr2_cv_sd = safe_sd(wr2_cv),
      resid_cv_weighted_sd = resid_sd,
      neutral_band_cv = neutral_band
    ) %>%
    rename(
      lambda_selected = lambda_selected_median,
      wrmse_cv = wrmse_cv_mean,
      wmae_cv = wmae_cv_mean,
      wr2_cv = wr2_cv_mean
    )
  
  # ---------------- coefficient outputs ---------------- #
  
  coef_table_one <- standardize_coef_table(
    coef_table_one = coef_table_one,
    x_term_stats = x_term_stats,
    term_meta = term_meta,
    y = y,
    w = w
  )
  
  coef_audit_one <- standardize_coef_audit(
    model_coef_repeats_one_model = model_coef_repeats_one_model,
    x_term_stats = x_term_stats,
    term_meta = term_meta,
    y = y,
    w = w
  )
  
  coef_influence_one <- build_coef_influence(coef_audit_one)
  
  list(
    score_all = scored_data,
    coef_table = coef_table_one,
    coef_audit = coef_audit_one,
    coef_influence = coef_influence_one,
    model_audit = model_audit_one,
    score_all_repeats = score_all_repeats_one_model,
    model_audit_repeats = model_audit_repeats_one_model
  )
}

combine_benchmark_models <- function(model_results,
                                     model_order) {
  score_all <- bind_rows(
    lapply(model_results, `[[`, "score_all")
  ) %>%
    mutate(model = factor(model, levels = model_order))
  
  coef_table <- bind_rows(
    lapply(model_results, `[[`, "coef_table")
  ) %>%
    mutate(model = factor(model, levels = model_order))
  
  coef_audit <- bind_rows(
    lapply(model_results, `[[`, "coef_audit")
  ) %>%
    mutate(model = factor(model, levels = model_order))
  
  coef_influence <- bind_rows(
    lapply(model_results, `[[`, "coef_influence")
  ) %>%
    mutate(model = factor(model, levels = model_order))
  
  coef_influence_no_year <- coef_influence %>%
    filter(group_key != "school_year")
  
  model_audit <- bind_rows(
    lapply(model_results, `[[`, "model_audit")
  ) %>%
    mutate(model = factor(model, levels = model_order))
  
  score_all_repeats <- bind_rows(
    lapply(model_results, `[[`, "score_all_repeats")
  ) %>%
    mutate(model = factor(model, levels = model_order))
  
  model_audit_repeats <- bind_rows(
    lapply(model_results, `[[`, "model_audit_repeats")
  ) %>%
    mutate(model = factor(model, levels = model_order))
  
  list(
    score_all = score_all,
    coef_table = coef_table,
    coef_audit = coef_audit,
    coef_influence = coef_influence,
    coef_influence_no_year = coef_influence_no_year,
    model_audit = model_audit,
    score_all_repeats = score_all_repeats,
    model_audit_repeats = model_audit_repeats
  )
}

build_selected_year_outputs <- function(score_all,
                                        score_all_repeats,
                                        coef_table,
                                        coef_audit,
                                        coef_influence,
                                        coef_influence_no_year,
                                        model_audit,
                                        model_audit_repeats,
                                        predictor_groups,
                                        model_toggles,
                                        baseline_model,
                                        selection_year,
                                        comparison_band_index,
                                        min_students_tested,
                                        neutral_band_multiplier,
                                        nfolds_use) {
  # ---------------- identify selected baseline rank bands ---------------- #
  
  baseline_rows <- score_all %>%
    filter(
      SchoolYear == selection_year,
      as.character(model) == baseline_model
    ) %>%
    arrange(.performance_rank_cv, SchoolName, SchoolCode)
  
  if (nrow(baseline_rows) < SETTINGS$comparison_band_count) {
    stop(
      paste0(
        "At least ", SETTINGS$comparison_band_count,
        " baseline schools are recommended to create comparison bands. ",
        "Only ", nrow(baseline_rows), " schools are available in the selected year."
      ),
      call. = FALSE
    )
  }
  
  focus_info <- build_comparison_focus_lookup(
    baseline_rows = baseline_rows,
    band_index = comparison_band_index,
    band_count = SETTINGS$comparison_band_count
  )
  
  comparison_focus_lookup <- focus_info$focus_lookup
  
  focus_schools <- comparison_focus_lookup %>%
    distinct(SchoolCode) %>%
    pull(SchoolCode)
  
  # ---------------- selected-year table ---------------- #
  
  feature_groups <- setdiff(
    names(predictor_groups),
    SETTINGS$mandatory_group
  )
  
  feature_cols <- predictor_groups[feature_groups] %>%
    unlist(use.names = FALSE) %>%
    unique()
  
  feature_cols <- intersect(feature_cols, names(score_all))
  
  model_comp_table <- score_all %>%
    filter(SchoolYear == selection_year) %>%
    mutate(
      SchoolYear = as.integer(SchoolYear),
      SchoolCode = as.character(SchoolCode),
      ModelGrade = as.character(ModelGrade)
    ) %>%
    select(
      model,
      formula_label,
      SchoolYear,
      DistrictName,
      SchoolName,
      SchoolCode,
      AssessmentLabel,
      ModelGrade,
      all_of(SETTINGS$outcome),
      n,
      any_of(feature_cols),
      .pred_cv,
      .resid_cv,
      .performance_z_cv,
      .performance_rank_cv,
      .performance_direction
    )
  
  # ---------------- long data for visuals ---------------- #
  
  make_model_comp_plot_long <- function(dat) {
    dat %>%
      select(
        -any_of(c(".performance_direction", feature_cols))
      ) %>%
      pivot_longer(
        cols = c(
          .pred_cv,
          .resid_cv,
          .performance_z_cv,
          .performance_rank_cv
        ),
        names_to = "metric",
        values_to = "value"
      ) %>%
      rename("ScaleScore.mean" = all_of(SETTINGS$outcome)) %>%
      mutate(SchoolYear = factor(SchoolYear))
  }
  
  # Full selected-year sample for aggregate diagnostics.
  model_comp_plot_all <- model_comp_table %>%
    make_model_comp_plot_long()
  
  # Tracked-school sample for detailed school-level visuals.
  model_comp_plot <- model_comp_table %>%
    filter(SchoolCode %in% focus_schools) %>%
    make_model_comp_plot_long()
  
  # ---------------- post-run audit ---------------- #
  
  post_run_audit <- list(
    n_models = length(model_toggles),
    models = names(model_toggles),
    baseline_model = baseline_model,
    selection_year = selection_year,
    min_students_tested = min_students_tested,
    neutral_band_multiplier = neutral_band_multiplier,
    nfolds_used = nfolds_use,
    
    comparison_band_count = SETTINGS$comparison_band_count,
    comparison_band_index = focus_info$band_index,
    comparison_lower_band = focus_info$lower_band,
    comparison_upper_band = focus_info$upper_band,
    comparison_lower_label = focus_info$lower_label,
    comparison_upper_label = focus_info$upper_label,
    comparison_lower_n = focus_info$lower_n,
    comparison_upper_n = focus_info$upper_n,
    comparison_focus_n = focus_info$focus_n,
    comparison_n_baseline_schools = focus_info$n_baseline_schools,
    
    score_all_rows = nrow(score_all),
    score_all_repeats_rows = nrow(score_all_repeats),
    coef_table_rows = nrow(coef_table),
    coef_audit_rows = nrow(coef_audit),
    coef_influence_rows = nrow(coef_influence),
    coef_influence_no_year_rows = nrow(coef_influence_no_year),
    model_audit_rows = nrow(model_audit),
    model_audit_repeats_rows = nrow(model_audit_repeats),
    model_comp_plot_rows = nrow(model_comp_plot),
    model_comp_plot_all_rows = nrow(model_comp_plot_all)
  )
  
  list(
    model_comp_table = model_comp_table,
    model_comp_plot = model_comp_plot,
    model_comp_plot_all = model_comp_plot_all,
    comparison_focus_lookup = comparison_focus_lookup,
    post_run_audit = post_run_audit
  )
}

rebuild_outputs_for_comparison_band <- function(run_result,
                                                comparison_band_index) {
  models <- run_result$post_run_audit$models
  
  model_toggles_stub <- stats::setNames(
    vector("list", length(models)),
    models
  )
  
  selected_year_outputs <- build_selected_year_outputs(
    score_all = run_result$score_all,
    score_all_repeats = run_result$score_all_repeats,
    coef_table = run_result$coef_table,
    coef_audit = run_result$coef_audit,
    coef_influence = run_result$coef_influence,
    coef_influence_no_year = run_result$coef_influence_no_year,
    model_audit = run_result$model_audit,
    model_audit_repeats = run_result$model_audit_repeats,
    predictor_groups = run_result$predictor_groups,
    model_toggles = model_toggles_stub,
    baseline_model = run_result$post_run_audit$baseline_model,
    selection_year = run_result$post_run_audit$selection_year,
    comparison_band_index = comparison_band_index,
    min_students_tested = run_result$post_run_audit$min_students_tested,
    neutral_band_multiplier = run_result$post_run_audit$neutral_band_multiplier,
    nfolds_use = run_result$post_run_audit$nfolds_used
  )
  
  run_result$model_comp_table <- selected_year_outputs$model_comp_table
  run_result$model_comp_plot <- selected_year_outputs$model_comp_plot
  run_result$model_comp_plot_all <- selected_year_outputs$model_comp_plot_all
  run_result$comparison_focus_lookup <- selected_year_outputs$comparison_focus_lookup
  run_result$post_run_audit <- selected_year_outputs$post_run_audit
  
  run_result
}

run_benchmark_workflow <- function(model_key = "grade_03__DeSSA.ELA",
                                   model_toggles = DEFAULT_MODEL_TOGGLES,
                                   selection_year = 2018,
                                   comparison_band_index = SETTINGS$comparison_band_index,
                                   min_students_tested = 10,
                                   neutral_band_multiplier = SETTINGS$neutral_band_multiplier) {
  
  # ---------------- data prep ---------------- #
  prepared <- prepare_model_data(
    model_key = model_key,
    min_students_tested = min_students_tested
  )
  
  df <- prepared$df
  n_schools <- prepared$n_schools
  nfolds_use <- prepared$nfolds_use
  predictor_groups <- prepared$predictor_groups
  prep_audit <- prepared$prep_audit
  
  baseline_model <- names(model_toggles)[1]
  
  model_results <- list()
  
  # ---------------- fit each model ---------------- #
  
  for (model_name in names(model_toggles)) {
    model_results[[model_name]] <- fit_benchmark_model(
      df = df,
      predictor_groups = predictor_groups,
      model_name = model_name,
      group_toggles = model_toggles[[model_name]],
      n_schools = n_schools,
      nfolds_use = nfolds_use,
      neutral_band_multiplier = neutral_band_multiplier
    )
  }
  
  # ---------------- combine outputs across models ---------------- #
  
  combined <- combine_benchmark_models(
    model_results = model_results,
    model_order = names(model_toggles)
  )
  
  score_all <- combined$score_all
  coef_table <- combined$coef_table
  coef_audit <- combined$coef_audit
  coef_influence <- combined$coef_influence
  coef_influence_no_year <- combined$coef_influence_no_year
  model_audit <- combined$model_audit
  score_all_repeats <- combined$score_all_repeats
  model_audit_repeats <- combined$model_audit_repeats
  
  # ---------------- build selected-year app outputs ---------------- #
  
  selected_year_outputs <- build_selected_year_outputs(
    score_all = score_all,
    score_all_repeats = score_all_repeats,
    coef_table = coef_table,
    coef_audit = coef_audit,
    coef_influence = coef_influence,
    coef_influence_no_year = coef_influence_no_year,
    model_audit = model_audit,
    model_audit_repeats = model_audit_repeats,
    predictor_groups = predictor_groups,
    model_toggles = model_toggles,
    baseline_model = baseline_model,
    selection_year = selection_year,
    comparison_band_index = comparison_band_index,
    min_students_tested = min_students_tested,
    neutral_band_multiplier = neutral_band_multiplier,
    nfolds_use = nfolds_use
  )
  
  model_comp_table <- selected_year_outputs$model_comp_table
  model_comp_plot <- selected_year_outputs$model_comp_plot
  model_comp_plot_all <- selected_year_outputs$model_comp_plot_all
  comparison_focus_lookup <- selected_year_outputs$comparison_focus_lookup
  post_run_audit <- selected_year_outputs$post_run_audit
  
  list(
    prep_audit = prep_audit,
    predictor_groups = predictor_groups,
    score_all = score_all,
    coef_table = coef_table,
    coef_audit = coef_audit,
    coef_influence = coef_influence,
    coef_influence_no_year = coef_influence_no_year,
    model_audit = model_audit,
    model_comp_table = model_comp_table,
    model_comp_plot = model_comp_plot,
    model_comp_plot_all = model_comp_plot_all,
    comparison_focus_lookup = comparison_focus_lookup,
    score_all_repeats = score_all_repeats,
    model_audit_repeats = model_audit_repeats,
    post_run_audit = post_run_audit
  )
}

# test <- run_benchmark_workflow()
