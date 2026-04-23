# =========================================================
# R/workflow.R
# Benchmark workflow and modeling helpers
# =========================================================

weighted_metrics <- function(y, pred, w) {
  ok <- is.finite(y) & is.finite(pred) & is.finite(w)
  
  y <- y[ok]
  pred <- pred[ok]
  w <- w[ok]
  
  w_sum <- sum(w)
  
  resid <- y - pred
  ybar <- sum(w * y) / w_sum
  
  sse <- sum(w * resid^2)
  sst <- sum(w * (y - ybar)^2)
  
  tibble(
    wrmse = sqrt(sse / w_sum),
    wmae  = sum(w * abs(resid)) / w_sum,
    wr2   = ifelse(sst > 0, 1 - sse / sst, NA_real_)
  )
}

make_school_foldid <- function(dat, seed) {
  set.seed(seed)
  
  schools <- sort(unique(dat[[SETTINGS$school_id]]))
  k <- min(SETTINGS$nfolds, length(schools))
  
  school_fold <- sample(rep(seq_len(k), length.out = length(schools)))
  names(school_fold) <- schools
  
  as.integer(school_fold[match(dat[[SETTINGS$school_id]], schools)])
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  
  if (length(x) <= 1) {
    return(0)
  }
  
  sd(x)
}

most_common <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  names(sort(table(x), decreasing = TRUE))[1]
}

consistency_rate <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  max(prop.table(table(x)))
}

weighted_residual_sd <- function(resid, w) {
  ok <- is.finite(resid) & is.finite(w)
  
  resid <- resid[ok]
  w <- w[ok]
  
  if (length(resid) == 0 || sum(w) <= 0) {
    return(NA_real_)
  }
  
  resid_mean <- weighted.mean(resid, w, na.rm = TRUE)
  resid_sd <- sqrt(weighted.mean((resid - resid_mean)^2, w, na.rm = TRUE))
  
  if (!is.finite(resid_sd) || resid_sd <= 0) {
    return(NA_real_)
  }
  
  resid_sd
}

# Build a default toggle object that matches the structure expected by
# run_benchmark_workflow(). This keeps manual/test calls compatible with
# the app's character-vector UI defaults.
DEFAULT_MODEL_TOGGLES <- lapply(DEFAULT_MODEL_SELECTIONS, build_toggle_vector)

run_benchmark_workflow <- function(model_key = "grade_03__DeSSA.ELA",
                                   model_toggles = DEFAULT_MODEL_TOGGLES,
                                   selection_year = 2018,
                                   top_n = SETTINGS$comparison_top_n,
                                   min_students_tested = 10,
                                   neutral_band_multiplier = SETTINGS$neutral_band_multiplier) {
  
  # ---------------- data prep ---------------- #
  df <- APP_DATA[[model_key]] %>%
    filter(n >= min_students_tested) %>%
    mutate(.row_id = row_number())
  
  if (nrow(df) == 0) {
    stop("No rows remain after filtering by min_students_tested.", call. = FALSE)
  }
  
  predictor_groups <- list(
    school_year  = names(df)[startsWith(names(df), "SchoolYear.")],
    na           = intersect("na", names(df)),
    units        = intersect("units", names(df)),
    county       = names(df)[startsWith(names(df), "County_Name.")],
    ell          = names(df)[startsWith(names(df), "ELL.")],
    foster_care  = names(df)[startsWith(names(df), "FosterCare.")],
    gender       = names(df)[startsWith(names(df), "Gender.")],
    geography    = names(df)[startsWith(names(df), "Geography.")],
    homeless     = names(df)[startsWith(names(df), "Homeless.")],
    immersion    = names(df)[startsWith(names(df), "Immersion.")],
    low_income   = names(df)[startsWith(names(df), "LowIncome.")],
    migrant      = names(df)[startsWith(names(df), "Migrant.")],
    military_dep = names(df)[startsWith(names(df), "MilitaryDep.")],
    race         = names(df)[startsWith(names(df), "RaceReportTitle.")],
    sped         = names(df)[startsWith(names(df), "SPEDCode.")]
  )
  
  prep_audit <- list(
    model_key = model_key,
    n_rows = nrow(df),
    n_schools = n_distinct(df[[SETTINGS$school_id]]),
    school_years = sort(unique(df$SchoolYear)),
    model_grade = unique(df$ModelGrade),
    assessment_label = unique(df$AssessmentLabel),
    predictor_counts = map_int(predictor_groups, length)
  )
  
  baseline_model <- names(model_toggles)[1]
  
  score_all_models <- list()
  coef_table_models <- list()
  coef_audit_models <- list()
  model_audit_models <- list()
  
  score_all_repeats_models <- list()
  model_audit_repeats_models <- list()
  
  # ---------------- fit each model ---------------- #
  for (model_name in names(model_toggles)) {
    group_toggles <- model_toggles[[model_name]]
    selected_groups <- names(group_toggles)[group_toggles]
    
    if (length(selected_groups) > 0) {
      formula_label <- paste0(
        SETTINGS$mandatory_group,
        "(",
        paste(selected_groups, collapse = " + "),
        ")"
      )
    } else {
      formula_label <- SETTINGS$mandatory_group
    }
    
    year_cols <- predictor_groups[[SETTINGS$mandatory_group]]
    
    if (length(selected_groups) > 0) {
      selected_cols <- unlist(predictor_groups[selected_groups], use.names = FALSE)
    } else {
      selected_cols <- character(0)
    }
    
    rhs_terms <- year_cols
    
    if (length(selected_cols) > 0) {
      rhs_terms <- c(
        rhs_terms,
        selected_cols,
        as.vector(outer(year_cols, selected_cols, paste, sep = ":"))
      )
    }
    
    rhs_terms <- unique(rhs_terms)
    
    formula_obj <- as.formula(
      paste(SETTINGS$outcome, "~", paste(rhs_terms, collapse = " + "))
    )
    
    X <- model.matrix(formula_obj, data = df)
    
    if ("(Intercept)" %in% colnames(X)) {
      X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    }
    
    keep_cols <- apply(X, 2, sd, na.rm = TRUE) > 0
    keep_cols[is.na(keep_cols)] <- FALSE
    X <- X[, keep_cols, drop = FALSE]
    
    y <- df[[SETTINGS$outcome]]
    w <- df[[SETTINGS$weight_var]]
    
    scored_repeats_one_model <- list()
    audit_repeats_one_model <- list()
    coef_repeats_one_model <- list()
    coef_table_one <- NULL
    
    # ---------------- repeated grouped CV ---------------- #
    for (cv_repeat in seq_len(SETTINGS$cv_repeats)) {
      cv_seed <- SETTINGS$cv_seed_start + cv_repeat - 1
      foldid <- make_school_foldid(df, seed = cv_seed)
      
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
      
      scored_one <- df %>%
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
        )
      
      resid_sd <- weighted_residual_sd(
        resid = scored_one$.resid_cv,
        w = scored_one[[SETTINGS$weight_var]]
      )
      neutral_band <- resid_sd * neutral_band_multiplier
      
      scored_one <- scored_one %>%
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
      
      metrics_cv <- weighted_metrics(y, pred_cv, w)
      
      audit_repeats_one_model[[cv_repeat]] <- tibble(
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
        n_schools = n_distinct(df[[SETTINGS$school_id]]),
        n_features = ncol(X),
        lambda_choice = SETTINGS$lambda_choice,
        lambda_selected = lambda_selected,
        wrmse_cv = metrics_cv$wrmse,
        wmae_cv = metrics_cv$wmae,
        wr2_cv = metrics_cv$wr2,
        resid_cv_weighted_sd = resid_sd,
        neutral_band_cv = neutral_band
      )
      
      scored_repeats_one_model[[cv_repeat]] <- scored_one
      
      coef_mat <- as.matrix(coef(cv_fit, s = SETTINGS$lambda_choice))
      coef_repeats_one_model[[cv_repeat]] <- tibble(
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
      
      if (is.null(coef_table_one)) {
        coef_table_one <- coef_repeats_one_model[[cv_repeat]]
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
    
    resid_sd <- weighted_residual_sd(
      resid = scored_data$.resid_cv,
      w = scored_data[[SETTINGS$weight_var]]
    )
    neutral_band <- resid_sd * neutral_band_multiplier
    
    scored_data <- scored_data %>%
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
      ungroup() %>%
      left_join(
        LEA_META,
        by = c("SchoolYear", "SchoolCode", "ModelGrade")
      )
    
    model_audit_one <- model_audit_repeats_one_model %>%
      summarise(
        model = first(model),
        formula_label = first(formula_label),
        selected_groups = first(selected_groups),
        n_cv_repeats = n(),
        n_obs = first(n_obs),
        n_schools = first(n_schools),
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
    
    coef_audit_one <- model_coef_repeats_one_model %>%
      group_by(model, formula_label, term) %>%
      summarise(
        n_cv_repeats = n(),
        b_cv_mean = mean(estimate, na.rm = TRUE),
        b_cv_sd = safe_sd(estimate),
        .groups = "drop"
      )
    
    score_all_models[[model_name]] <- scored_data
    coef_table_models[[model_name]] <- coef_table_one
    coef_audit_models[[model_name]] <- coef_audit_one
    model_audit_models[[model_name]] <- model_audit_one
    
    score_all_repeats_models[[model_name]] <- score_all_repeats_one_model
    model_audit_repeats_models[[model_name]] <- model_audit_repeats_one_model
  }
  
  # ---------------- combine outputs across models ---------------- #
  score_all <- bind_rows(score_all_models) %>%
    mutate(model = factor(model, levels = names(model_toggles)))
  
  coef_table <- bind_rows(coef_table_models) %>%
    mutate(model = factor(model, levels = names(model_toggles)))
  
  coef_audit <- bind_rows(coef_audit_models) %>%
    mutate(model = factor(model, levels = names(model_toggles)))
  
  model_audit <- bind_rows(model_audit_models) %>%
    mutate(model = factor(model, levels = names(model_toggles)))
  
  score_all_repeats <- bind_rows(score_all_repeats_models) %>%
    mutate(model = factor(model, levels = names(model_toggles)))
  
  model_audit_repeats <- bind_rows(model_audit_repeats_models) %>%
    mutate(model = factor(model, levels = names(model_toggles)))
  
  # ---------------- build comparison data for visuals ---------------- #
  baseline_rows <- score_all %>%
    filter(
      SchoolYear == selection_year,
      as.character(model) == baseline_model
    ) %>%
    arrange(.performance_rank_cv)
  
  top_n <- min(top_n, floor(nrow(baseline_rows) / 2))
  
  if (top_n < 1) {
    stop("Not enough baseline schools to create top/bottom comparison groups.", call. = FALSE)
  }
  
  focus_schools <- bind_rows(
    slice_head(baseline_rows, n = top_n),
    slice_tail(baseline_rows, n = top_n)
  ) %>%
    distinct(SchoolCode) %>%
    pull(SchoolCode)
  
  model_comp_table <- score_all %>%
    filter(
      SchoolYear == selection_year
    ) %>%
    select(
      model,
      formula_label,
      SchoolYear,
      DistrictName,
      SchoolName,
      SchoolCode,
      ModelGrade,
      all_of(SETTINGS$outcome),
      n,
      na,
      units,
      County_Name.New_Castle,
      County_Name.Sussex,
      ELL.ELL,
      ELL.ELM,
      ELL.ELX,
      FosterCare.FOSTR,
      Gender.M,
      Geography.W,
      Homeless.HOMLES,
      Immersion.IMM,
      LowIncome.LOWINC,
      Migrant.MIGRNT,
      MilitaryDep.MILTRY,
      RaceReportTitle.African_American,
      RaceReportTitle.American_Indian,
      RaceReportTitle.Asian,
      RaceReportTitle.Hawaiian,
      RaceReportTitle.Hispanic_Latino,
      RaceReportTitle.Multi_Racial,
      SPEDCode.100,
      SPEDCode.1000,
      SPEDCode.1100,
      SPEDCode.1200,
      SPEDCode.1300,
      SPEDCode.1400,
      SPEDCode.200,
      SPEDCode.300,
      SPEDCode.400,
      SPEDCode.500,
      SPEDCode.601,
      SPEDCode.602,
      SPEDCode.700,
      SPEDCode.800,
      SPEDCode.900,
      .pred_cv,
      .resid_cv,
      .performance_z_cv,
      .performance_rank_cv,
      .performance_direction
    ) 
  
  model_comp_plot <-
    model_comp_table %>%
    filter(SchoolCode %in% focus_schools) %>% 
    select(-c(.performance_direction,
              na,
              units,
              County_Name.New_Castle,
              County_Name.Sussex,
              ELL.ELL,
              ELL.ELM,
              ELL.ELX,
              FosterCare.FOSTR,
              Gender.M,
              Geography.W,
              Homeless.HOMLES,
              Immersion.IMM,
              LowIncome.LOWINC,
              Migrant.MIGRNT,
              MilitaryDep.MILTRY,
              RaceReportTitle.African_American,
              RaceReportTitle.American_Indian,
              RaceReportTitle.Asian,
              RaceReportTitle.Hawaiian,
              RaceReportTitle.Hispanic_Latino,
              RaceReportTitle.Multi_Racial,
              SPEDCode.100,
              SPEDCode.1000,
              SPEDCode.1100,
              SPEDCode.1200,
              SPEDCode.1300,
              SPEDCode.1400,
              SPEDCode.200,
              SPEDCode.300,
              SPEDCode.400,
              SPEDCode.500,
              SPEDCode.601,
              SPEDCode.602,
              SPEDCode.700,
              SPEDCode.800,
              SPEDCode.900)) %>% 
    pivot_longer(
      cols = c(.pred_cv, .resid_cv, .performance_z_cv, .performance_rank_cv),
      names_to = "metric",
      values_to = "value"
    ) %>%
    rename("ScaleScore.mean" = all_of(SETTINGS$outcome)) %>%
    mutate(SchoolYear = factor(SchoolYear))
  
  post_run_audit <- list(
    n_models = length(model_toggles),
    models = names(model_toggles),
    baseline_model = baseline_model,
    selection_year = selection_year,
    min_students_tested = min_students_tested,
    neutral_band_multiplier = neutral_band_multiplier,
    top_n = top_n,
    score_all_rows = nrow(score_all),
    score_all_repeats_rows = nrow(score_all_repeats),
    coef_table_rows = nrow(coef_table),
    coef_audit_rows = nrow(coef_audit),
    model_audit_rows = nrow(model_audit),
    model_audit_repeats_rows = nrow(model_audit_repeats),
    model_comp_plot_rows = nrow(model_comp_plot)
  )
  
  list(
    prep_audit = prep_audit,
    predictor_groups = predictor_groups,
    score_all = score_all,
    coef_table = coef_table,
    coef_audit = coef_audit,
    model_audit = model_audit,
    model_comp_table = model_comp_table,
    model_comp_plot = model_comp_plot,
    score_all_repeats = score_all_repeats,
    model_audit_repeats = model_audit_repeats,
    post_run_audit = post_run_audit
  )
}

#test <- run_benchmark_workflow()
