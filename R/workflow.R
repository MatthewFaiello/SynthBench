# =========================================================
# R/workflow.R
# Historical benchmarking workflow
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
    wmae = sum(w * abs(resid)) / w_sum,
    wr2 = ifelse(sst > 0, 1 - sse / sst, NA_real_)
  )
}

make_school_foldid <- function(dat, seed, nfolds) {
  set.seed(seed)
  
  schools <- sort(unique(dat[[SETTINGS$school_id]]))
  k <- min(nfolds, length(schools))
  
  school_fold <- sample(rep(seq_len(k), length.out = length(schools)))
  names(school_fold) <- schools
  
  as.integer(school_fold[match(dat[[SETTINGS$school_id]], schools)])
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  
  if (length(x) <= 1) {
    return(0)
  }
  
  stats::sd(x)
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

weighted_sd_pop <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & (w > 0)
  
  x <- x[ok]
  w <- w[ok]
  
  if (length(x) <= 1 || sum(w) <= 0) {
    return(NA_real_)
  }
  
  mu <- weighted.mean(x, w)
  sqrt(sum(w * (x - mu)^2) / sum(w))
}

choose_nfolds <- function(n_schools,
                          default_nfolds = SETTINGS$nfolds,
                          small_sample_cutoff = 60,
                          small_sample_nfolds = 5) {
  if (n_schools < small_sample_cutoff) {
    return(small_sample_nfolds)
  }
  
  default_nfolds
}

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

get_group_label <- function(group_key) {
  lookup <- c(
    school_year = "School year",
    na = "Missing scores",
    units = "Student count",
    county = "County",
    ell = "ELL",
    foster_care = "Foster care",
    gender = "Gender",
    geography = "Wilmington",
    homeless = "Homelessness",
    immersion = "Immersion",
    low_income = "Low income",
    migrant = "Migrant",
    military_dep = "Military-connected",
    race = "Race",
    sped = "SPED",
    other = "Other"
  )
  
  unname(lookup[group_key])
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


make_formula_label <- function(selected_groups) {
  if (length(selected_groups) == 0) {
    return(SETTINGS$mandatory_group)
  }
  
  paste0(
    SETTINGS$mandatory_group,
    "(",
    paste(selected_groups, collapse = " + "),
    ")"
  )
}

make_rhs_terms <- function(predictor_groups, selected_groups) {
  year_cols <- predictor_groups[[SETTINGS$mandatory_group]]
  
  if (length(selected_groups) > 0) {
    selected_cols <- unlist(predictor_groups[selected_groups], use.names = FALSE)
  } else {
    selected_cols <- character(0)
  }
  
  rhs_terms <- year_cols
  
  if (length(selected_cols) > 0) {
    interaction_terms <- as.vector(
      outer(year_cols, selected_cols, paste, sep = ":")
    )
    
    rhs_terms <- c(
      rhs_terms,
      selected_cols,
      interaction_terms
    )
  }
  
  unique(rhs_terms)
}

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
  
  n_schools <- n_distinct(df[[SETTINGS$school_id]])
  nfolds_use <- choose_nfolds(n_schools)
  
  predictor_groups <- list(
    school_year = names(df)[startsWith(names(df), "SchoolYear.")],
    na = intersect("na", names(df)),
    units = intersect("units", names(df)),
    county = names(df)[startsWith(names(df), "County_Name.")],
    ell = names(df)[startsWith(names(df), "ELL.")],
    foster_care = names(df)[startsWith(names(df), "FosterCare.")],
    gender = names(df)[startsWith(names(df), "Gender.")],
    geography = names(df)[startsWith(names(df), "Geography.")],
    homeless = names(df)[startsWith(names(df), "Homeless.")],
    immersion = names(df)[startsWith(names(df), "Immersion.")],
    low_income = names(df)[startsWith(names(df), "LowIncome.")],
    migrant = names(df)[startsWith(names(df), "Migrant.")],
    military_dep = names(df)[startsWith(names(df), "MilitaryDep.")],
    race = names(df)[startsWith(names(df), "RaceReportTitle.")],
    sped = names(df)[startsWith(names(df), "SPEDCode.")]
  )
  
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
  
  baseline_model <- names(model_toggles)[1]
  
  score_all_models <- list()
  coef_table_models <- list()
  coef_audit_models <- list()
  coef_influence_models <- list()
  model_audit_models <- list()
  
  score_all_repeats_models <- list()
  model_audit_repeats_models <- list()
  
  # ---------------- fit each model ---------------- #
  for (model_name in names(model_toggles)) {
    group_toggles <- model_toggles[[model_name]]
    selected_groups <- names(group_toggles)[group_toggles]
    
    formula_label <- make_formula_label(selected_groups)
    
    rhs_terms <- make_rhs_terms(
      predictor_groups = predictor_groups,
      selected_groups = selected_groups
    )
    
    formula_obj <- as.formula(
      paste(SETTINGS$outcome, "~", paste(rhs_terms, collapse = " + "))
    )
    
    X <- model.matrix(formula_obj, data = df)
    
    if ("(Intercept)" %in% colnames(X)) {
      X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    }
    
    keep_cols <- apply(X, 2, stats::sd, na.rm = TRUE) > 0
    keep_cols[is.na(keep_cols)] <- FALSE
    X <- X[, keep_cols, drop = FALSE]
    
    y <- df[[SETTINGS$outcome]]
    w <- df[[SETTINGS$weight_var]]
    
    x_term_stats <- build_x_term_stats(X, w)
    term_meta <- build_term_meta(colnames(X))
    
    scored_repeats_one_model <- list()
    audit_repeats_one_model <- list()
    coef_repeats_one_model <- list()
    coef_table_one <- NULL
    
    # ---------------- repeated grouped CV ---------------- #
    for (cv_repeat in seq_len(SETTINGS$cv_repeats)) {
      cv_seed <- SETTINGS$cv_seed_start + cv_repeat - 1
      foldid <- make_school_foldid(df, seed = cv_seed, nfolds = nfolds_use)
      
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
    
    score_all_models[[model_name]] <- scored_data
    coef_table_models[[model_name]] <- coef_table_one
    coef_audit_models[[model_name]] <- coef_audit_one
    coef_influence_models[[model_name]] <- coef_influence_one
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
  
  coef_influence <- bind_rows(coef_influence_models) %>%
    mutate(model = factor(model, levels = names(model_toggles)))
  
  coef_influence_no_year <- coef_influence %>%
    filter(group_key != "school_year")
  
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
    filter(SchoolYear == selection_year) %>%
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
    select(-c(
      .performance_direction,
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
      SPEDCode.900
    )) %>%
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
    nfolds_used = nfolds_use,
    top_n = top_n,
    score_all_rows = nrow(score_all),
    score_all_repeats_rows = nrow(score_all_repeats),
    coef_table_rows = nrow(coef_table),
    coef_audit_rows = nrow(coef_audit),
    coef_influence_rows = nrow(coef_influence),
    coef_influence_no_year_rows = nrow(coef_influence_no_year),
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
    coef_influence = coef_influence,
    coef_influence_no_year = coef_influence_no_year,
    model_audit = model_audit,
    model_comp_table = model_comp_table,
    model_comp_plot = model_comp_plot,
    score_all_repeats = score_all_repeats,
    model_audit_repeats = model_audit_repeats,
    post_run_audit = post_run_audit
  )
}

# test <- run_benchmark_workflow()
