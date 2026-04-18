#--------------------------low income-----------------------------------------#
###############################################################################
fit_Li <- 
  fit_ridge_benchmark_model(df = df,
                            outcome = outcome,
                            weight_var = weight_var,
                            school_id = school_id,
                            mandatory_groups = mandatory_groups,
                            group_toggles = c(
                              na           = FALSE,
                              county       = FALSE,
                              ell          = FALSE,
                              foster_care  = FALSE,
                              gender       = FALSE,
                              geography    = FALSE,
                              
                              homeless     = FALSE,
                              immersion    = FALSE,
                              low_income   = TRUE,
                              migrant      = FALSE,
                              military_dep = FALSE,
                              race         = FALSE,
                              sped         = FALSE,
                              units        = FALSE
                            ),
                            named_predictor_groups = named_predictor_groups,
                            non_predictor_columns = non_predictor_columns,
                            lambda_grid = lambda_grid,
                            nfolds = nfolds,
                            seed = seed,
                            neutral_band_multiplier = neutral_band_multiplier)
score_Li <-
  fit_Li$scored_data %>% 
  mutate(model = paste(fit_Li$benchmark_spec$group, collapse = " + "),
         .before = SchoolYear)


#--------------------------low income + gender--------------------------------#
###############################################################################
fit_LiGe <- 
  fit_ridge_benchmark_model(df = df,
                            outcome = outcome,
                            weight_var = weight_var,
                            school_id = school_id,
                            mandatory_groups = mandatory_groups,
                            group_toggles = c(
                              na           = FALSE,
                              county       = FALSE,
                              ell          = FALSE,
                              foster_care  = FALSE,
                              gender       = TRUE,
                              geography    = FALSE,
                              
                              homeless     = FALSE,
                              immersion    = FALSE,
                              low_income   = TRUE,
                              migrant      = FALSE,
                              military_dep = FALSE,
                              race         = FALSE,
                              sped         = FALSE,
                              units        = FALSE
                            ),
                            named_predictor_groups = named_predictor_groups,
                            non_predictor_columns = non_predictor_columns,
                            lambda_grid = lambda_grid,
                            nfolds = nfolds,
                            seed = seed,
                            neutral_band_multiplier = neutral_band_multiplier)
score_LiGe <-
  fit_LiGe$scored_data %>% 
  mutate(model = paste(fit_LiGe$benchmark_spec$group, collapse = " + "),
         .before = SchoolYear)


#--------------------------low income + race----------------------------------#
###############################################################################
fit_LiRa <- 
  fit_ridge_benchmark_model(df = df,
                            outcome = outcome,
                            weight_var = weight_var,
                            school_id = school_id,
                            mandatory_groups = mandatory_groups,
                            group_toggles = c(
                              na           = FALSE,
                              county       = FALSE,
                              ell          = FALSE,
                              foster_care  = FALSE,
                              gender       = FALSE,
                              geography    = FALSE,
                              
                              homeless     = FALSE,
                              immersion    = FALSE,
                              low_income   = TRUE,
                              migrant      = FALSE,
                              military_dep = FALSE,
                              race         = TRUE,
                              sped         = FALSE,
                              units        = FALSE
                            ),
                            named_predictor_groups = named_predictor_groups,
                            non_predictor_columns = non_predictor_columns,
                            lambda_grid = lambda_grid,
                            nfolds = nfolds,
                            seed = seed,
                            neutral_band_multiplier = neutral_band_multiplier)
score_LiRa <-
  fit_LiRa$scored_data %>% 
  mutate(model = paste(fit_LiRa$benchmark_spec$group, collapse = " + "),
         .before = SchoolYear)


#--------------------------low income + sped----------------------------------#
###############################################################################
fit_LiSp <- 
  fit_ridge_benchmark_model(df = df,
                            outcome = outcome,
                            weight_var = weight_var,
                            school_id = school_id,
                            mandatory_groups = mandatory_groups,
                            group_toggles = c(
                              na           = FALSE,
                              county       = FALSE,
                              ell          = FALSE,
                              foster_care  = FALSE,
                              gender       = FALSE,
                              geography    = FALSE,
                              
                              homeless     = FALSE,
                              immersion    = FALSE,
                              low_income   = TRUE,
                              migrant      = FALSE,
                              military_dep = FALSE,
                              race         = FALSE,
                              sped         = TRUE,
                              units        = FALSE
                            ),
                            named_predictor_groups = named_predictor_groups,
                            non_predictor_columns = non_predictor_columns,
                            lambda_grid = lambda_grid,
                            nfolds = nfolds,
                            seed = seed,
                            neutral_band_multiplier = neutral_band_multiplier)
score_LiSp <-
  fit_LiSp$scored_data %>% 
  mutate(model = paste(fit_LiSp$benchmark_spec$group, collapse = " + "),
         .before = SchoolYear)


#--------------------------low income + ell-----------------------------------#
###############################################################################
fit_LiEl <- 
  fit_ridge_benchmark_model(df = df,
                            outcome = outcome,
                            weight_var = weight_var,
                            school_id = school_id,
                            mandatory_groups = mandatory_groups,
                            group_toggles = c(
                              na           = FALSE,
                              county       = FALSE,
                              ell          = TRUE,
                              foster_care  = FALSE,
                              gender       = FALSE,
                              geography    = FALSE,
                              
                              homeless     = FALSE,
                              immersion    = FALSE,
                              low_income   = TRUE,
                              migrant      = FALSE,
                              military_dep = FALSE,
                              race         = FALSE,
                              sped         = FALSE,
                              units        = FALSE
                            ),
                            named_predictor_groups = named_predictor_groups,
                            non_predictor_columns = non_predictor_columns,
                            lambda_grid = lambda_grid,
                            nfolds = nfolds,
                            seed = seed,
                            neutral_band_multiplier = neutral_band_multiplier)
score_LiEl <-
  fit_LiEl$scored_data %>% 
  mutate(model = paste(fit_LiEl$benchmark_spec$group, collapse = " + "),
         .before = SchoolYear)


#--------------------------low income + county--------------------------------#
###############################################################################
fit_LiCo <- 
  fit_ridge_benchmark_model(df = df,
                            outcome = outcome,
                            weight_var = weight_var,
                            school_id = school_id,
                            mandatory_groups = mandatory_groups,
                            group_toggles = c(
                              na           = FALSE,
                              county       = TRUE,
                              ell          = FALSE,
                              foster_care  = FALSE,
                              gender       = FALSE,
                              geography    = FALSE,
                              
                              homeless     = FALSE,
                              immersion    = FALSE,
                              low_income   = TRUE,
                              migrant      = FALSE,
                              military_dep = FALSE,
                              race         = FALSE,
                              sped         = FALSE,
                              units        = FALSE
                            ),
                            named_predictor_groups = named_predictor_groups,
                            non_predictor_columns = non_predictor_columns,
                            lambda_grid = lambda_grid,
                            nfolds = nfolds,
                            seed = seed,
                            neutral_band_multiplier = neutral_band_multiplier)
score_LiCo <-
  fit_LiCo$scored_data %>% 
  mutate(model = paste(fit_LiCo$benchmark_spec$group, collapse = " + "),
         .before = SchoolYear)


#--------------low income + gender + race + sped + ell + county---------------#
###############################################################################
fit_LiGeRaSpElCo <- 
  fit_ridge_benchmark_model(df = df,
                            outcome = outcome,
                            weight_var = weight_var,
                            school_id = school_id,
                            mandatory_groups = mandatory_groups,
                            group_toggles = c(
                              na           = FALSE,
                              county       = TRUE,
                              ell          = TRUE,
                              foster_care  = FALSE,
                              gender       = TRUE,
                              geography    = FALSE,
                              
                              homeless     = FALSE,
                              immersion    = FALSE,
                              low_income   = TRUE,
                              migrant      = FALSE,
                              military_dep = FALSE,
                              race         = TRUE,
                              sped         = TRUE,
                              units        = FALSE
                            ),
                            named_predictor_groups = named_predictor_groups,
                            non_predictor_columns = non_predictor_columns,
                            lambda_grid = lambda_grid,
                            nfolds = nfolds,
                            seed = seed,
                            neutral_band_multiplier = neutral_band_multiplier)
score_LiGeRaSpElCo <-
  fit_LiGeRaSpElCo$scored_data %>% 
  mutate(model = paste(fit_LiGeRaSpElCo$benchmark_spec$group, collapse = " + "),
         .before = SchoolYear)


#---------------------------compare rankings----------------------------------#
###############################################################################
scoreAll <-
  bind_rows(score_Li,
            score_LiGe,
            score_LiRa,
            score_LiSp,
            score_LiEl,
            score_LiCo,
            score_LiGeRaSpElCo) %>% 
  mutate(model = fct_inorder(model))

top10bottom10_2025_Li <-
  scoreAll %>% 
  filter(SchoolYear == 2025,
         model == "school_year + low_income") %>%
  arrange(.performance_rank_cv) %>% 
  slice(c(1:10, (n() - 9):n()))

top10bottom10_2025_other <-
  scoreAll %>% 
  filter(SchoolYear == 2025,
         model != "school_year + low_income",
         SchoolCode %in% top10bottom10_2025_Li$SchoolCode)

modelComp <-
  bind_rows(top10bottom10_2025_Li,
            top10bottom10_2025_other) %>% 
  select(model:ELL.ELX,
         Gender.M,
         LowIncome.LOWINC,
         RaceReportTitle.African_American:SPEDCode.900,
         .pred_cv_1se,
         .resid_cv_1se,
         .performance_z_cv,
         .performance_rank_cv)

modelComp_wide <-
  modelComp %>% 
  pivot_wider(id_cols = c(SchoolYear:SPEDCode.900),
  names_from = model,
  values_from = c(.pred_cv_1se:.performance_rank_cv))

modelComp_plot <-
  modelComp %>% 
  select(model:n,
         .pred_cv_1se:.performance_rank_cv) %>% 
  pivot_longer(.pred_cv_1se:.performance_rank_cv,
               names_to = "metric") %>% 
  mutate(SchoolYear = factor(SchoolYear)) %>% 
  left_join(LEA_META %>% distinct(DistrictName,
                                  SchoolName,
                                  SchoolCode,
                                  ModelGrade)) %>% 
  mutate(SchoolName = str_remove_all(SchoolName, " School"),
         SchoolName = str_remove_all(SchoolName, " Elementary"),
         SchoolName = str_wrap(SchoolName, width = 22))
write_csv(modelComp_plot, "~/Desktop/modelComp_plot.csv")







