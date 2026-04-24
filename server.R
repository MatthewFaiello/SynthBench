# =========================================================
# server.R
# Server logic for Model-Based Benchmarking
# =========================================================

# Plot resolution
res <- 85


# =========================================================
# Full-results table helpers
# =========================================================

make_model_comp_table <- function(dat, format_for_display = TRUE) {
  pct <- function(x) {
    if (format_for_display) {
      scales::percent(x, accuracy = 0.1)
    } else {
      x
    }
  }
  
  dat %>%
    transmute(
      `Benchmark model` = as.character(model),
      `Benchmark definition` = as.character(formula_label),
      `School year` = as.character(SchoolYear),
      
      County = case_when(
        County_Name.New_Castle == 1 ~ "New Castle County",
        County_Name.Sussex == 1 ~ "Sussex County",
        County_Name.New_Castle == 0 & County_Name.Sussex == 0 ~ "Kent County",
        TRUE ~ "Unknown / not available"
      ),
      
      `District name` = as.character(DistrictName),
      `School name` = as.character(SchoolName),
      `School code` = as.character(SchoolCode),
      Assessment = as.character(AssessmentLabel),
      Grade = vapply(as.character(ModelGrade), format_grade, character(1)),
      
      `Tested students` = as.integer(n),
      `Observed score` = round(ScaleScore.mean, 1),
      `Benchmark score` = round(.pred_cv, 1),
      `Benchmark gap` = round(.resid_cv, 1),
      `Scaled benchmark gap` = round(.performance_z_cv, 2),
      `Within-year rank` = as.integer(.performance_rank_cv),
      
      `Benchmark label` = recode(
        .performance_direction,
        above_expected = "Above benchmark",
        near_expected  = "Near benchmark",
        below_expected = "Below benchmark"
      ),
      
      `Percent missing scores` = pct(na),
      `Student units` = round(units, 1),
      
      ELL = pct(ELL.ELL),
      ELM = pct(ELL.ELM),
      ELX = pct(ELL.ELX),
      
      `Foster care` = pct(FosterCare.FOSTR),
      Male = pct(Gender.M),
      Wilmington = pct(Geography.W),
      Homeless = pct(Homeless.HOMLES),
      Immersion = pct(Immersion.IMM),
      `Low income` = pct(LowIncome.LOWINC),
      Migrant = pct(Migrant.MIGRNT),
      `Military connected` = pct(MilitaryDep.MILTRY),
      
      `African American` = pct(RaceReportTitle.African_American),
      `American Indian` = pct(RaceReportTitle.American_Indian),
      Asian = pct(RaceReportTitle.Asian),
      Hawaiian = pct(RaceReportTitle.Hawaiian),
      `Hispanic / Latino` = pct(RaceReportTitle.Hispanic_Latino),
      `Multi-racial` = pct(RaceReportTitle.Multi_Racial),
      
      `SPED 100` = pct(SPEDCode.100),
      `SPED 200` = pct(SPEDCode.200),
      `SPED 300` = pct(SPEDCode.300),
      `SPED 400` = pct(SPEDCode.400),
      `SPED 500` = pct(SPEDCode.500),
      `SPED 601` = pct(SPEDCode.601),
      `SPED 602` = pct(SPEDCode.602),
      `SPED 700` = pct(SPEDCode.700),
      `SPED 800` = pct(SPEDCode.800),
      `SPED 900` = pct(SPEDCode.900),
      `SPED 1000` = pct(SPEDCode.1000),
      `SPED 1100` = pct(SPEDCode.1100),
      `SPED 1200` = pct(SPEDCode.1200),
      `SPED 1300` = pct(SPEDCode.1300),
      `SPED 1400` = pct(SPEDCode.1400)
    ) %>%
    arrange(`District name`, `School name`)
}


write_model_comp_workbook <- function(dat, file) {
  wb <- createWorkbook()
  sheet <- "Benchmark results"
  
  addWorksheet(wb, sheet, gridLines = TRUE)
  writeData(wb, sheet, dat)
  
  header_style <- createStyle(
    textDecoration = "bold",
    fgFill = "dodgerblue4",
    fontColour = "white",
    border = "bottom",
    halign = "center",
    valign = "center"
  )
  
  int_style <- createStyle(numFmt = "0")
  one_dec_style <- createStyle(numFmt = "0.0")
  two_dec_style <- createStyle(numFmt = "0.00")
  pct_style <- createStyle(numFmt = "0.0%")
  
  addStyle(
    wb, sheet, header_style,
    rows = 1,
    cols = seq_len(ncol(dat)),
    gridExpand = TRUE
  )
  
  addFilter(
    wb, sheet,
    row = 1,
    cols = seq_len(ncol(dat))
  )
  
  freezePane(wb, sheet, firstRow = TRUE)
  
  int_cols <- which(names(dat) %in% c(
    "Tested students",
    "Within-year rank"
  ))
  
  one_dec_cols <- which(names(dat) %in% c(
    "Observed score",
    "Benchmark score",
    "Benchmark gap",
    "Student units"
  ))
  
  two_dec_cols <- which(names(dat) %in% c(
    "Scaled benchmark gap"
  ))
  
  pct_cols <- which(names(dat) %in% c(
    "Percent missing scores",
    "ELL", "ELM", "ELX",
    "Foster care", "Male", "Wilmington", "Homeless", "Immersion",
    "Low income", "Migrant", "Military connected",
    "African American", "American Indian", "Asian", "Hawaiian",
    "Hispanic / Latino", "Multi-racial",
    "SPED 100", "SPED 200", "SPED 300", "SPED 400", "SPED 500",
    "SPED 601", "SPED 602", "SPED 700", "SPED 800", "SPED 900",
    "SPED 1000", "SPED 1100", "SPED 1200", "SPED 1300", "SPED 1400"
  ))
  
  data_rows <- 2:(nrow(dat) + 1)
  
  if (nrow(dat) > 0 && length(int_cols) > 0) {
    addStyle(
      wb, sheet, int_style,
      rows = data_rows,
      cols = int_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  if (nrow(dat) > 0 && length(one_dec_cols) > 0) {
    addStyle(
      wb, sheet, one_dec_style,
      rows = data_rows,
      cols = one_dec_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  if (nrow(dat) > 0 && length(two_dec_cols) > 0) {
    addStyle(
      wb, sheet, two_dec_style,
      rows = data_rows,
      cols = two_dec_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  if (nrow(dat) > 0 && length(pct_cols) > 0) {
    addStyle(
      wb, sheet, pct_style,
      rows = data_rows,
      cols = pct_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  setColWidths(
    wb, sheet,
    cols = seq_len(ncol(dat)),
    widths = "auto"
  )
  
  saveWorkbook(wb, file, overwrite = TRUE)
}


# =========================================================
# Model-run helper
# =========================================================

run_benchmark_and_visuals <- function(model_key,
                                      model_toggles,
                                      selection_year,
                                      min_students_tested,
                                      neutral_band_multiplier) {
  run_result <- run_benchmark_workflow(
    model_key = model_key,
    model_toggles = model_toggles,
    selection_year = selection_year,
    min_students_tested = min_students_tested,
    neutral_band_multiplier = neutral_band_multiplier
  )
  
  viz <- build_visual_data(
    model_comp_plot = run_result$model_comp_plot,
    baseline_model = run_result$post_run_audit$baseline_model,
    score_all = run_result$score_all
  )
  
  list(
    run_result = run_result,
    viz = viz
  )
}


# =========================================================
# Current-settings summary helpers
# =========================================================

label_selected_groups <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("School year only")
  }
  
  group_values <- unname(GROUP_CHOICES)
  group_labels <- names(GROUP_CHOICES)
  
  if (is.null(group_labels)) {
    group_labels <- group_values
  }
  
  matched <- group_labels[match(x, group_values)]
  matched <- matched[!is.na(matched)]
  
  if (length(matched) == 0) {
    "School year only"
  } else {
    paste(matched, collapse = ", ")
  }
}


format_school_year <- function(x) {
  yr <- suppressWarnings(as.integer(x))
  
  if (is.na(yr)) {
    return(as.character(x))
  }
  
  paste0(yr - 1, "-", substr(as.character(yr), 3, 4))
}


format_grade <- function(x) {
  if (!exists("grade_labels", inherits = TRUE)) {
    return(as.character(x))
  }
  
  out <- unname(grade_labels[as.character(x)])
  
  if (length(out) == 0 || is.na(out)) {
    as.character(x)
  } else {
    out
  }
}


settings_item <- function(label, value, wide = FALSE) {
  value <- if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    "None selected"
  } else {
    paste(value, collapse = ", ")
  }
  
  div(
    class = paste(
      "sb-settings-item",
      if (isTRUE(wide)) "sb-settings-item-wide" else NULL
    ),
    span(class = "sb-settings-label", label),
    span(class = "sb-settings-value", value)
  )
}


settings_summary_ui <- function(settings, run_ok = TRUE) {
  title <- if (isTRUE(run_ok)) {
    "Settings used for the displayed results"
  } else {
    "Settings used for the attempted run"
  }
  
  div(
    class = "sb-settings-summary",
    
    div(class = "sb-settings-summary-title", title),
    
    div(
      class = "sb-settings-grid",
      
      settings_item("Assessment", settings$assessment),
      settings_item("Grade", settings$grade),
      settings_item("Comparison year", settings$comparison_year),
      settings_item("Minimum tested students", settings$min_students),
      settings_item("Neutral band", settings$neutral_band),
      
      settings_item("Baseline", settings$baseline, wide = TRUE),
      settings_item("Alt 1", settings$alt1, wide = TRUE),
      settings_item("Alt 2", settings$alt2, wide = TRUE),
      settings_item("Alt 3", settings$alt3, wide = TRUE)
    )
  )
}


# =========================================================
# Server
# =========================================================

server <- function(input, output, session) {
  
  # ---------------- scope input updates ---------------- #
  
  observeEvent(input$scope_1, {
    req(input$scope_1)
    
    scope_2_choices <- get_scope_2_choices(input$scope_1)
    req(length(scope_2_choices) > 0)
    
    freezeReactiveValue(input, "scope_2")
    
    updateSelectInput(
      session = session,
      inputId = "scope_2",
      choices = scope_2_choices,
      selected = unname(scope_2_choices[1])
    )
  }, ignoreInit = TRUE)
  
  
  observeEvent(list(input$scope_1, input$scope_2), {
    req(input$scope_1, input$scope_2)
    
    n_range <- get_range_n(input$scope_1, input$scope_2)
    min_n <- n_range$n_min
    max_n <- n_range$n_max
    req(length(min_n) > 0, length(max_n) > 0)
    
    freezeReactiveValue(input, "min_students")
    
    updateNumericInput(
      session = session,
      inputId = "min_students",
      label = paste0("Minimum tested students (", min_n, " to ", max_n, ")"),
      min = min_n,
      max = max_n,
      value = max(min_n, 10),
      step = 1
    )
    
    year_choices <- get_year_choices(input$scope_1, input$scope_2)
    req(length(year_choices) > 0)
    
    freezeReactiveValue(input, "selection_year")
    
    updateSelectInput(
      session = session,
      inputId = "selection_year",
      choices = year_choices,
      selected = unname(tail(year_choices, 1))
    )
  }, ignoreInit = TRUE)
  
  
  # ---------------- selected scope ---------------- #
  
  selected_model_key <- reactive({
    req(input$scope_1, input$scope_2)
    
    model_keys <- get_model_keys(input$scope_1, input$scope_2)
    
    validate(
      need(length(model_keys) >= 1, "No matching dataset was found for this scope."),
      need(
        length(model_keys) == 1,
        paste(
          "Assessment + grade does not uniquely identify one APP_DATA element.",
          "Add another scope filter or revise OPTIONS."
        )
      )
    )
    
    model_keys[1]
  })
  
  
  # ---------------- selected benchmark definitions ---------------- #
  
  model_toggles_reactive <- reactive({
    list(
      Baseline = build_toggle_vector(input$baseline_groups),
      `Alt 1`  = build_toggle_vector(input$alt1_groups),
      `Alt 2`  = build_toggle_vector(input$alt2_groups),
      `Alt 3`  = build_toggle_vector(input$alt3_groups)
    )
  })
  
  
  # ---------------- current settings snapshot ---------------- #
  
  make_settings_snapshot <- reactive({
    req(
      input$scope_1,
      input$scope_2,
      input$selection_year,
      input$min_students,
      input$neutral_band_multiplier
    )
    
    list(
      assessment = as.character(input$scope_1),
      grade = format_grade(input$scope_2),
      comparison_year = format_school_year(input$selection_year),
      min_students = as.integer(input$min_students),
      neutral_band = paste0(
        scales::number(
          as.numeric(input$neutral_band_multiplier),
          accuracy = 0.01
        ),
        " × weighted residual SD"
      ),
      baseline = label_selected_groups(input$baseline_groups),
      alt1 = label_selected_groups(input$alt1_groups),
      alt2 = label_selected_groups(input$alt2_groups),
      alt3 = label_selected_groups(input$alt3_groups)
    )
  })
  
  
  # ---------------- run benchmark models ---------------- #
  
  benchmark_results <- eventReactive(input$run_models, {
    req(selected_model_key(), input$selection_year, input$min_students)
    
    # Capture every run input once, before the long model process starts.
    # Downstream outputs use these stored values so they stay aligned with the run.
    run_params <- list(
      model_key = selected_model_key(),
      model_toggles = model_toggles_reactive(),
      selection_year = as.numeric(input$selection_year),
      min_students_tested = as.integer(input$min_students),
      neutral_band_multiplier = as.numeric(input$neutral_band_multiplier)
    )
    
    settings_snapshot <- make_settings_snapshot()
    
    withProgress(message = "Running benchmark models...", value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Fitting models")
        
        result <- run_benchmark_and_visuals(
          model_key = run_params$model_key,
          model_toggles = run_params$model_toggles,
          selection_year = run_params$selection_year,
          min_students_tested = run_params$min_students_tested,
          neutral_band_multiplier = run_params$neutral_band_multiplier
        )
        
        incProgress(0.8, detail = "Finalizing outputs")
        
        list(
          ok = TRUE,
          run_params = run_params,
          settings = settings_snapshot,
          run_result = result$run_result,
          viz = result$viz,
          error = NULL
        )
      }, error = function(e) {
        list(
          ok = FALSE,
          run_params = run_params,
          settings = settings_snapshot,
          run_result = NULL,
          viz = NULL,
          error = conditionMessage(e)
        )
      })
    })
  }, ignoreInit = TRUE)
  
  
  benchmark_checked <- reactive({
    result <- benchmark_results()
    req(result)
    
    validate(
      need(isTRUE(result$ok), result$error)
    )
    
    result
  })
  
  
  # ---------------- current settings summary ---------------- #
  
  output$current_settings_summary <- renderUI({
    result <- benchmark_results()
    req(result)
    
    settings_summary_ui(
      settings = result$settings,
      run_ok = isTRUE(result$ok)
    )
  })
  
  
  # ---------------- full-results tables ---------------- #
  
  model_comp_display <- reactive({
    result <- benchmark_checked()
    
    make_model_comp_table(
      dat = result$run_result$model_comp_table,
      format_for_display = TRUE
    )
  })
  
  
  model_comp_download <- reactive({
    result <- benchmark_checked()
    
    make_model_comp_table(
      dat = result$run_result$model_comp_table,
      format_for_display = FALSE
    )
  })
  
  
  # ---------------- gt outputs ---------------- #
  
  output$rank_summary_tbl <- render_gt({
    result <- benchmark_checked()
    
    tbl_rank_summary(result$viz)
  })
  
  
  output$year_coef_tbl <- render_gt({
    result <- benchmark_checked()
    
    make_year_coef_gt(
      run = result$run_result,
      year = result$run_params$selection_year
    )
  })
  
  
  # ---------------- plot outputs ---------------- #
  
  output$stability_summary <- renderPlot({
    result <- benchmark_checked()
    
    p_stability_summary(result$viz)
  }, res = res)
  
  output$rank_heat <- renderPlot({
    result <- benchmark_checked()
    
    p_rank_heat(result$viz)
  }, res = res)
  
  
  output$dumbbell_all <- renderPlot({
    result <- benchmark_checked()
    
    p_dumbbell_all(result$viz)
  }, res = res)
  
  
  output$rank_shift_summary <- renderPlot({
    result <- benchmark_checked()
    
    p_rank_shift_summary(result$viz)
  }, res = res)
  
  
  output$metric_facets <- renderPlot({
    result <- benchmark_checked()
    
    p_metric_facets(result$viz)
  }, res = res)
  
  
  # ---------------- full results table ---------------- #
  
  output$model_comp_dt <- renderDT({
    dat <- model_comp_display()
    
    left_cols <- match(
      c(
        "Benchmark model",
        "Benchmark definition",
        "County",
        "District name",
        "School name",
        "School code",
        "Assessment",
        "Grade"
      ),
      names(dat),
      nomatch = 0
    )
    
    center_cols <- match(
      c(
        "School year",
        "Within-year rank",
        "Benchmark label"
      ),
      names(dat),
      nomatch = 0
    )
    
    left_cols <- left_cols[left_cols > 0] - 1
    center_cols <- center_cols[center_cols > 0] - 1
    
    text_cols <- unique(c(left_cols, center_cols))
    right_cols <- setdiff(seq_along(dat) - 1, text_cols)
    
    datatable(
      dat,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      class = "compact stripe hover nowrap",
      extensions = c("FixedHeader", "FixedColumns"),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        autoWidth = FALSE,
        scrollX = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 1),
        order = list(),
        columnDefs = list(
          list(className = "dt-left", targets = left_cols),
          list(className = "dt-center", targets = center_cols),
          list(className = "dt-right", targets = right_cols)
        )
      )
    ) %>%
      formatStyle(
        columns = c("District name", "School name"),
        `white-space` = "normal"
      ) %>%
      formatStyle(
        "Benchmark label",
        target = "row",
        backgroundColor = styleEqual(
          c("Above benchmark", "Near benchmark", "Below benchmark"),
          c("#eef7ee", "#fffdf2", "#fdeeee")
        )
      )
  }, server = FALSE)
  
  
  # ---------------- full results download ---------------- #
  
  output$download_model_comp_full <- downloadHandler(
    filename = function() {
      paste0("benchmark_comparison_full_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_model_comp_workbook(
        dat = model_comp_download(),
        file = file
      )
    }
  )
}
