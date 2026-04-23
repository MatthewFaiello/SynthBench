# plot resolution
res <- 85

server <- function(input, output, session) {
  
  # ---------------- update scope inputs when the user changes scope ---------------- #
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
  
  model_toggles_reactive <- reactive({
    list(
      Baseline = build_toggle_vector(input$baseline_groups),
      `Alt 1`  = build_toggle_vector(input$alt1_groups),
      `Alt 2`  = build_toggle_vector(input$alt2_groups),
      `Alt 3`  = build_toggle_vector(input$alt3_groups)
    )
  })
  
  benchmark_results <- eventReactive(input$run_models, {
    req(selected_model_key(), input$selection_year, input$min_students)
    
    withProgress(message = "Running benchmark models...", value = 0, {
      incProgress(0.2, detail = "Fitting models")
      
      out <- tryCatch({
        run_result <- run_benchmark_workflow(
          model_key = selected_model_key(),
          model_toggles = model_toggles_reactive(),
          selection_year = as.numeric(input$selection_year),
          min_students_tested = input$min_students,
          neutral_band_multiplier = input$neutral_band_multiplier
        )
        
        incProgress(0.8, detail = "Building visuals")
        
        viz <- build_visual_data(
          model_comp_plot = run_result$model_comp_plot,
          baseline_model = run_result$post_run_audit$baseline_model,
          score_all = run_result$score_all
        )
        
        list(
          ok = TRUE,
          run_result = run_result,
          viz = viz,
          error = NULL
        )
      }, error = function(e) {
        list(
          ok = FALSE,
          run_result = NULL,
          viz = NULL,
          error = conditionMessage(e)
        )
      })
      
      out
    })
  }, ignoreInit = TRUE)
  
  benchmark_ok <- reactive({
    req(benchmark_results())
    isTRUE(benchmark_results()$ok)
  })
  
  # ---------------- outputs ---------------- #
  output$rank_summary_tbl <- render_gt({
    req(benchmark_results())
    validate(need(benchmark_ok(), benchmark_results()$error))
    tbl_rank_summary(benchmark_results()$viz)
  })
  
  output$rank_heat <- renderPlot({
    req(benchmark_results())
    validate(need(benchmark_ok(), benchmark_results()$error))
    p_rank_heat(benchmark_results()$viz)
  }, res = res)
  
  output$dumbbell_all <- renderPlot({
    req(benchmark_results())
    validate(need(benchmark_ok(), benchmark_results()$error))
    p_dumbbell_all(benchmark_results()$viz)
  }, res = res)
  
  output$rank_shift_summary <- renderPlot({
    req(benchmark_results())
    validate(need(benchmark_ok(), benchmark_results()$error))
    p_rank_shift_summary(benchmark_results()$viz)
  }, res = res)
  
  output$metric_facets <- renderPlot({
    req(benchmark_results())
    validate(need(benchmark_ok(), benchmark_results()$error))
    p_metric_facets(benchmark_results()$viz)
  }, res = res)
  
  # Cleaned table used for both display and full download
  model_comp_display <- reactive({
    req(benchmark_results())
    validate(need(benchmark_ok(), benchmark_results()$error))
    
    benchmark_results()$run_result$model_comp_table %>%
      transmute(
        `Benchmark model` = as.character(model),
        `Benchmark definition` = as.character(formula_label),
        `School year` = as.character(SchoolYear),
        County = case_when(
          County_Name.New_Castle == 1 ~ "New Castle County",
          County_Name.Sussex == 1 ~ "Sussex County",
          .default = "Kent County"
        ),
        `District name` = as.character(DistrictName),
        `School name` = as.character(SchoolName),
        `School code` = as.character(SchoolCode),
        Grade = as.character(ModelGrade),
        
        `Tested students` = as.integer(n),
        `Observed score` = round(ScaleScore.mean, 1),
        `Benchmark-predicted score` = round(.pred_cv, 1),
        `Benchmark gap` = round(.resid_cv, 1),
        `Scaled residual` = round(.performance_z_cv, 2),
        `Within-year rank` = as.integer(.performance_rank_cv),
        `Direction label` = recode(
          .performance_direction,
          above_expected = "Above expected",
          near_expected  = "Near expected",
          below_expected = "Below expected"
        ),
        
        `Percent missing scores` = percent(na, accuracy = 0.1),
        `Student units` = round(units, 1),
        
        `ELL` = percent(ELL.ELL, accuracy = 0.1),
        `ELM` = percent(ELL.ELM, accuracy = 0.1),
        `ELX` = percent(ELL.ELX, accuracy = 0.1),
        
        `Foster care` = percent(FosterCare.FOSTR, accuracy = 0.1),
        Male = percent(Gender.M, accuracy = 0.1),
        Wilmington = percent(Geography.W, accuracy = 0.1),
        Homeless = percent(Homeless.HOMLES, accuracy = 0.1),
        Immersion = percent(Immersion.IMM, accuracy = 0.1),
        `Low income` = percent(LowIncome.LOWINC, accuracy = 0.1),
        Migrant = percent(Migrant.MIGRNT, accuracy = 0.1),
        `Military connected` = percent(MilitaryDep.MILTRY, accuracy = 0.1),
        
        `African American` = percent(RaceReportTitle.African_American, accuracy = 0.1),
        `American Indian` = percent(RaceReportTitle.American_Indian, accuracy = 0.1),
        Asian = percent(RaceReportTitle.Asian, accuracy = 0.1),
        Hawaiian = percent(RaceReportTitle.Hawaiian, accuracy = 0.1),
        `Hispanic / Latino` = percent(RaceReportTitle.Hispanic_Latino, accuracy = 0.1),
        `Multi-racial` = percent(RaceReportTitle.Multi_Racial, accuracy = 0.1),
        
        `SPED 100` = percent(SPEDCode.100, accuracy = 0.1),
        `SPED 200` = percent(SPEDCode.200, accuracy = 0.1),
        `SPED 300` = percent(SPEDCode.300, accuracy = 0.1),
        `SPED 400` = percent(SPEDCode.400, accuracy = 0.1),
        `SPED 500` = percent(SPEDCode.500, accuracy = 0.1),
        `SPED 601` = percent(SPEDCode.601, accuracy = 0.1),
        `SPED 602` = percent(SPEDCode.602, accuracy = 0.1),
        `SPED 700` = percent(SPEDCode.700, accuracy = 0.1),
        `SPED 800` = percent(SPEDCode.800, accuracy = 0.1),
        `SPED 900` = percent(SPEDCode.900, accuracy = 0.1),
        `SPED 1000` = percent(SPEDCode.1000, accuracy = 0.1),
        `SPED 1100` = percent(SPEDCode.1100, accuracy = 0.1),
        `SPED 1200` = percent(SPEDCode.1200, accuracy = 0.1),
        `SPED 1300` = percent(SPEDCode.1300, accuracy = 0.1),
        `SPED 1400` = percent(SPEDCode.1400, accuracy = 0.1)
      ) %>%
      arrange(`District name`, `School name`)
  })
  
  output$model_comp_dt <- renderDT({
    req(benchmark_results())
    validate(need(benchmark_ok(), benchmark_results()$error))
    
    datatable(
      model_comp_display(),
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
          list(className = "dt-left", targets = c(0, 1, 3, 4, 5, 6, 7)),
          list(className = "dt-center", targets = c(2, 13, 14)),
          list(className = "dt-right", targets = c(8, 9, 10, 11, 12, 15, 16:43))
        )
      )
    ) %>%
      formatStyle(
        columns = c("District name", "School name"),
        `white-space` = "normal"
      ) %>%
      formatStyle(
        "Direction label",
        target = "row",
        backgroundColor = styleEqual(
          c("Above expected", "Near expected", "Below expected"),
          c("#eef7ee", "#fffdf2", "#fdeeee")
        )
      )
  }, server = FALSE)
  
  output$download_model_comp_full <- downloadHandler(
    filename = function() {
      paste0("benchmark_comparison_full_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      validate(need(benchmark_ok(), benchmark_results()$error))
      
      dat <- benchmark_results()$run_result$model_comp_table %>%
        transmute(
          `Benchmark model` = as.character(model),
          `Benchmark definition` = as.character(formula_label),
          `School year` = as.character(SchoolYear),
          County = case_when(
            County_Name.New_Castle == 1 ~ "New Castle County",
            County_Name.Sussex == 1 ~ "Sussex County",
            .default = "Kent County"
          ),
          `District name` = as.character(DistrictName),
          `School name` = as.character(SchoolName),
          `School code` = as.character(SchoolCode),
          Grade = as.character(ModelGrade),
          
          `Tested students` = as.integer(n),
          `Observed score` = round(ScaleScore.mean, 1),
          `Benchmark-predicted score` = round(.pred_cv, 1),
          `Benchmark gap` = round(.resid_cv, 1),
          `Scaled residual` = round(.performance_z_cv, 2),
          `Within-year rank` = as.integer(.performance_rank_cv),
          `Direction label` = recode(
            .performance_direction,
            above_expected = "Above expected",
            near_expected  = "Near expected",
            below_expected = "Below expected"
          ),
          
          `Percent missing scores` = na,
          `Student units` = round(units, 1),
          
          `ELL` = ELL.ELL,
          `ELM` = ELL.ELM,
          `ELX` = ELL.ELX,
          
          `Foster care` = FosterCare.FOSTR,
          Male = Gender.M,
          Wilmington = Geography.W,
          Homeless = Homeless.HOMLES,
          Immersion = Immersion.IMM,
          `Low income` = LowIncome.LOWINC,
          Migrant = Migrant.MIGRNT,
          `Military connected` = MilitaryDep.MILTRY,
          
          `African American` = RaceReportTitle.African_American,
          `American Indian` = RaceReportTitle.American_Indian,
          Asian = RaceReportTitle.Asian,
          Hawaiian = RaceReportTitle.Hawaiian,
          `Hispanic / Latino` = RaceReportTitle.Hispanic_Latino,
          `Multi-racial` = RaceReportTitle.Multi_Racial,
          
          `SPED 100` = SPEDCode.100,
          `SPED 200` = SPEDCode.200,
          `SPED 300` = SPEDCode.300,
          `SPED 400` = SPEDCode.400,
          `SPED 500` = SPEDCode.500,
          `SPED 601` = SPEDCode.601,
          `SPED 602` = SPEDCode.602,
          `SPED 700` = SPEDCode.700,
          `SPED 800` = SPEDCode.800,
          `SPED 900` = SPEDCode.900,
          `SPED 1000` = SPEDCode.1000,
          `SPED 1100` = SPEDCode.1100,
          `SPED 1200` = SPEDCode.1200,
          `SPED 1300` = SPEDCode.1300,
          `SPED 1400` = SPEDCode.1400
        ) %>%
        arrange(`District name`, `School name`)
      
      wb <- createWorkbook()
      addWorksheet(wb, "Benchmark results", gridLines = TRUE)
      writeData(wb, "Benchmark results", dat)
      
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
        wb, "Benchmark results", header_style,
        rows = 1, cols = 1:ncol(dat), gridExpand = TRUE
      )
      
      addFilter(
        wb, "Benchmark results",
        row = 1, cols = 1:ncol(dat)
      )
      
      freezePane(
        wb, "Benchmark results",
        firstRow = TRUE
      )
      
      int_cols <- which(names(dat) %in% c("Tested students", "Within-year rank"))
      one_dec_cols <- which(names(dat) %in% c(
        "Observed score", "Benchmark-predicted score", "Benchmark gap", "Student units"
      ))
      two_dec_cols <- which(names(dat) %in% c("Scaled residual"))
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
      
      if (length(int_cols) > 0) {
        addStyle(
          wb, "Benchmark results", int_style,
          rows = 2:(nrow(dat) + 1), cols = int_cols,
          gridExpand = TRUE, stack = TRUE
        )
      }
      
      if (length(one_dec_cols) > 0) {
        addStyle(
          wb, "Benchmark results", one_dec_style,
          rows = 2:(nrow(dat) + 1), cols = one_dec_cols,
          gridExpand = TRUE, stack = TRUE
        )
      }
      
      if (length(two_dec_cols) > 0) {
        addStyle(
          wb, "Benchmark results", two_dec_style,
          rows = 2:(nrow(dat) + 1), cols = two_dec_cols,
          gridExpand = TRUE, stack = TRUE
        )
      }
      
      if (length(pct_cols) > 0) {
        addStyle(
          wb, "Benchmark results", pct_style,
          rows = 2:(nrow(dat) + 1), cols = pct_cols,
          gridExpand = TRUE, stack = TRUE
        )
      }
      
      setColWidths(
        wb, "Benchmark results",
        cols = 1:ncol(dat), widths = "auto"
      )
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}