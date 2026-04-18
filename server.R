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
      mutate(
        model = as.character(model),
        SchoolYear = as.character(SchoolYear),
        SchoolCode = as.character(SchoolCode),
        n = as.integer(n),
        ScaleScore.mean = round(ScaleScore.mean, 1),
        .pred_cv = round(.pred_cv, 1),
        .resid_cv = round(.resid_cv, 1),
        .performance_z_cv = round(.performance_z_cv, 2),
        .performance_rank_cv = as.integer(.performance_rank_cv),
        .performance_direction = dplyr::recode(
          .performance_direction,
          above_expected = "Above expected",
          near_expected  = "Near expected",
          below_expected = "Below expected"
        )
      )
  })
  
  output$model_comp_dt <- renderDT({
    req(benchmark_results())
    validate(need(benchmark_ok(), benchmark_results()$error))
    
    datatable(
      model_comp_display() %>% arrange(DistrictName, SchoolName),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      class = "compact stripe hover nowrap",
      extensions = c("FixedHeader", "FixedColumns"),
      colnames = c(
        "Model", 
        "Benchmark definition", 
        "Year", 
        "LEA", 
        "School", 
        "School code",
        "Grade", 
        "Observed score", 
        "Tested n", 
        "Benchmark score", 
        "Residual",
        "Scaled residual", 
        "Within-year rank", "Direction"
      ),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        autoWidth = FALSE,
        scrollX = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 1),
        order = list(),
        columnDefs = list(
          list(className = "dt-left",   targets = c(0, 1, 3, 4, 5, 6, 13)),
          list(className = "dt-center", targets = 2),
          list(className = "dt-right",  targets = c(7, 8, 9, 10, 11, 12))
        )
      )
    ) %>%
      formatStyle(
        columns = c("DistrictName", "SchoolName"),
        `white-space` = "normal"
      ) %>%
      formatStyle(
        ".performance_direction",
        target = "row",
        backgroundColor = styleEqual(
          c("Above expected", "Near expected", "Below expected"),
          c("#eef7ee", "#fffdf2", "#fdeeee")
        )
      )
  }, server = FALSE)
  
  output$download_model_comp_full <- downloadHandler(
    filename = function() {
      paste0("benchmark_comparison_full_", Sys.Date(), ".csv")
    },
    content = function(file) {
      validate(need(benchmark_ok(), benchmark_results()$error))
      readr::write_csv(model_comp_display(), file, na = "")
    }
  )
  
}