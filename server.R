# =========================================================
# server.R
# Server logic for Model-Based Benchmarking
# =========================================================

# Plot resolution
res <- 85

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
      value = min(max(10, min_n), max_n),
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
      input$comparison_band_index,
      input$min_students,
      input$neutral_band_multiplier
    )
    
    list(
      assessment = as.character(input$scope_1),
      grade = format_grade(input$scope_2),
      comparison_year = format_school_year(input$selection_year),
      tracked_rank_bands = format_comparison_band(input$comparison_band_index),
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
    req(selected_model_key(), input$selection_year, input$comparison_band_index, input$min_students)
    
    # Capture every run input once, before the long model process starts.
    # Downstream outputs use these stored values so they stay aligned with the run.
    run_params <- list(
      model_key = selected_model_key(),
      model_toggles = model_toggles_reactive(),
      selection_year = as.numeric(input$selection_year),
      comparison_band_index = as.integer(input$comparison_band_index),
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
          comparison_band_index = run_params$comparison_band_index,
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
  
  
  benchmark_current_band <- reactive({
    result <- benchmark_checked()
    
    updated_run_result <- rebuild_outputs_for_comparison_band(
      run_result = result$run_result,
      comparison_band_index = as.integer(input$comparison_band_index)
    )
    
    updated_viz <- build_visual_data(
      model_comp_plot = updated_run_result$model_comp_plot,
      model_comp_plot_all = updated_run_result$model_comp_plot_all,
      baseline_model = updated_run_result$post_run_audit$baseline_model,
      score_all = updated_run_result$score_all,
      comparison_focus_lookup = updated_run_result$comparison_focus_lookup,
      comparison_lower_label = updated_run_result$post_run_audit$comparison_lower_label,
      comparison_upper_label = updated_run_result$post_run_audit$comparison_upper_label
    )
    
    result$run_result <- updated_run_result
    result$viz <- updated_viz
    
    result$settings$tracked_rank_bands <- format_comparison_band(
      input$comparison_band_index
    )
    
    result
  })
  
  
  # ---------------- current settings summary ---------------- #
  
  output$current_settings_summary <- renderUI({
    result <- benchmark_results()
    req(result)

    settings <- result$settings

    # If the run succeeded, allow the settings card to reflect the current
    # tracked-band dropdown even when the model itself has not been rerun.
    # If the run failed, keep the attempted-run settings intact.
    if (isTRUE(result$ok)) {
      settings$tracked_rank_bands <- format_comparison_band(
        input$comparison_band_index
      )
    }

    settings_summary_ui(
      settings = settings,
      run_ok = isTRUE(result$ok)
    )
  })
  
  
  # ---------------- full-results tables ---------------- #
  
  model_comp_display <- reactive({
    result <- benchmark_checked()

    # Keep percentages numeric for DT sorting/filtering.
    # Formatting is applied in renderDT with formatPercentage().
    make_model_comp_table(
      dat = result$run_result$model_comp_table,
      format_for_display = FALSE
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
    result <- benchmark_current_band()
    
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
    result <- benchmark_current_band()
    
    p_rank_heat(result$viz)
  }, res = res)
  
  
  output$dumbbell_all <- renderPlot({
    result <- benchmark_current_band()
    
    p_dumbbell_all(result$viz)
  }, res = res)
  
  
  output$rank_shift_summary <- renderPlot({
    result <- benchmark_checked()
    
    p_rank_shift_summary(result$viz)
  }, res = res)
  
  
  output$metric_facets <- renderPlot({
    result <- benchmark_current_band()
    
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
    
    pct_display_cols <- c(
      "Percent missing scores",
      "ELL", "ELM", "ELX",
      "Foster care", "Male", "Wilmington", "Homeless", "Immersion",
      "Low income", "Migrant", "Military connected",
      "African American", "American Indian", "Asian", "Hawaiian",
      "Hispanic / Latino", "Multi-racial",
      "SPED 100", "SPED 200", "SPED 300", "SPED 400", "SPED 500",
      "SPED 601", "SPED 602", "SPED 700", "SPED 800", "SPED 900",
      "SPED 1000", "SPED 1100", "SPED 1200", "SPED 1300", "SPED 1400"
    )
    
    pct_display_cols <- intersect(pct_display_cols, names(dat))
    name_wrap_cols <- intersect(c("District name", "School name"), names(dat))
    
    datatable(
      dat,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      class = "compact stripe hover",
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        autoWidth = FALSE,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        order = list(),
        columnDefs = list(
          list(className = "dt-left", targets = left_cols),
          list(className = "dt-center", targets = center_cols),
          list(className = "dt-right", targets = right_cols)
        ),
        initComplete = JS("
      function(settings, json) {
        var api = this.api();

        function adjustModelCompTable() {
          window.requestAnimationFrame(function() {
            api.columns.adjust();

            setTimeout(function() {
              api.columns.adjust();
            }, 100);

            setTimeout(function() {
              api.columns.adjust();
            }, 300);

            setTimeout(function() {
              api.columns.adjust();
            }, 700);
          });
        }

        adjustModelCompTable();

        $(document)
          .off('shown.bs.tab.modelCompDt')
          .on('shown.bs.tab.modelCompDt', 'a[data-toggle=\"tab\"]', function() {
            adjustModelCompTable();
          });

        $(document)
          .off('shiny:value.modelCompDt shiny:bound.modelCompDt')
          .on('shiny:value.modelCompDt shiny:bound.modelCompDt', function() {
            adjustModelCompTable();
          });

        $(window)
          .off('resize.modelCompDt')
          .on('resize.modelCompDt', function() {
            adjustModelCompTable();
          });
      }
    ")
      )
    ) %>%
      formatStyle(
        columns = name_wrap_cols,
        `white-space` = "normal"
      ) %>%
      formatStyle(
        "District name",
        `min-width` = "180px"
      ) %>%
      formatStyle(
        "School name",
        `min-width` = "220px"
      ) %>%
      formatStyle(
        "Benchmark label",
        target = "row",
        backgroundColor = styleEqual(
          c("Above benchmark", "Near benchmark", "Below benchmark"),
          c("#eef7ee", "#fffdf2", "#fdeeee")
        )
      ) %>%
      formatPercentage(
        columns = pct_display_cols,
        digits = 1
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
