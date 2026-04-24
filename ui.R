# Default choices used when the app first opens
default_scope_1 <- SCOPE_1_CHOICES[1]
default_scope_2_choices <- get_scope_2_choices(default_scope_1)
default_scope_2 <- unname(default_scope_2_choices[1])
default_year_choices <- get_year_choices(default_scope_1, default_scope_2)
default_year <- unname(tail(default_year_choices, 1))
default_n <- get_range_n(default_scope_1, default_scope_2)
default_n_min <- max(default_n$n_min, 10)
default_n_max <- default_n$n_max
default_neutral_band <- SETTINGS$neutral_band_multiplier

sb_callout <- function(title, body, tone = c("info", "guide")) {
  tone <- match.arg(tone)
  
  div(
    class = paste("sb-callout", paste0("sb-callout--", tone)),
    div(class = "sb-callout-title", title),
    p(class = "sb-callout-body", body)
  )
}

sb_sidebar_section <- function(step, title, note = NULL, ...) {
  div(
    class = "sb-card sb-sidebar-section",
    div(class = "sb-step", paste("Step", step)),
    h2(class = "sb-section-title", title),
    if (!is.null(note)) p(class = "sb-section-note", note),
    ...
  )
}

sb_result_tab <- function(title, output_ui, caption) {
  tabPanel(
    title,
    div(
      class = "sb-result-panel",
      output_ui,
      p(class = "sb-result-caption", caption)
    )
  )
}

# Plain-language copy used across the interface
header_subtitle <- paste(
  "Build a historical benchmark for each school-year,",
  "then see how the results change when you change the comparison frame."
)

header_note <- paste(
  "This tool is for transparent benchmark-adjusted comparison.",
  "It is meant to show whether conclusions stay similar across reasonable comparison frames, not to find one final adjustment rule.",
  "It does not explain what caused results, estimate policy effects, or serve as a stand-alone accountability judgment."
)

sidebar_intro <- paste(
  "Pick the historical dataset and the comparison settings here.",
  "The app fits benchmark models on the full selected assessment-and-grade dataset that meets the student-count threshold,",
  "builds benchmark scores from held-out predictions,",
  "and then shows comparison views for the year you choose."
)

step1_note <- paste(
  "Assessment and grade determine which historical dataset is used to train the model.",
  "The minimum tested-student threshold is applied before the model is trained."
)

min_students_note <- paste(
  "School-years below this threshold are excluded before model fitting.",
  "Higher thresholds usually make the benchmark more stable, but they also reduce coverage."
)

step2_note <- paste(
  "These settings help define the benchmark.",
  "They control which year is highlighted in the displays",
  "and how large a benchmark gap has to be before it is labeled above, near, or below benchmark."
)

year_callout <- paste(
  "The year you pick here only controls which year is highlighted in the plots and tables.",
  "It does not change the full historical sample used to train the benchmark model."
)

year_note <- paste(
  "Use this to focus the displays on one year after the benchmark models are trained",
  "on the full selected historical dataset."
)

neutral_band_note <- paste(
  "The neutral band is a practical reading aid, not a significance test.",
  "A wider band treats more benchmark gaps as near benchmark and fewer as above or below benchmark.",
  "Its width is based on the residual standard deviation, not the observed-score standard deviation."
)

model_subtitle <- paste(
  "Each model uses school-year average scale score as the outcome and a different set of comparison features.",
  "The app builds a historical benchmark from the selected data and comparison frame,",
  "then compares the observed score with the benchmark prediction."
)

model_method_callout <- paste(
  "The boxes you check here define the model features and the comparison frame.",
  "For each model, the outcome is the school-year average score.",
  "The app builds the benchmark using school year, the benchmark features you choose, and year-specific adjustments.",
  "Those features define the comparison being made; they are not being treated as the true causes of performance.",
  "It uses repeated grouped cross-validation, so each school's benchmark is based on held-out school histories rather than the same rows used to fit that run."
)

results_subtitle <- paste(
  "Review how results change across benchmark definitions in the selected year.",
  "Most tabs follow the tracked baseline schools, while the full results tab includes all schools."
)

results_guide <- paste(
  "First, identify the comparison frame from the selected scope and benchmark groups.",
  "Second, compare the observed score, benchmark score, and difference from benchmark (residual).",
  "Third, use the within-year rank, scaled benchmark gap, and direction label to judge whether the interpretation changes across model definitions."
)

results_placeholder_title <- "Ready to run the benchmark models"
results_placeholder_body <- paste(
  "Choose the historical dataset, comparison rules, and model feature groups, then click 'Run benchmark models'.",
  "The comparison plots and tables will appear here after the models finish running."
)

ui <- fluidPage(
  tags$head(
    tags$title("Model-Based Benchmarking"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  div(
    class = "sb-page",
    
    div(
      class = "container-fluid sb-shell",
      
      div(
        class = "sb-header sb-card",
        div(class = "sb-eyebrow", "Historical assessment benchmarking tool"),
        h1(class = "sb-title", "Model-Based Benchmarking"),
        p(class = "sb-subtitle", header_subtitle),
        p(class = "sb-header-note", header_note)
      ),
      
      div(
        class = "row sb-layout",
        
        div(
          class = "col-sm-12 col-lg-3 col-xl-3 sb-sidebar-col",
          
          div(
            class = "sb-sidebar-stack",
            
            div(
              class = "sb-card sb-sidebar-intro",
              h2(class = "sb-sidebar-intro-title", "Set up the comparison"),
              p(class = "sb-sidebar-intro-copy", sidebar_intro)
            ),
            
            sb_sidebar_section(
              step = 1,
              title = "Choose the historical dataset",
              note = step1_note,
              
              div(
                class = "sb-control-block",
                selectInput(
                  inputId = "scope_1",
                  label = "Assessment",
                  choices = SCOPE_1_CHOICES,
                  selected = default_scope_1
                )
              ),
              
              div(
                class = "sb-control-block",
                selectInput(
                  inputId = "scope_2",
                  label = "Grade",
                  choices = default_scope_2_choices,
                  selected = default_scope_2
                )
              ),
              
              div(
                class = "sb-control-block",
                numericInput(
                  inputId = "min_students",
                  label = paste0(
                    "Minimum tested students (",
                    default_n_min,
                    " to ",
                    default_n_max,
                    ")"
                  ),
                  value = default_n_min,
                  min = default_n_min,
                  max = default_n_max,
                  step = 1
                ),
                p(class = "sb-control-note", min_students_note)
              )
            ),
            
            sb_sidebar_section(
              step = 2,
              title = "Set comparison rules",
              note = step2_note,
              
              sb_callout(
                title = "All eligible years stay in the training data.",
                body = year_callout,
                tone = "guide"
              ),
              
              div(
                class = "sb-control-block",
                selectInput(
                  inputId = "selection_year",
                  label = "Year shown in the comparison views",
                  choices = default_year_choices,
                  selected = default_year
                ),
                p(class = "sb-control-note", year_note)
              ),
              
              div(
                class = "sb-control-block",
                sliderInput(
                  inputId = "neutral_band_multiplier",
                  label = "Neutral-band width (fraction of residual SD)",
                  min = 0,
                  max = 1,
                  value = default_neutral_band,
                  step = 0.05
                ),
                p(class = "sb-control-note", neutral_band_note)
              )
            )
          )
        ),
        
        div(
          class = "col-sm-12 col-lg-9 col-xl-9 sb-main-col",
          
          div(
            class = "sb-main-stack",
            
            div(
              class = "sb-card sb-model-card",
              
              div(
                class = "sb-model-card-header",
                div(
                  class = "sb-model-card-copy",
                  div(class = "sb-step", "Step 3"),
                  h2(class = "sb-main-title", "Define benchmark models"),
                  p(class = "sb-main-subtitle", model_subtitle)
                ),
                div(
                  class = "sb-model-card-actions",
                  actionButton(
                    inputId = "run_models",
                    label = "Run benchmark models",
                    class = "sb-run-btn"
                  )
                )
              ),
              
              div(
                class = "sb-model-card-body",
                
                sb_callout(
                  title = "How these models work",
                  body = model_method_callout,
                  tone = "guide"
                ),
                
                div(
                  class = "sb-model-tabs",
                  div(
                    class = "sb-model-tabs-note",
                    "Baseline is the main reference model. Alt 1 to Alt 3 show whether conclusions move when the benchmark definition changes."
                  ),
                  div(
                    class = "sb-model-tabs-note sb-model-tabs-note--detail",
                    paste(
                      "Each checkbox turns on a group of related model inputs.",
                      "Depending on the group, that may mean one yes-or-no input, several category indicators,",
                      "or a single continuous measure, such as percent missing or student units."
                    )
                  ),
                  tabsetPanel(
                    id = "model_tabs",
                    
                    tabPanel(
                      "Baseline",
                      checkboxGroupInput(
                        inputId = "baseline_groups",
                        label = NULL,
                        choices = GROUP_CHOICES,
                        selected = DEFAULT_MODEL_SELECTIONS$Baseline,
                        inline = TRUE
                      )
                    ),
                    
                    tabPanel(
                      "Alt 1",
                      checkboxGroupInput(
                        inputId = "alt1_groups",
                        label = NULL,
                        choices = GROUP_CHOICES,
                        selected = DEFAULT_MODEL_SELECTIONS$`Alt 1`,
                        inline = TRUE
                      )
                    ),
                    
                    tabPanel(
                      "Alt 2",
                      checkboxGroupInput(
                        inputId = "alt2_groups",
                        label = NULL,
                        choices = GROUP_CHOICES,
                        selected = DEFAULT_MODEL_SELECTIONS$`Alt 2`,
                        inline = TRUE
                      )
                    ),
                    
                    tabPanel(
                      "Alt 3",
                      checkboxGroupInput(
                        inputId = "alt3_groups",
                        label = NULL,
                        choices = GROUP_CHOICES,
                        selected = DEFAULT_MODEL_SELECTIONS$`Alt 3`,
                        inline = TRUE
                      )
                    )
                  )
                )
              )
            ),
            
            div(
              class = "sb-card sb-main-card",
              
              div(
                class = "sb-main-header",
                h2(class = "sb-main-title", "Benchmark comparison views"),
                p(class = "sb-main-subtitle", results_subtitle)
              ),
              
              div(
                class = "sb-main-guide",
                sb_callout(
                  title = "A simple way to read the results",
                  body = results_guide,
                  tone = "guide"
                )
              ),
              
              div(
                class = "sb-main-body",
                
                conditionalPanel(
                  condition = "input.run_models === 0",
                  div(
                    class = "sb-result-panel",
                    sb_callout(
                      title = results_placeholder_title,
                      body = results_placeholder_body,
                      tone = "guide"
                    ),
                    p(
                      class = "sb-result-caption",
                      "Nothing has been run yet. Results will appear here after the benchmark models are fit."
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.run_models > 0",
                  div(
                    class = "sb-results-tabs",
                    tabsetPanel(
                      id = "visual_tabs",
                      
                      sb_result_tab(
                        title = "Benchmarking overview",
                        output_ui = gt_output("rank_summary_tbl"),
                        caption = paste(
                          "Summarizes how the baseline set of tracked schools changes across benchmark definitions",
                          "in the selected year."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Coefficient influence",
                        output_ui = gt_output("year_coef_tbl"),
                        caption = paste(
                          "Shows year-specific standardized coefficient influence for the selected year.",
                          "Group rows summarize relative influence across benchmark groups,",
                          "and feature rows show the individual feature coefficients that contribute to those group summaries."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Rank comparison",
                        output_ui = plotOutput("dumbbell_all", height = "90vh"),
                        caption = paste(
                          "Compares each tracked school’s baseline rank with its rank under alternative benchmark definitions.",
                          "Smaller rank numbers indicate stronger benchmark-adjusted performance within year."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Rank movement",
                        output_ui = plotOutput("rank_heat", height = "90vh"),
                        caption = paste(
                          "Shows how each tracked baseline school’s within-year rank changes",
                          "across benchmark definitions."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Average rank movement",
                        output_ui = plotOutput("rank_shift_summary", height = "60vh"),
                        caption = paste(
                          "Shows the average amount of rank movement for the tracked baseline schools",
                          "across benchmark definitions."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Scores and benchmark gaps",
                        output_ui = plotOutput("metric_facets", height = "70vh"),
                        caption = paste(
                          "Compares benchmark predictions, differences from benchmark, scaled residuals, and within-year ranks",
                          "for the tracked baseline schools across benchmark definitions."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Full results",
                        output_ui = tagList(
                          div(
                            class = "sb-table-actions",
                            downloadButton(
                              outputId = "download_model_comp_full",
                              label = "Download full table",
                              class = "btn btn-default"
                            )
                          ),
                          DTOutput("model_comp_dt")
                        ),
                        caption = paste(
                          "Lists all schools in the selected year for the fitted benchmark models, including observed score,",
                          "benchmark score, difference from benchmark (residual), scaled residual, within-year rank, and direction label."
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)