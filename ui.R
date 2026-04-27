# =========================================================
# ui.R
# User interface for Model-Based Benchmarking
# =========================================================


# ---------------- default choices used when the app first opens ---------------- #

default_scope_1 <- SCOPE_1_CHOICES[1]
default_scope_2_choices <- get_scope_2_choices(default_scope_1)
default_scope_2 <- unname(default_scope_2_choices[1])

default_year_choices <- get_year_choices(default_scope_1, default_scope_2)
default_year <- unname(tail(default_year_choices, 1))

default_n <- get_range_n(default_scope_1, default_scope_2)
default_n_min <- default_n$n_min
default_n_max <- default_n$n_max
default_n_value <- min(max(10, default_n_min), default_n_max)

default_neutral_band <- SETTINGS$neutral_band_multiplier

default_comparison_band_index <- SETTINGS$comparison_band_index
comparison_band_choices <- setNames(
  seq_len(floor(SETTINGS$comparison_band_count / 2)),
  vapply(seq_len(floor(SETTINGS$comparison_band_count / 2)), function(i) {
    labels <- get_comparison_band_labels(
      band_index = i,
      band_count = SETTINGS$comparison_band_count
    )
    paste0(labels$lower_label, " vs ", labels$upper_label)
  }, character(1))
)


# ---------------- small UI helpers ---------------- #

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


# ---------------- plain-language copy used across the interface ---------------- #

header_subtitle <- paste(
  "Review school assessment performance against model-based benchmarks,",
  "then check whether conclusions stay stable as more school-year context is included."
)

header_note <- paste(
  "Use this tool for internal review, policy discussion, and benchmarking conversations.",
  "It helps compare school results with contextualized benchmarks; it does not identify causes, estimate policy effects, forecast future scores, or make accountability decisions."
)

sidebar_intro <- paste(
  "Choose the assessment results to review, the year to display,",
  "and how the benchmark should label school performance."
)

step1_note <- paste(
  "Select the assessment-grade results and the tested-student count."
)

min_students_note <- paste(
  "School-years below this count are excluded.",
  "Higher thresholds may make results more stable, but may remove more schools."
)

step2_note <- paste(
  "Choose the year to display and the band used to label results as above, near, or below benchmark."
)

year_callout <- paste(
  "The display year controls what appears in the charts and tables.",
  "The benchmark models still use all eligible historical years for the selected assessment and grade."
)

year_note <- paste(
  "Select the school year to show in the output views."
)

comparison_band_note <- paste(
  "Changes only the tracked-school plots.",
  "Summary views and the full table still use all schools in the selected year."
)

neutral_band_note <- paste(
  "A wider band labels more schools as near benchmark.",
  "The band is based on the model's typical benchmark gap; it is not a significance test."
)

model_subtitle <- paste(
  "Start with a simple benchmark, then compare what changes as more school-year context is added.",
  "Each model compares observed school scores with a model-based benchmark."
)

model_method_callout <- paste(
  "Checked groups define what context the benchmark accounts for.",
  "These choices support historical benchmarking and sensitivity review; they do not explain what caused school performance."
)

model_tabs_note <- paste(
  "The Baseline is the anchor for comparison.",
  "The default setup starts with low income, then tests what changes when ELL, SPED, or both are added."
)

model_tabs_detail <- paste(
  "This is a simple-first model comparison.",
  "Use the alternatives to ask whether school conclusions change when the benchmark accounts for additional context."
)

results_subtitle <- paste(
  "Compare benchmark gaps, labels, ranks, and rank movement for the selected year."
)

results_guide <- paste(
  "Start with the benchmark gap: observed score minus benchmark score.",
  "Then check whether schools stay above, near, or below benchmark as more context is added."
)

results_view_control_note <- paste(
  "Choose the result view and tracked Baseline rank bands.",
  "These controls update the displayed results but do not rerun the benchmark models."
)

result_view_note <- paste(
  "Choose the plot or table shown below."
)

results_placeholder_title <- "Ready to run the benchmark models"

results_placeholder_body <- paste(
  "Choose the assessment, grade, display year, tested-student threshold, and benchmark definitions.",
  "Then run the benchmark models to populate the views."
)


# ---------------- UI ---------------- #

ui <- fluidPage(
  tags$head(
    tags$title("Model-Based Benchmarking"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(HTML("
      (function() {
        function moveVisualTabNav() {
          var target = document.getElementById('visual_tab_nav_home');
          var nav = document.querySelector('#visual_tabs.nav-tabs') ||
            document.querySelector('.sb-results-content > .tabbable > ul.nav-tabs');

          if (!target || !nav || nav.parentElement === target) {
            return;
          }

          target.appendChild(nav);
        }

        function polishSettingsSummary() {
          var title = document.querySelector('.sb-settings-summary .sb-settings-summary-title');

          if (!title) {
            return;
          }

          title.textContent = 'Settings used for these results';

          var summary = title.closest('.sb-settings-summary');

          if (summary && !summary.querySelector('.sb-settings-summary-note')) {
            var note = document.createElement('p');
            note.className = 'sb-settings-summary-note';
            note.textContent = 'Captured when the benchmark models were run. Change setup inputs and rerun to update these settings.';
            title.insertAdjacentElement('afterend', note);
          }
        }

        function polishUi() {
          moveVisualTabNav();
          polishSettingsSummary();
        }

        document.addEventListener('DOMContentLoaded', function() {
          polishUi();
          window.setTimeout(polishUi, 0);
          window.setTimeout(polishUi, 250);
        });

        document.addEventListener('shiny:connected', polishUi);
        document.addEventListener('shiny:bound', polishUi);
        document.addEventListener('shiny:value', polishUi);
      })();
    "))
  ),
  
  div(
    class = "sb-page",
    
    div(
      class = "container-fluid sb-shell",
      
      # ---------------- header ---------------- #
      div(
        class = "sb-header sb-card",
        div(class = "sb-eyebrow", "Historical assessment benchmarking"),
        h1(class = "sb-title", "Model-Based Benchmarking"),
        p(class = "sb-subtitle", header_subtitle),
        p(class = "sb-header-note", header_note)
      ),
      
      div(
        class = "row sb-layout",
        
        # ---------------- sidebar ---------------- #
        div(
          class = "col-sm-12 col-lg-3 col-xl-3 sb-sidebar-col",
          
          div(
            class = "sb-sidebar-stack",
            
            div(
              class = "sb-card sb-sidebar-intro",
              h2(class = "sb-sidebar-intro-title", "Set up the benchmark review"),
              p(class = "sb-sidebar-intro-copy", sidebar_intro)
            ),
            
            sb_sidebar_section(
              step = 1,
              title = "Choose assessment and grade",
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
                  value = default_n_value,
                  min = default_n_min,
                  max = default_n_max,
                  step = 1
                ),
                p(class = "sb-control-note", min_students_note)
              )
            ),
            
            sb_sidebar_section(
              step = 2,
              title = "Set display and label rules",
              note = step2_note,
              
              sb_callout(
                title = "Display year does not refit the model.",
                body = year_callout,
                tone = "guide"
              ),
              
              div(
                class = "sb-control-block",
                selectInput(
                  inputId = "selection_year",
                  label = "Display year",
                  choices = default_year_choices,
                  selected = default_year
                ),
                p(class = "sb-control-note", year_note)
              ),
              
              div(
                class = "sb-control-block",
                sliderInput(
                  inputId = "neutral_band_multiplier",
                  label = "Near-benchmark band width",
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
        
        # ---------------- main content ---------------- #
        div(
          class = "col-sm-12 col-lg-9 col-xl-9 sb-main-col",
          
          div(
            class = "sb-main-stack",
            
            # ---------------- model definition card ---------------- #
            div(
              class = "sb-card sb-model-card",
              
              div(
                class = "sb-model-card-header",
                
                div(
                  class = "sb-model-card-copy",
                  div(class = "sb-step", "Step 3"),
                  h2(class = "sb-main-title", "Choose benchmark definitions"),
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
                  title = "How to interpret model choices",
                  body = model_method_callout,
                  tone = "guide"
                ),
                
                div(
                  class = "sb-model-tabs",
                  
                  div(
                    class = "sb-model-tabs-note",
                    model_tabs_note
                  ),
                  
                  div(
                    class = "sb-model-tabs-note sb-model-tabs-note--detail",
                    model_tabs_detail
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
            
            # ---------------- results card ---------------- #
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
                      "Results will appear here after the benchmark models are fit."
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.run_models > 0",
                  
                  div(
                    id = "settings_summary_slot",
                    uiOutput("current_settings_summary")
                  ),
                  
                  div(
                    class = "sb-card sb-view-controls",
                    div(class = "sb-step", "View control"),
                    h2(class = "sb-section-title", "Adjust displayed results"),
                    p(class = "sb-section-note", results_view_control_note),
                    
                    div(
                      class = "sb-view-control-grid",
                      
                      div(
                        class = "sb-control-block sb-result-view-control",
                        h3(class = "sb-control-heading", "Result view"),
                        p(class = "sb-control-note", result_view_note),
                        div(
                          id = "visual_tab_nav_home",
                          class = "sb-result-view-tabs"
                        )
                      ),
                      
                      div(
                        class = "sb-control-block sb-rank-band-control",
                        h3(class = "sb-control-heading", "Tracked Baseline rank bands"),
                        selectInput(
                          inputId = "comparison_band_index",
                          label = NULL,
                          choices = comparison_band_choices,
                          selected = default_comparison_band_index,
                          width = "100%"
                        ),
                        p(class = "sb-control-note", comparison_band_note)
                      )
                    )
                  ),
                  
                  div(
                    class = "sb-results-content",
                    
                    tabsetPanel(
                      id = "visual_tabs",
                      
                      sb_result_tab(
                        title = "Benchmarking overview",
                        output_ui = gt_output("rank_summary_tbl"),
                        caption = paste(
                          "Look for schools whose benchmark label, rank, or gap changes across models.",
                          "Large changes mean the conclusion depends more on the benchmark definition."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Benchmark feature weights",
                        output_ui = gt_output("year_coef_tbl"),
                        caption = paste(
                          "Look for which selected context groups have the largest role in the selected-year benchmark.",
                          "Use this as a model diagnostic, not as evidence of what caused school performance."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Stability check",
                        output_ui = plotOutput("stability_summary", height = "520px"),
                        caption = paste(
                          "Look for whether school results stay fairly stable when the benchmark is tested across different groups of schools.",
                          "More variation means individual school results should be discussed with more caution."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Rank comparison",
                        output_ui = plotOutput("dumbbell_all", height = "90vh"),
                        caption = paste(
                          "Look for schools that move up or down when additional context is included in the benchmark.",
                          "Lower rank numbers indicate stronger benchmark-adjusted performance."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Rank movement",
                        output_ui = plotOutput("rank_heat", height = "90vh"),
                        caption = paste(
                          "Look for the schools with the largest rank movement across benchmark definitions.",
                          "Large movement means the school looks different depending on what context is included."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Average rank movement",
                        output_ui = plotOutput("rank_shift_summary", height = "60vh"),
                        caption = paste(
                          "Look for which alternative benchmark changes overall school rankings the most compared with the Baseline.",
                          "This summary uses all schools in the selected year."
                        )
                      ),
                      
                      sb_result_tab(
                        title = "Scores and benchmark gaps",
                        output_ui = plotOutput("metric_facets", height = "70vh"),
                        caption = paste(
                          "Each line follows one school across the Baseline and Alternative models.",
                          "Use this view to see whether benchmark scores, gaps, scaled gaps, and ranks stay stable or shift as context is added."
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
                          DTOutput("model_comp_dt", width = "100%")
                        ),
                        caption = paste(
                          "Use this table to filter, sort, export, and review all school-level results for the selected year.",
                          "Benchmark labels are practical comparison labels, not significance tests."
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