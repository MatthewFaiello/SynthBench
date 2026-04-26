# =========================================================
# R/result_table.R
# Full-results table and Excel export helpers
# =========================================================
# Purpose:
#   Convert workflow-level benchmark results into the user-facing
#   full-results table and downloadable Excel workbook.
#
# Main helpers:
#   - ensure_columns()
#   - make_model_comp_table()
#   - write_model_comp_workbook()
#
# Expected input:
#   run_result$model_comp_table from run_benchmark_workflow()
# =========================================================

ensure_columns <- function(dat, cols, fill = NA_real_) {
  missing_cols <- setdiff(cols, names(dat))
  
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      dat[[col]] <- fill
    }
  }
  
  dat
}


make_model_comp_table <- function(dat, format_for_display = TRUE) {
  pct <- function(x) {
    if (format_for_display) {
      scales::percent(x, accuracy = 0.1)
    } else {
      x
    }
  }
  
  required_display_cols <- c(
    "County_Name.New_Castle",
    "County_Name.Sussex",
    "ELL.ELL",
    "ELL.ELM",
    "ELL.ELX",
    "FosterCare.FOSTR",
    "Gender.M",
    "Geography.W",
    "Homeless.HOMLES",
    "Immersion.IMM",
    "LowIncome.LOWINC",
    "Migrant.MIGRNT",
    "MilitaryDep.MILTRY",
    "RaceReportTitle.African_American",
    "RaceReportTitle.American_Indian",
    "RaceReportTitle.Asian",
    "RaceReportTitle.Hawaiian",
    "RaceReportTitle.Hispanic_Latino",
    "RaceReportTitle.Multi_Racial",
    "SPEDCode.100",
    "SPEDCode.200",
    "SPEDCode.300",
    "SPEDCode.400",
    "SPEDCode.500",
    "SPEDCode.601",
    "SPEDCode.602",
    "SPEDCode.700",
    "SPEDCode.800",
    "SPEDCode.900",
    "SPEDCode.1000",
    "SPEDCode.1100",
    "SPEDCode.1200",
    "SPEDCode.1300",
    "SPEDCode.1400"
  )
  
  dat <- ensure_columns(dat, required_display_cols)
  
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