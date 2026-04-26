# =========================================================
# R/labels.R
# Shared labels and formatting helpers
# =========================================================
# Purpose:
#   Keep app-wide display labels in one place so UI, tables,
#   downloads, and summaries use consistent language.
#
# Main objects/helpers:
#   - grade_labels
#   - group_labels
#   - format_grade()
#   - get_group_label()
#   - pretty_feature_label()
# =========================================================

grade_labels <- c(
  "03"    = "3rd",
  "04"    = "4th",
  "05"    = "5th",
  "06"    = "6th",
  "07"    = "7th",
  "08"    = "8th",
  "09_12" = "9th–12th",
  "11"    = "11th"
)


format_grade <- function(x) {
  out <- unname(grade_labels[as.character(x)])
  
  ifelse(
    is.na(out),
    as.character(x),
    out
  )
}

group_labels <- c(
  school_year  = "School year",
  na           = "Missing scores",
  units        = "Student count",
  county       = "County",
  ell          = "ELL",
  foster_care  = "Foster care",
  gender       = "Gender",
  geography    = "Wilmington",
  homeless     = "Homelessness",
  immersion    = "Immersion",
  low_income   = "Low income",
  migrant      = "Migrant",
  military_dep = "Military-connected",
  race         = "Race",
  sped         = "SPED",
  other        = "Other"
)


get_group_label <- function(group_key) {
  out <- unname(group_labels[group_key])
  ifelse(is.na(out), as.character(group_key), out)
}

pretty_feature_label <- function(x) {
  label_map <- c(
    "County_Name.New_Castle" = "New Castle County",
    "County_Name.Sussex" = "Sussex County",
    "na" = "Percent missing scores",
    "units" = "Student units",
    
    "ELL.ELL" = "ELL",
    "ELL.ELM" = "ELM",
    "ELL.ELX" = "ELX",
    
    "FosterCare.FOSTR" = "Foster care",
    "Gender.M" = "Male",
    "Geography.W" = "Wilmington",
    "Homeless.HOMLES" = "Homeless",
    "Immersion.IMM" = "Immersion",
    "LowIncome.LOWINC" = "Low income",
    "Migrant.MIGRNT" = "Migrant",
    "MilitaryDep.MILTRY" = "Military connected",
    
    "RaceReportTitle.African_American" = "African American",
    "RaceReportTitle.American_Indian" = "American Indian",
    "RaceReportTitle.Asian" = "Asian",
    "RaceReportTitle.Hawaiian" = "Hawaiian",
    "RaceReportTitle.Hispanic_Latino" = "Hispanic / Latino",
    "RaceReportTitle.Multi_Racial" = "Multi-racial",
    
    "SPEDCode.100" = "SPED 100",
    "SPEDCode.200" = "SPED 200",
    "SPEDCode.300" = "SPED 300",
    "SPEDCode.400" = "SPED 400",
    "SPEDCode.500" = "SPED 500",
    "SPEDCode.601" = "SPED 601",
    "SPEDCode.602" = "SPED 602",
    "SPEDCode.700" = "SPED 700",
    "SPEDCode.800" = "SPED 800",
    "SPEDCode.900" = "SPED 900",
    "SPEDCode.1000" = "SPED 1000",
    "SPEDCode.1100" = "SPED 1100",
    "SPEDCode.1200" = "SPED 1200",
    "SPEDCode.1300" = "SPED 1300",
    "SPEDCode.1400" = "SPED 1400"
  )
  
  dplyr::recode(x, !!!label_map, .default = x)
}