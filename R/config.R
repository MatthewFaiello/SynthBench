# =========================================================
# R/config.R
# App settings and user-facing choices
# =========================================================

# Core modeling settings
SETTINGS <- list(
  outcome = "ScaleScore.mean",
  school_id = "SchoolCode",
  weight_var = "w_sqrt_n",
  mandatory_group = "school_year",
  lambda_choice = "lambda.1se",
  nfolds = 10,
  cv_repeats = 5,
  cv_seed_start = 1000,
  neutral_band_multiplier = 0.5,
  comparison_band_count = 10,
  comparison_band_index = 1,
  min_schools_for_cv = 5
)

# Each toggle can be TRUE or FALSE.
# TRUE means the group is included in the model.
# FALSE means the group is not included in the model.
#
# school_year is not listed here because it is always included.
DEFAULT_GROUP_TOGGLES <- c(
  na           = FALSE,
  units        = FALSE,
  county       = FALSE,
  geography    = FALSE,
  sped         = FALSE,
  ell          = FALSE,
  immersion    = FALSE,
  gender       = FALSE,
  race         = FALSE,
  low_income   = FALSE,
  foster_care  = FALSE,
  homeless     = FALSE,
  migrant      = FALSE,
  military_dep = FALSE
)

# Labels shown in the UI
GROUP_CHOICES <- c(
  "Grade size"                                  = "units",
  "County"                                      = "county",
  "Wilmington"                                  = "geography",
  "SPED Code"                                   = "sped",
  "ELL"                                         = "ell",
  "Immersion"                                   = "immersion",
  "Gender"                                      = "gender",
  "Race"                                        = "race",
  "Low income"                                  = "low_income",
  "Foster care"                                 = "foster_care",
  "Homelessness"                                = "homeless",
  "Migrant"                                     = "migrant",
  "Military-connected"                          = "military_dep"
)

# Column patterns used to find model features in APP_DATA_FLAT.
#
# These names should align with:
# - SETTINGS$mandatory_group
# - DEFAULT_GROUP_TOGGLES
# - GROUP_CHOICES values
#
# Use regular expressions so exact columns like "na" and "units"
# do not accidentally match unrelated columns.
FEATURE_GROUP_PATTERNS <- c(
  school_year  = "^SchoolYear\\.",
  na           = "^na$",
  units        = "^units$",
  county       = "^County_Name\\.",
  geography    = "^Geography\\.",
  sped         = "^SPEDCode\\.",
  ell          = "^ELL\\.",
  immersion    = "^Immersion\\.",
  gender       = "^Gender\\.",
  race         = "^RaceReportTitle\\.",
  low_income   = "^LowIncome\\.",
  foster_care  = "^FosterCare\\.",
  homeless     = "^Homeless\\.",
  migrant      = "^Migrant\\.",
  military_dep = "^MilitaryDep\\."
)

# Default model definitions shown when the app first opens
DEFAULT_MODEL_SELECTIONS <- list(
  Baseline    = c("low_income"),
  `Alt 1`     = c("low_income", "ell"),
  `Alt 2`     = c("low_income", "sped"),
  `Alt 3`     = c("low_income", "ell", "sped")
)
