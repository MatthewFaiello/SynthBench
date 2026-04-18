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
  neutral_band_multiplier = 1 / 3,
  comparison_top_n = 10
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
  ell          = FALSE,
  foster_care  = FALSE,
  gender       = FALSE,
  geography    = FALSE,
  homeless     = FALSE,
  immersion    = FALSE,
  low_income   = FALSE,
  migrant      = FALSE,
  military_dep = FALSE,
  race         = FALSE,
  sped         = FALSE
)

# Labels shown in the UI
GROUP_CHOICES <- c(
  "Percent eligible students w/ missing scores" = "na",
  "Student Unit Count"                          = "units",
  "County"                                      = "county",
  "ELL"                                         = "ell",
  "Foster care"                                 = "foster_care",
  "Gender"                                      = "gender",
  "Wilmington"                                  = "geography",
  "Homelessness"                                = "homeless",
  "Immersion"                                   = "immersion",
  "Low income"                                  = "low_income",
  "Migrant"                                     = "migrant",
  "Military-connected"                          = "military_dep",
  "Race"                                        = "race",
  "SPED Code"                                   = "sped"
)

# Default model definitions shown when the app first opens
DEFAULT_MODEL_SELECTIONS <- list(
  Baseline    = c("low_income"),
  `Alt 1`     = c("low_income", "ell"),
  `Alt 2`     = c("low_income", "sped"),
  `Alt 3`     = c("low_income", "ell", "sped")
)
