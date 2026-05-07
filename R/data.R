# =========================================================
# R/data.R
# Load flat app data and build UI lookup tables
# =========================================================

# This file is intentionally independent of SETTINGS.
# global.R sources data.R before config.R, so do not use SETTINGS here.


# ---------------- file paths ---------------- #

app_data_path <- file.path("input_data", "APP_DATA.rds")
lea_meta_path <- file.path("input_data", "LEA_META.rds")


# ---------------- input checks ---------------- #

if (!file.exists(app_data_path)) {
  stop(
    "Missing input file: ",
    app_data_path,
    ". Run prep/organize.R first.",
    call. = FALSE
  )
}

if (!file.exists(lea_meta_path)) {
  stop(
    "Missing input file: ",
    lea_meta_path,
    ". Run prep/organize.R first.",
    call. = FALSE
  )
}


# ---------------- load flat app data ---------------- #

APP_DATA_FLAT <- readRDS(app_data_path)

if (!is.data.frame(APP_DATA_FLAT)) {
  stop(
    "APP_DATA_flat.rds must contain a data.frame or tibble.",
    call. = FALSE
  )
}

# If an older flat file is missing model_key, create it here.
# Your updated organize.R should already create model_key, so this is a fallback.
if (!"model_key" %in% names(APP_DATA_FLAT)) {
  APP_DATA_FLAT <- APP_DATA_FLAT %>%
    mutate(
      model_key = paste0(
        "grade_",
        ModelGrade,
        "__",
        make.names(AssessmentLabel)
      ),
      .before = SchoolYear
    )
}

APP_DATA_FLAT <- APP_DATA_FLAT %>%
  mutate(
    model_key = as.character(model_key),
    SchoolYear = as.integer(SchoolYear),
    SchoolCode = as.character(SchoolCode),
    ModelGrade = as.character(ModelGrade),
    AssessmentLabel = as.character(AssessmentLabel)
  )


# ---------------- load flat school metadata ---------------- #

LEA_META <- readRDS(lea_meta_path)

if (!is.data.frame(LEA_META)) {
  stop(
    "LEA_META.rds must contain a data.frame or tibble.",
    call. = FALSE
  )
}

LEA_META <- LEA_META %>%
  mutate(
    SchoolYear = as.integer(SchoolYear),
    SchoolCode = as.character(SchoolCode),
    ModelGrade = as.character(ModelGrade),
    DistrictName = as.character(DistrictName),
    SchoolName = as.character(SchoolName)
  )


# ---------------- validate modeling contract ---------------- #

required_app_cols <- c(
  "model_key",
  "SchoolYear",
  "SchoolCode",
  "ModelGrade",
  "AssessmentLabel",
  "ScaleScore.mean",
  "n",
  "w_sqrt_n",
  "na",
  "units"
)

missing_app_cols <- setdiff(required_app_cols, names(APP_DATA_FLAT))

if (length(missing_app_cols) > 0) {
  stop(
    "APP_DATA_FLAT is missing required columns: ",
    paste(missing_app_cols, collapse = ", "),
    call. = FALSE
  )
}

duplicate_app_rows <- APP_DATA_FLAT %>%
  count(
    SchoolYear,
    SchoolCode,
    ModelGrade,
    AssessmentLabel,
    name = "row_count"
  ) %>%
  filter(row_count > 1)

if (nrow(duplicate_app_rows) > 0) {
  stop(
    "APP_DATA_FLAT has duplicate rows by SchoolYear, SchoolCode, ModelGrade, and AssessmentLabel.",
    call. = FALSE
  )
}

required_meta_cols <- c(
  "SchoolYear",
  "SchoolCode",
  "ModelGrade",
  "DistrictName",
  "SchoolName"
)

missing_meta_cols <- setdiff(required_meta_cols, names(LEA_META))

if (length(missing_meta_cols) > 0) {
  stop(
    "LEA_META is missing required columns: ",
    paste(missing_meta_cols, collapse = ", "),
    call. = FALSE
  )
}

duplicate_meta_rows <- LEA_META %>%
  count(
    SchoolYear,
    SchoolCode,
    ModelGrade,
    name = "row_count"
  ) %>%
  filter(row_count > 1)

if (nrow(duplicate_meta_rows) > 0) {
  stop(
    "LEA_META has duplicate rows by SchoolYear, SchoolCode, and ModelGrade.",
    call. = FALSE
  )
}


# ---------------- compatibility object for current workflow.R ---------------- #

# workflow.R currently expects APP_DATA to be a named list:
# APP_DATA[[model_key]]
#
# We build that list from the flat file for now.
# Later, we can update workflow.R to use APP_DATA_FLAT directly.

model_keys <- APP_DATA_FLAT %>%
  distinct(model_key) %>%
  arrange(model_key) %>%
  pull(model_key)

APP_DATA <- setNames(
  lapply(model_keys, function(key) {
    APP_DATA_FLAT %>%
      filter(model_key == key) %>%
      select(-model_key)
  }),
  model_keys
)


# ---------------- UI lookup table ---------------- #

OPTIONS <- APP_DATA_FLAT %>%
  group_by(model_key, AssessmentLabel, ModelGrade, SchoolYear) %>%
  summarise(
    n_min = suppressWarnings(min(n, na.rm = TRUE)),
    n_max = suppressWarnings(max(n, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  distinct(
    model_key,
    AssessmentLabel,
    ModelGrade,
    SchoolYear,
    n_min,
    n_max
  )

SCOPE_1_CHOICES <- OPTIONS %>%
  distinct(AssessmentLabel) %>%
  pull(AssessmentLabel) %>%
  sort()


# ---------------- small accessor for future workflow refactor ---------------- #

get_model_data <- function(model_key) {
  if (!model_key %in% names(APP_DATA)) {
    stop(
      "No data found for model_key: ",
      model_key,
      call. = FALSE
    )
  }
  
  APP_DATA[[model_key]]
}