# =========================================================
# organize.R
# Build the input_data files used by the Shiny app
#
# Run this script from the project root.
# =========================================================

library(tidyverse)
library(readxl)
library(fastDummies)

# ---------------- file paths ---------------- #
data_dir <- file.path("prep", "data")
output_dir <- "input_data"

# ---------------- load raw files ---------------- #
assessment_raw <- read_csv(
  file.path(data_dir, "assessments.csv"),
  na = c("", "NA", "NULL")
)

unit_counts_raw <- read_csv(
  file.path(data_dir, "unitCount.csv"),
  na = c("", "NA", "NULL")
)

# ---------------- constants ---------------- #
grades_keep <- c(sprintf("%02d", 3:9), "10", "11", "12")
bio_grades <- c("09", "10", "11", "12")

assessment_lookup <- tribble(
  ~AssessmentName, ~ContentArea, ~AssessmentLabel,
  "SBAC",  "ELA",   "DeSSA ELA",
  "SBAC",  "MATH",  "DeSSA Math",
  "SAT11", "ELA",   "SAT ELA",
  "SAT11", "MATH",  "SAT Math",
  "SAT11", "ESSAY", "SAT Essay",
  "DESSA", "SOC",   "DeSSA Social Studies",
  "DESSA", "SCI",   "DeSSA Science"
)

# =========================================================
# 1) student unit count data
# =========================================================

unit_base <- unit_counts_raw %>%
  mutate(
    SchoolYear = as.integer(SchoolYear),
    Grade = str_pad(as.character(Grade), width = 2, pad = "0")
  ) %>%
  filter(
    SchoolYear >= 2015 & SchoolYear <= 2025,
    Grade %in% grades_keep
  ) %>%
  select(
    StudentID,
    SchoolYear,
    DistrictName,
    SchoolName,
    SchoolCode,
    Grade,
    County_Name,
    Geography,
    Gender,
    RaceReportTitle,
    LowIncome,
    SPEDCode,
    SWD,
    ELL,
    Migrant,
    Homeless,
    FosterCare,
    Immersion,
    MilitaryDep
  )

unit_grade <- unit_base %>%
  select(
    StudentID,
    SchoolYear,
    SchoolCode,
    ModelGrade = Grade,
    County_Name,
    Geography,
    Gender,
    RaceReportTitle,
    LowIncome,
    SPEDCode,
    SWD,
    ELL,
    Migrant,
    Homeless,
    FosterCare,
    Immersion,
    MilitaryDep
  )

unit_bio <- unit_base %>%
  filter(Grade %in% bio_grades) %>%
  transmute(
    StudentID,
    SchoolYear,
    SchoolCode,
    ModelGrade = "09_12",
    County_Name,
    Geography,
    Gender,
    RaceReportTitle,
    LowIncome,
    SPEDCode,
    SWD,
    ELL,
    Migrant,
    Homeless,
    FosterCare,
    Immersion,
    MilitaryDep
  )

unit_model <- bind_rows(unit_grade, unit_bio)

school_year_features <- unit_model %>%
  group_by(SchoolYear, SchoolCode, ModelGrade) %>%
  mutate(units = n()) %>%
  ungroup() %>%
  mutate(across(County_Name:MilitaryDep, ~ factor(.))) %>%
  pivot_longer(
    cols = -c(StudentID, SchoolYear, SchoolCode, ModelGrade, units),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    dummy = 1,
    name = paste(variable, value, sep = ".")
  ) %>%
  select(-c(variable, value)) %>%
  arrange(name) %>%
  pivot_wider(
    names_from = name,
    values_from = dummy,
    values_fill = 0
  ) %>%
  group_by(SchoolYear, SchoolCode, ModelGrade, units) %>%
  summarise(
    across(County_Name.Kent:SWD.SWD, mean),
    .groups = "drop"
  )

# School metadata for the app
schools_grade <- unit_base %>%
  distinct(SchoolYear, DistrictName, SchoolCode, SchoolName, Grade) %>%
  rename(ModelGrade = Grade)

schools_bio <- unit_base %>%
  filter(Grade %in% bio_grades) %>%
  distinct(SchoolYear, DistrictName, SchoolCode, SchoolName) %>%
  mutate(ModelGrade = "09_12")

schools_meta <- bind_rows(schools_grade, schools_bio) %>%
  distinct(SchoolYear, DistrictName, SchoolName, SchoolCode, ModelGrade) %>% 
  mutate(SchoolName = case_when(SchoolCode == 295 ~ "Charter School of Wilmington",
                                SchoolCode == 565 ~ "Delaware College Preparatory Academy",
                                SchoolCode == 578 ~ "Delaware Military Academy",
                                .default = SchoolName))

# =========================================================
# 2) assessment target data
# =========================================================

assessment_labeled <- assessment_raw %>%
  mutate(
    SchoolYear = as.integer(SchoolYear),
    Grade = str_pad(as.character(Grade), width = 2, pad = "0"),
    AssessmentName = str_to_upper(AssessmentName),
    ContentArea = str_to_upper(ContentArea)
  ) %>%
  left_join(
    assessment_lookup,
    by = c("AssessmentName", "ContentArea")
  ) %>%
  mutate(
    ModelGrade = case_when(
      AssessmentLabel == "DeSSA Science" & Grade %in% bio_grades ~ "09_12",
      TRUE ~ Grade
    ),
    AssessmentLabel = case_when(
      AssessmentLabel == "DeSSA Science" & Grade %in% bio_grades ~ "DeSSA Science - Bio",
      TRUE ~ AssessmentLabel
    )
  )

current_assessments <- assessment_labeled %>%
  filter(
    SchoolYear == 2025,
    !is.na(AssessmentLabel)
  ) %>%
  distinct(ModelGrade, AssessmentLabel)

assessment_school_year <- assessment_labeled %>%
  semi_join(
    current_assessments,
    by = c("ModelGrade", "AssessmentLabel")
  ) %>%
  group_by(
    SchoolYear,
    SchoolCode,
    ModelGrade,
    AssessmentLabel
  ) %>%
  summarise(
    ScaleScore.mean = mean(ScaleScore, na.rm = TRUE),
    n = sum(!is.na(ScaleScore)),
    na = sum(is.na(ScaleScore)),
    students = n_distinct(StudentID),
    .groups = "drop"
  ) %>%
  filter(n > 0) %>%
  mutate(
    check = (n + na) == students,
    na = na / (n + na)
  ) %>%
  select(-students, -check)

# =========================================================
# 3) training matrix for the app
# =========================================================

drop_cols <- c(
  "County_Name.Kent",
  "ELL.N_ELL",
  "FosterCare.N_FC",
  "Gender.F",
  "Geography.N_WILM",
  "Homeless.N_H",
  "Immersion.N_IMM",
  "LowIncome.N_LI",
  "Migrant.N_MIG",
  "MilitaryDep.N_MILI",
  "RaceReportTitle.White",
  "SPEDCode.0",
  "SWD.SWD",
  "SWD.N_SWD"
)

training_base <- school_year_features %>%
  inner_join(
    assessment_school_year,
    by = c("SchoolYear", "SchoolCode", "ModelGrade")
  ) %>%
  select(
    SchoolYear,
    SchoolCode,
    ModelGrade,
    AssessmentLabel,
    ScaleScore.mean,
    n,
    na,
    units,
    County_Name.Kent:SWD.SWD
  )

training_matrix <- training_base %>%
  dummy_cols(
    "SchoolYear",
    remove_first_dummy = TRUE,
    remove_selected_columns = FALSE
  ) %>%
  rename_with(~ str_replace(.x, "_", "."), starts_with("SchoolYear")) %>%
  select(-any_of(drop_cols)) %>%
  rename_with(~ gsub("[[:space:]/-]+", "_", .x)) %>%
  mutate(
    w_sqrt_n = sqrt(n),
    .before = na
  )

# =========================================================
# 4) split into one dataset per model
# =========================================================

training_list <- training_matrix %>%
  group_split(ModelGrade, AssessmentLabel, .keep = TRUE)

training_list_names <- training_matrix %>%
  distinct(ModelGrade, AssessmentLabel) %>%
  arrange(ModelGrade, AssessmentLabel) %>%
  transmute(name = paste0("grade_", ModelGrade, "__", make.names(AssessmentLabel))) %>%
  pull(name)

names(training_list) <- training_list_names

# =========================================================
# 5) write app data
# =========================================================

dir.create(output_dir, recursive = TRUE)

write_rds(schools_meta, file.path(output_dir, "LEA_META.rds"))
write_rds(training_list, file.path(output_dir, "APP_DATA.rds"))
