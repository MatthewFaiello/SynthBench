# =========================================================
# R/data.R
# Load app data and build UI lookup tables
# =========================================================

APP_DATA <- read_rds(file.path("input_data", "APP_DATA.rds"))
LEA_META <- read_rds(file.path("input_data", "LEA_META.rds"))

# Flatten APP_DATA so the app can look up available
# assessment / grade / year combinations in one table.
OPTIONS <- do.call(bind_rows, c(APP_DATA, list(.id = "model_key"))) %>%
  group_by(model_key, AssessmentLabel, ModelGrade, SchoolYear) %>% 
  summarise(n_min = min(n, na.rm = TRUE),
            n_max = max(n, na.rm = TRUE),
            .groups = "drop") %>% 
  distinct(model_key, AssessmentLabel, ModelGrade, SchoolYear, n_min, n_max)

SCOPE_1_CHOICES <- OPTIONS %>%
  distinct(AssessmentLabel) %>%
  pull(AssessmentLabel) %>%
  sort()
