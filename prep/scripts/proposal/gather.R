#---------------------------packages & route----------------------------------#
###############################################################################
if (require(pacman) == F) {
  install.packages("pacman")}
pacman::p_load(pacman,
               tidyverse,
               readxl,
               fastDummies,
               update = T)

main <- "prep"


#-----------------------load files--------------------------------------------#
###############################################################################
#-----------------------example data------------------------------------------#
exmpl0 = read_xlsx(file.path(main, "data", "Income vs. poverty metric.xlsx"), na = c("", "NA", "NULL"))


#-----------------------student data------------------------------------------#
ass0 = read_csv(file.path(main, "data", "assessments.csv"), na = c("", "NA", "NULL"))
unit0 = read_csv(file.path(main, "data", "unitCount.csv"), na = c("", "NA", "NULL"))


#-----------------------student unit count------------------------------------#
#grab school level features by year
unit1 <-
  unit0 %>% 
  select(-c(DistrictCode,
            DistrictName,
            SchoolName)) %>% 
  filter(SchoolYear >= 2015 & SchoolYear <= 2025,
         Grade %in% c(paste0("0", rep(3:9)), 10:12)) %>%
  group_by(SchoolYear, SchoolCode) %>% 
  mutate(units = n()) %>% 
  ungroup() %>% 
  mutate(SchoolYear = as.factor(SchoolYear),
         across(Grade:MilitaryDep, ~ factor(.))) %>% 
  pivot_longer(-c(StudentID, SchoolYear, SchoolCode, units), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(dummy = 1,
         name = paste(variable, value, sep = ".")) %>%
  select(-c(variable, value)) %>% 
  arrange(name) %>% 
  pivot_wider(names_from = name,
              values_from = dummy,
              values_fill = 0) %>% 
  group_by(SchoolYear,
           SchoolCode,
           units) %>% 
  reframe(across(County_Name.Kent:SWD.SWD, ~ mean(.)))

#2025 unit count schools
schls <- 
  unit0 %>% 
  mutate(SchoolYear = as.factor(SchoolYear)) %>% 
  filter(SchoolYear == 2025,
         Grade %in% c(paste0("0", rep(3:9)), 10:12)) %>% 
  distinct(SchoolYear, DistrictCode, DistrictName, SchoolCode, SchoolName)


#---------------------summative assessments
ass1 <- 
  ass0 %>% 
  mutate(SchoolYear = as.factor(SchoolYear),
         AssessmentName = str_to_upper(AssessmentName),
         ContentArea = str_to_upper(ContentArea))

#https://education.delaware.gov/educators/academic-support/standards-and-assessments/testing_calendar/
assessCurrent <-
  ass1 %>% 
  filter(SchoolYear == 2025,
         AssessmentName %in% c("DESSA", "SAT11", "SBAC")) %>% 
  distinct(AssessmentName, ContentArea, Grade)

#mean and number of summative assessments (target) available and missing by school and year
ass2 <-
  ass1 %>% 
  semi_join(assessCurrent) %>% 
  group_by(SchoolYear,
           SchoolCode,
           Grade,
           AssessmentName,
           ContentArea) %>% 
  reframe(ScaleScore.mean = mean(ScaleScore, na.rm = T),
          n = sum(!is.na(ScaleScore)),
          na = sum(is.na(ScaleScore))) %>% 
  filter(!is.na(ScaleScore.mean))

check <- 
  ass2 %>% 
  group_by(SchoolYear,
           Grade,
           AssessmentName,
           ContentArea) %>% 
  reframe(((sum(na) / sum(n)) * 100) %>% round(1))

#--------------------training data
drop_cols <- 
  c("County_Name.Kent",
    "ELL.N_ELL",
    "FosterCare.N_FC",
    "Gender.F",
    "Geography.N_WILM",
    "Grade.12",
    "Homeless.N_H",
    "Immersion.N_IMM",
    "LowIncome.N_LI",
    "Migrant.N_MIG",
    "MilitaryDep.N_MILI",
    "RaceReportTitle.White",
    "SPEDCode.0",
    "SWD.SWD",
    "SWD.N_SWD")

train0 <-
  unit1 %>% 
  left_join(ass2) %>% 
  droplevels() %>% 
  mutate(AssessmentLabel = case_when(AssessmentName == "SBAC"  & ContentArea == "ELA" ~ "DeSSA ELA",
                                     AssessmentName == "SBAC"  & ContentArea == "MATH" ~ "DeSSA Math",
                                     AssessmentName == "SAT11" & ContentArea == "ELA" ~ "SAT ELA",
                                     AssessmentName == "SAT11" & ContentArea == "MATH" ~ "SAT Math",
                                     AssessmentName == "SAT11" & ContentArea == "ESSAY" ~ "SAT Essay",
                                     AssessmentName == "DESSA" & ContentArea == "SOC" ~ "DeSSA Social Studies",
                                     AssessmentName == "DESSA" & ContentArea == "SCI" ~ "DeSSA Science",
                                     AssessmentName == "NA" | ContentArea == "NA" ~ NA,
                                     .default = paste(AssessmentName, ContentArea))) %>% 
  select(SchoolYear,
         SchoolCode,
         Grade,
         AssessmentLabel,
         ScaleScore.mean,
         n,
         na,
         units,
         County_Name.Kent:SWD.SWD) %>% 
  filter(!is.na(ScaleScore.mean)) 

train1 <-
  train0 %>%
  dummy_cols("SchoolYear", remove_first_dummy = T) %>%
  rename_with(~ str_replace(.x, "_", "."), starts_with("SchoolYear")) %>%
  select(-any_of(drop_cols)) %>%
  rename_with(~ gsub("[[:space:]/-]+", "_", .x))

#----------------2025 schools
#twntyFv <- schls %>% left_join(ass2) %>% filter(!is.na(ScaleScore.mean)) %>% left_join(train1)
 














