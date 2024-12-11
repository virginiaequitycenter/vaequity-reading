# Purpose of script: to prep the MATH data for the multiple demographic comparisons of the most recent cohort. 

# Setup ----
library(tidyverse)

# Get data ----
## data from VDOE Fall Membership Build-a-Table
#https://p1pe.doe.virginia.gov/apex/f?p=152:1:::::p_session_id,p_application_name:2802562216169884745,testresults 

# Note: cohorts are defined according to the spring in which they graduate 8th grade

### Query parameters:
# school years = 2005-2006 through 2023-2024
# Division: multi-select all
# race = create separate tables for white, black, Hispanic
# gender = combine genders 
# grade = All Grades
# [reporting categories] = create separate tables for disadvantaged and English Learner 
# Test Level = Grade 3 - Grade 8
# Test Source = SOL
# Subject Area = Mathematics
# Test = Mathematics
# Statistic = Total Count, Pass Rate, Pass Proficient Rate, Pass Proficient Count, Pass Count, Pass Advanced Rate, 
# Pass Advanced Count, Fail Rate, Fail Count, Average SOL Scaled Score

# Black Reading, Black Math
# Hispanic Reading, Hispanic Math
# White Reading, White Math
# Disadv Reading, Disadv Math

# *Race ----
black_students <-
  read_csv("1_dataprep/data/raw/2024_black_students_all_divisions_math.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_")) %>%
  select(school_year, division_number, division_name, level = race, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
    test_year = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    demographic = "Race",
    level = "Black"
  ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end)  

white_students <- 
  read_csv("1_dataprep/data/raw/2024_white_students_all_divisions_math.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_")) %>%
  select(school_year, division_number, division_name, level = race, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
    test_year = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    demographic = "Race",
    level = "White"
  ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end)  

# *Ethnicity ----
hispanic_students <- 
  read_csv("1_dataprep/data/raw/2024_hispanic_students_all_divisions_math.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_")) %>%
  select(school_year, division_number, division_name, level = race, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
    test_year = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    demographic = "Ethnicity",
    level = "Hispanic"
  ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end)

white_students_ethnicity <- 
  read_csv("1_dataprep/data/raw/2024_white_students_all_divisions_math.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_")) %>%
  select(school_year, division_number, division_name, level = race, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
    test_year = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    demographic = "Ethnicity",
    level = "Non-Hispanic"
  ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end)  

# *SES Advantage ----
ses <-
  read_csv("1_dataprep/data/raw/2024_by_advantage_all_divisions_math.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_")) %>%
  select(school_year, division_number, division_name, level = disadvantaged, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
    level = case_when(
      level == "N" ~  "Other",      #"Not Disadvantaged",
      TRUE ~    "Disadvan"  # "Disadvantaged"
    ),
    test_year = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    demographic = "Socioeconomics"
  ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", ""))))%>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end)  

# *English Language Learning ----
# ell <-
#  read_csv("1_dataprep/data/raw/2023_by_englishlearning_all_divisions.csv") %>%
#  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_"))  %>%
#  select(school_year, division_number, division_name, level = english_learners, test_level, pass_count, total_count, pass_rate) %>%
#  mutate(
#    level = case_when(
#      level == "N" ~ "Other",
#      TRUE ~ "Learner"
#    ),
#    test_year = as.numeric(str_sub(school_year, 6,9)),
#    grade = as.numeric(str_sub(test_level, 7,7)),
#    demographic = "English Language"
#  ) %>%
#  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>%
#  mutate(across(contains("rate"), as.numeric) ) %>%
#  mutate(max_grade = max(grade), 
#         years_from_end = max_grade - grade,
#         cohort = test_year + years_from_end )  

# *Overall Rates ----
overall <-
  read_csv("1_dataprep/data/raw/2024_all_students_all_divisions_math.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_")) %>%
  select(school_year, division_number, division_name, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
    level = "All Students",
    demographic = "Overall",
    test_year = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
  ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>%
  mutate(across(contains("rate"), as.numeric)) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end)

# Fix names ----
# Use division number exclusively in future years add re-add division name later in the process?
division_regions <- read_csv("1_dataprep/data/region_divisions.csv") %>%
  mutate(division = case_when(
    division %in% c("Colonial Beach", "West Point", "Alleghany Highlands") ~ division,
    grepl("County", division) == FALSE ~ paste0(division, " City"),
    TRUE ~ division
  )) %>%
  mutate(division_use = tolower(str_replace_all(division, " ", "_"))) 

# Get averages ----
averages <-
  overall %>%
  select(school_year, division_number, grade, average = pass_rate)

# Join ----
complete_data <- 
  black_students %>%
  bind_rows(white_students, hispanic_students, white_students_ethnicity, ses, overall) %>%
  mutate(division_use = tolower(str_replace_all(division_name, " ", "_"))) %>%
  left_join(division_regions) %>%
  mutate(division= ifelse(division=="Alleghany County","Alleghany Highlands",division)) %>%
  mutate(division_use= ifelse(division_use=="alleghany_county","alleghany_highlands",division_use)) %>%
  mutate(division_name= ifelse(division_name=="Alleghany County","Alleghany Highlands",division_name)) %>%
  group_by(
    division_name, test_year, grade, demographic)  %>%
  mutate(
    mean_rate = mean(pass_rate),
    direction = case_when(
      pass_rate >= mean_rate ~  1,
      TRUE ~ -1
    ),
    min_rate = min(pass_rate, na.rm = TRUE)
  ) %>%
  left_join(averages) %>%
  mutate(                   # add in the center lines to help the cut paths in the vis. 
    average = case_when(
      is.na(mean_rate) ~ average,
      is.infinite(mean_rate) ~ average,
      min_rate > average ~ mean_rate,
      TRUE ~ average))

# Save ----
write_csv(complete_data, "1_dataprep/data/processed/1_complete_math.csv")

# *Upload to Box ----
# This is done manually. Once you've saved the csv to Box, move on to step 2, modeling 


# ARCHIVE ----
#cohort 2019:
#complete_data %>%
#  filter(cohort == 2019) %>%
#  mutate(
#    filter_out = case_when(
#      demographic == "English Language" & grade  >= 7 ~ 1,
#      TRUE ~ 0
#    )
#  ) %>%
#  filter(filter_out == 0) %>%
#  mutate(
#    demographic_use = str_replace_all(str_to_lower(demographic), " ", "_"),
#    level_use = str_replace_all(str_to_lower(level), " ", "_")
#  ) %>%
#  filter(!is.na(pass_rate)) %>%
#  
#  write_csv(., "../assets/data/cohort_2019.csv")
