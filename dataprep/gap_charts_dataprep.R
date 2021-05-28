## ---------------------------
## Script name: gap_charts_dataprep.R
##
## Author:Sam Powers
## Date Created: 2021-05-27
##
## ---------------------------
## Purpose of script: to prep the data for the multiple demographic comparisons of the most recent cohort. 
##   
##
## ---------------------------
## set working directory

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/vaequity-reading/dataprep")

## ---------------------------
## load up the packages we will need:

library(tidyverse)
library(extrafont)
loadfonts() ## Load in the fonts I want to use

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------
## read in data:  

# Columns that i need:
# School Year
# Division Name & Number
# Demographic Column
# Level of Demographic Column
# Test Level (for grade)
# Pass count
# Total Count
# Pass Rate
# need to calculate a cohort # 


# Race
black_students <-
read_csv("data/black_students_all_divisions.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_")) %>%
  select(school_year, division_number, division_name, level = race, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
    test_year = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    demographic = "Race",
    level = "Black"
  ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "") )   ) )%>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end )  # cohorts are defined according to the spring in which they graduate 8th grade


white_students <- 
  read_csv("data/white_students_all_divisions.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_")) %>%
  select(school_year, division_number, division_name, level = race, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
    test_year = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    demographic = "Race",
    level = "White"
  ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "") )   ) )%>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end )  


# SES Advantage

ses <-
read_csv("data/by_advantage_all_divisions.csv") %>%
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
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "") )   ) )%>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end )  


# English Language Learning
ell <-
read_csv("data/by_englishlearning_all_divisions.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_"))  %>%
  select(school_year, division_number, division_name, level = english_learner, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
    level = case_when(
      level == "N" ~ "Other",
      TRUE ~ "Learner"
    ),
    test_year = as.numeric(str_sub(school_year, 6,9)),
    grade = as.numeric(str_sub(test_level, 7,7)),
    demographic = "English Language"
  ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "") )   ) )%>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end )  


# Overall Rates
overall <-
read_csv("data/all_students_all_divisions.csv") %>%
  rename_with(.cols = everything(), .fn = ~str_replace_all(str_to_lower(.x),  " ", "_")) %>%
  select(school_year, division_number, division_name, test_level, pass_count, total_count, pass_rate) %>%
  mutate(
      level = "All Students",
      demographic = "Overall",
      test_year = as.numeric(str_sub(school_year, 6,9)),
      grade = as.numeric(str_sub(test_level, 7,7)),
    ) %>%
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "") )   ) )%>%
  mutate(across(contains("rate"), as.numeric) ) %>%
  mutate(max_grade = max(grade), 
         years_from_end = max_grade - grade,
         cohort = test_year + years_from_end 
        )  

division_regions <- read_csv("data/region_divisions.csv") %>%
  mutate(division = case_when(
    division %in% c("Colonial Beach", "West Point") ~ division,
    grepl("County", division) == FALSE ~ paste0(division, " City"),
    TRUE ~ division
  )) %>%
  mutate(division_use = tolower(str_replace_all(division, " ", "_")))

averages <-
overall %>%
  select(school_year, division_number, grade, average = pass_rate)

averages %>%
  filter(is.na(average))
View(averages)
# bind them together ------------------------------------------------------

complete_data <- 
black_students %>%
  bind_rows(
    white_students,
    ses,
    ell
 #   overall
  ) %>%
  mutate(
    division_use = tolower(str_replace_all(division_name, " ", "_"))
  ) %>%
  left_join(division_regions) %>%
  group_by(
    division_name, test_year, grade, demographic
  )  %>%
  mutate(
    mean_rate = mean(pass_rate, na.rm = TRUE),
    direction = case_when(
      pass_rate >= mean_rate ~  1,
      TRUE ~ -1
    )
  ) %>%
  left_join(averages)

complete_data %>%
  filter(is.na(region)) ## perfect, we've got them all named. 

# View(complete_data)


# Export it ---------------------------------------------------------------
# Complete data
write_csv(complete_data, "data/complete_data.csv")

unique(complete_data$cohort)
table(complete_data$cohort, complete_data$grade)


# cohort 2019

complete_data %>%
  filter(cohort == 2019) %>%
  mutate(
    demographic_use = str_replace_all(str_to_lower(demographic), " ", "_"),
    level_use = str_replace_all(str_to_lower(level), " ", "_")
  ) %>%
  filter(!is.na(pass_rate)) %>%
  
write_csv(., "../assets/data/cohort_2019.csv")


unique(complete_data$level)


complete_data$grade
complete_data$pass_rate



    
    

  
  
  













