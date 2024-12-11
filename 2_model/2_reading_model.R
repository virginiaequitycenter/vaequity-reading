## Purpose of script: to simulate the missing READING data and then model for all VA students.

# Setup ----
library(boxr)
library(lme4)
library(tidyverse)

# Note: because the data sets are large running the models all at once can crash RStudio. 
# As a workaround this script saves and exports each model so we can clear the RSession each 
# time, and then removes the df from the global environment. Future versions could modularize 
# this process with a function and DRY programming style. 

# Get data ----
#box_auth() #set up your box app at: https://r-box.github.io/boxr/articles/boxr-app-interactive.html
#reading <- box_read_csv() 

reading <- read_csv("1_dataprep/data/processed/1_complete_reading.csv")

division_list <- reading %>%
  select(division_name, division_use) %>%
  unique()

# Split into subgroups: race, socioeconomic status, and ethnicity 
reading_race <- reading %>%
  filter(demographic == "Race")

reading_ses <- reading %>%
  filter(demographic == "Socioeconomics")

reading_eth <- reading %>%
  filter(demographic == "Ethnicity")

# Model ----
# Define model:
yearly_model <- function(df) {
  glmer(pass ~   level + 
          (1 + level|division_name),
        family = binomial("logit"), 
        data = df)
}

# *Race ----

# Explore missing data:
# In theory, the random coefficients helps draw those measures towards the center 
# so that helps. But at the same time, a bit precarious. 
reading_race %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

reading_race %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)
reading_race[is.na(reading_race)] <- 0 
reading_race$total_count[reading_race$pass_count == 0] <- 0  

# Simulate missing student data:
reading_race <- reading_race %>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0))

# Create nested data frame with only columns relevant to the model (one df per year):
reading_race_byyear <- reading_race %>% 
  select(level, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()

# Estimate models: takes a lot of RAM 
reading_race_byyear <- reading_race_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so you don't crash R :)
#saveRDS(reading_race_data_sim_byyear_models, "reading_race_data_sim_byyear_models.RDS" )

# Get model coefficients:
reading_race_coefs <- reading_race_byyear %>%
  mutate(
    coefficients = map(model, ~coef(.x)$division_name %>%
                         data.frame() %>%
                         rownames_to_column() %>%
                         tibble() %>%
                         rename(
                           division_name = 1,
                           intercept = 2,
                           level = 3
                         ) %>%
                         mutate(
                           black = exp(intercept)/(1 + exp(intercept)),
                           white = exp(intercept + level)/(1 + exp(intercept + level))
                         )
                       )
  ) %>%
  select(-data, - model) %>%
  unnest(coefficients)

# Format and save for plot:
reading_race_coefs %>%
  select(test_year, division_name, black, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Race") %>%
  write_csv(file = "2_model/2_reading_race.csv")
 
# *SES ----

# Explore missing data:
reading_ses %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

reading_ses %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)
reading_ses[is.na(reading_ses)] <- 0 
reading_ses$total_count[reading_ses$pass_count == 0] <- 0  

# Simulate missing student data:
reading_ses <- reading_ses %>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0)) 

# Create nested data frame with only columns relevant to the model (one df per year):
reading_ses_byyear <- reading_ses %>% 
  select(level, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()
 
# Estimate models: takes time
reading_ses_byyear <- reading_ses_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so that you don't crash RStudio :) 
#saveRDS(reading_ses_byyear, "2_model/reading_ses_byyear.RDS")

# Get coefficients:
reading_ses_coefs <- reading_ses_byyear%>%
  mutate(
    coefficients = map(model, ~coef(.x)$division_name %>%
                         data.frame() %>%
                         rownames_to_column() %>%
                         tibble() %>%
                         rename(
                           division_name = 1,
                           intercept = 2,
                           level = 3
                         ) %>%
                         mutate(
                           disadvantaged = exp(intercept)/(1 + exp(intercept)),
                           other = exp(intercept + level)/(1 + exp(intercept + level))
                         )
    )
  ) %>%
  select(-data, - model) %>%
  unnest(coefficients)

# Format and save for plot:
reading_ses_coefs %>%
  select(test_year, division_name, disadvantaged, other) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Socioeconomic Status") %>%
  write_csv(file = "2_model/2_reading_ses.csv")

# *Ethnicity ----

# Explore missing values:
reading_eth %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

reading_eth %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)
reading_eth[is.na(reading_eth)] <- 0
reading_eth$total_count[reading_eth$pass_count == 0] <- 0

# Simulate missing student data:
reading_eth <- reading_eth %>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0))  

# Create nested data frame with only columns relevant to the model (one df per year):
reading_eth_byyear <- reading_eth %>% 
  select(level, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()

# Estimate models: takes time
reading_eth_byyear <- reading_eth_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so that you don't crash RStudio :) 
#saveRDS(reading_eth_data_byyear_models, "reading_eth_data_byyear_models.RDS")

# Get coefficients:
reading_eth_coefs <- reading_eth_byyear %>%
  mutate(
    coefficients = map(model, ~coef(.x)$division_name %>%
                         data.frame() %>%
                         rownames_to_column() %>%
                         tibble() %>%
                         rename(
                           division_name = 1,
                           intercept = 2,
                           level = 3
                         ) %>%
                         mutate(
                           hispanic = exp(intercept)/(1 + exp(intercept)),
                           white = exp(intercept + level)/(1 + exp(intercept + level))
                         )
    )
  ) %>%
  select(-data, - model) %>%
  unnest(coefficients)

# Format and save for tornado plot:
reading_eth_coefs %>%
  select(test_year, division_name, hispanic, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Ethnicity") %>%
  write_csv(file = "2_model/2_reading_eth.csv")


# Export ----

# Combine:
reading_race <- read_csv("2_model/2_reading_race.csv")
reading_ses <- read_csv("2_model/2_reading_ses.csv")
reading_eth <- read_csv("2_model/2_reading_eth.csv")

reading_tornado <- bind_rows(list(reading_race, reading_ses, reading_eth))

# Save:
reading_tornado %>%
  group_by(demographic, test_year, division_name) %>%
  mutate(dif = abs(rate - lag(rate)),
         dif = case_when(
           is.na(dif) ~ 0,
           TRUE ~ dif
         ),
         dif = sum(dif),
         max_rate = max(rate)) %>%
  arrange(demographic, test_year, division_name, level) %>%
  group_by(demographic, test_year) %>%
  arrange(desc(dif) )%>%
  mutate(rank = ceiling((1:n())/2)   ) %>%
  left_join(division_list) %>%
  write_csv("4_reading_gaps/4_reading_tornado.csv")
