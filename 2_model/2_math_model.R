## Purpose of script: to simulate the missing MATH data and then model for all VA students.

# Setup ----
library(boxr)
library(lme4)
library(tidyverse)

# Note: because the data sets are large running the models all at once can crash RStudio. 
# As a workaround this script saves and exports each model so we can clear the RSession each 
# time, and then removes the df from the global environment. Future versions could modularize 
# this process with a function and dry programming style. 

# Get data ----
#box_auth() #set up your box app at: https://r-box.github.io/boxr/articles/boxr-app-interactive.html
#math <- box_read_csv() 

math <- read_csv("1_dataprep/data/processed/1_complete_math.csv")

division_list <- math %>%
  select(division_name, division_use) %>%
  unique()

# Split into subgroups: race, socioeconomic status, and ethnicity
math_race <- math %>%
  filter(demographic == "Race")

math_ses <- math %>%
  filter(demographic == "Socioeconomics")

math_eth <- math %>%
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

# Explore missing values:
math_race %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

math_race %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well 
# to not be measured (make the data point missing as opposed to having no one pass the exam)
math_race[is.na(math_race)] <- 0
math_race$total_count[math_race$pass_count == 0] <- 0

# Simulate missing student data:
math_race <- math_race %>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0))

# Create nested data frame with only columns relevant to the model (one df per year):
math_race_byyear <- math_race %>% 
  group_by(test_year) %>% 
  nest()

# Estimate models: takes a lot of RAM 
math_race_byyear <- math_race_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so you don't crash R :)
saveRDS(math_race_byyear, "2_model/2_math_race_byyear.RDS")

# Get model coefficients:
math_race_coefs <- math_race_byyear %>%
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

# Format and save for tornado plot:
math_race_coefs %>%
  select(test_year, division_name, black, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Race") %>%
  write_csv(., file = "2_model/2_math_race.csv")

# *SES ----

# Explore missing values:
math_ses %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

math_ses %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

math_ses[is.na(math_ses)] <- 0 
math_ses$total_count[math_ses$pass_count == 0] <- 0  

# Simulate missing student data:
math_ses <- math_ses %>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0))

# Create nested data frame with only columns relevant to the model (one df per year):
math_ses_byyear <- math_ses %>% 
  group_by(test_year) %>% 
  nest()

# Estimate models - takes time
math_ses_byyear <- math_ses_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so that you don't crash RStudio :) 
#saveRDS(math_ses_byyear, "2_model/2_math_ses_byyear.RDS")

# Get model coefficients:
math_ses_coefs <- math_ses_byyear %>%
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

# Format and save for tornado plot:
math_ses_coefs %>%
  select(test_year, division_name, disadvantaged, other) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Socioeconomic Status") %>%
  write_csv(file = "2_model/2_math_ses.csv")

# *Ethnicity ----

# Explore missing values:
math_eth %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

math_eth %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well 
# to not be measured (make the data point missing as opposed to having no one pass the exam)
math_eth[is.na(math_eth)] <- 0 
math_eth$total_count[math_eth$pass_count == 0] <- 0  

# Simulate missing student data:
math_eth <- math_eth %>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0))

# Create nested data frame with only columns relevant to the model (one df per year):
math_eth_byyear <- math_eth %>% 
  select(level, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()

# Estimate models:
math_eth_byyear <- math_eth_byyear %>% 
  mutate(model = map(data, yearly_model))
# Note: you might get a warning saying something like, "Model failed to converge," but it's ok

# Intermediary step so that you don't crash RStudio :) 
#saveRDS(math_eth_byyear, "2_model/math_eth_byyear.RDS")

# Get coefficients:
math_eth_coefs <- math_eth_byyear %>%
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
math_eth_coefs %>%
  select(test_year, division_name, hispanic, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Ethnicity") %>%
  write_csv(file = "2_model/2_math_eth.csv")

# Export ----

# Combine:
math_race <- read_csv("2_model/2_math_race.csv")
math_ses <- read_csv("2_model/2_math_ses.csv")
math_eth <- read_csv("2_model/2_math_eth.csv")

math_tornado <- bind_rows(list(math_race, math_ses, math_eth))

# Save:
math_tornado %>%
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
  write_csv("3_math_gaps/3_math_tornado.csv")
