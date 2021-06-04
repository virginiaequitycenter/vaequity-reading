setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/vaequity-reading/dataprep")


# 1. Load libraries and data ----
library(tidyverse)
library(lme4)
library(ggeffects)


# Read in Data ------------------------------------------------------------
complete_data <- read_csv("data/complete_data.csv")

division_list <-
complete_data %>%
  select(division_name, division_use) %>%
  unique()

unique(complete_data$demographic)
# Split into subgroups

race_data <-
complete_data %>%
  filter(demographic == "Race") 

ses_data <-
  complete_data %>%
  filter(demographic == "Socioeconomics") 

ell_data <-
  complete_data %>%
  filter(demographic == "English Language") 

# We are not incorporating gender effects into this (that would only really be relevant if there were differences in gender composition between races)


# Start with Race ---------------------------------------------------------

# there is a significant amount of Black data missing.... 
# In theory, the random coefficients helps draw those measures towards the center so that helps. But at the same time, a bit precarious. 
race_data %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

race_data %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

race_data[is.na(race_data)] <- 0 
race_data$total_count[race_data$pass_count == 0] <- 0  # If the numerator equals zero/is not measured then set the denominator to 0 as well to not be measured (make the data point missing as opposed to having noone pass the exam)

## Make the simulated data
names(race_data)

race_data_sim <- 
  race_data %>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0
         )
  )  

write_csv(race_data_sim, file = "data/race_data_sim.csv")


# Run the Modelling
#race_data_sim <- read_csv("data/race_data_sim.csv")

# create nested data frame
race_data_sim_byyear <- race_data_sim %>% 
  group_by(test_year) %>% 
  nest()

# define model
yearly_model <- function(df) {
  glmer(pass ~   level + 
          (1 + level|division_name),
        family = binomial("logit"), 
        data = df)
}

# estimate models
race_data_sim_byyear_models <- race_data_sim_byyear %>% 
  mutate(model = map(data, yearly_model))

# predict out 
race_predicted <- 
race_data_sim_byyear_models %>%
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

write_csv(race_predicted, file = "data/race_predicted.csv")

race_predicted %>%
  select(test_year, division_name, black, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Race") %>%
  write_csv(., file = "../assets/data/tornado.csv")


# race_models <-
# race_data_sim_byyear_models %>% select(-data) 
# save(race_models , file = "data/race_model.Rdata")
#load("data/race_model.Rdata")
  
# SES Simulation ----------------------------------------------------------

ses_data %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

ses_data %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

ses_data[is.na(ses_data)] <- 0 
ses_data$total_count[ses_data$pass_count == 0] <- 0  # If the numerator equals zero/is not measured then set the denominator to 0 as well to not be measured (make the data point missing as opposed to having noone pass the exam)

## Make the simulated data
names(ses_data)

ses_data_sim <- 
  ses_data %>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0
         )
  )  

write_csv(ses_data_sim, file = "data/ses_data_sim.csv")

ses_data_sim <- read_csv("data/ses_data_sim.csv")

# create nested data frame
ses_data_sim_byyear <- ses_data_sim %>% 
  group_by(test_year) %>% 
  nest()

# estimate models
ses_data_sim_byyear_models <- ses_data_sim_byyear %>% 
  mutate(model = map(data, yearly_model))

ses_data_sim_byyear_models$model[[2]]

# need to know which one is the base level and which is modelled. 
ses_data_sim_byyear_models$model[[6]]

# predict out 
ses_predicted <- 
  ses_data_sim_byyear_models %>%
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

write_csv(ses_predicted, file = "data/ses_predicted.csv")


race_predicted %>%
  select(test_year, division_name, black, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Race")  %>%
  bind_rows(
    
ses_predicted %>%
  select(test_year, division_name, disadvantaged, other) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Socioeconomics") 
) %>%
  group_by(
    demographic, test_year, division_name
  ) %>%
  mutate(
    dif = abs(rate - lag(rate)),
    dif = case_when(
      is.na(dif) ~ 0,
      TRUE ~ dif
    ),
    dif = sum(dif),
    max_rate = max(rate) 
  )  %>%
  arrange(demographic, test_year, division_name, level) %>%
  group_by(demographic, test_year) %>%
  arrange(desc(dif) )%>%
  mutate(rank = ceiling((1:n())/2)   ) %>%
  left_join(division_list) %>%
  write_csv(., file = "../assets/data/tornado.csv")

# ELL Simulation ----------------------------------------------------------
# there is ALOT of data missing here
ell_data %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

ell_data %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

ell_data[is.na(ell_data)] <- 0 
ell_data$total_count[ell_data$pass_count == 0] <- 0  # If the numerator equals zero/is not measured then set the denominator to 0 as well to not be measured (make the data point missing as opposed to having noone pass the exam)

## Make the simulated data
names(ell_data)

ell_data_sim <- 
  ell_data %>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0
         )
  )  

write_csv(ell_data_sim, file = "data/ell_data_sim.csv")

ell_data_sim <- read_csv("data/ell_data_sim.csv")

# Make nested datawws
ell_data_sim_byyear <- ell_data_sim %>% 
  group_by(test_year) %>% 
  nest()

# estimate models
ell_data_sim_byyear_models <- ell_data_sim_byyear %>% 
  mutate(model = map(data, yearly_model))

# predict out
ell_pred <- ell_data_sim_byyear_models %>% 
  mutate(pred = map(model, ~ggpredict(.x, terms = c("level", "division_name"), type = "random")))



save(race_pred, ses_pred, ell_pred, file = "data/predicted_models.Rdata")









