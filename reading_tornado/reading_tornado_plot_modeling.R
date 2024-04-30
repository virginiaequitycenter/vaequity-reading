library(boxr)
library(lme4)
library(tidyverse)
library(magrittr)

# Note: because the data sets are large running the models all at once can crash RStudio. 
# As a workaround this script saves and exports each model so we can clear the RSession each 
# time, and then removes the df from the global environment. Future versions could modularize 
# this process with a function and DRY programming style. 

# 1 Get data ----

box_auth() #set up your box app at: https://r-box.github.io/boxr/articles/boxr-app-interactive.html
#math <- box_read_csv(1489555264174) #2005_2023_math_complete_data hold for math 

# Read in complete reading data 
reading_complete_data <- box_read_csv(1489544488173) #2005_2023_reading_complete_data

division_list <-
reading_complete_data %>%
  select(division_name, division_use) %>%
  unique()

# Split into subgroups: race, socioeconomic status, and ethnicity 
reading_race_data <- reading_complete_data %>%
  filter(demographic == "Race")

reading_ses_data <- reading_complete_data %>%
  filter(demographic == "Socioeconomics")

reading_eth_data <- reading_complete_data %>%
  filter(demographic == "Ethnicity")

# reading_ell_data <- reading_complete_data %>%
#   filter(demographic == "English Language") 

# 2 Race --------------------------------------------------------

# In theory, the random coefficients helps draw those measures towards the center 
# so that helps. But at the same time, a bit precarious. 
reading_race_data %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

reading_race_data %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

#reading_race_data[is.na(reading_race_data)] <- 0 
reading_race_data$total_count[reading_race_data$pass_count == 0] <- 0  

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)

# Prep for simulation - DRY programming style
reading_race_data %<>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0
         )
  )

# Create nested data frame with only columns relevant to the model (one df per year)
reading_race_data_byyear <- reading_race_data %>% 
  select(level, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest() 

# Define model
yearly_model <- function(df) {
  glmer(pass ~   level + 
          (1 + level|division_name),
        family = binomial("logit"), 
        data = df)
}

# Estimate models - takes a lot of RAM (TODO: progress bar)
reading_race_data_byyear <- reading_race_data_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so you don't crash R :)
#saveRDS(reading_race_data_sim_byyear_models, "reading_race_data_sim_byyear_models.RDS" )

# Get model coefficients 
reading_race_data_byyear <- reading_race_data_byyear %>%
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

# Format and save for tornado plot
reading_race_data_byyear %>%
  select(test_year, division_name, black, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Race") %>%
  write_csv(file = "reading_tornado_race.csv")
 
# 3 SES ----------------------------------------------------------

# Handle missing values (similar to race)
reading_ses_data %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

reading_ses_data %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

#reading_ses_data[is.na(reading_ses_data)] <- 0 not sure what this does
reading_ses_data$total_count[reading_ses_data$pass_count == 0] <- 0  

# Prep for simulation 
reading_ses_data %<>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0
         )
  )  

# write_csv(reading_ses_data_sim, file = "dataprep/data/2005_2023_data/reading_ses_data_sim.csv")
# reading_ses_data_sim <- read_csv("dataprep/data/2005_2023_data/reading_ses_data_sim.csv")

# Create nested data frame with only columns relevant to the model (one df per year)
reading_ses_data_byyear <- reading_ses_data %>% 
  select(level, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()
 
# Estimate models - takes time
reading_ses_data_byyear_models <- reading_ses_data_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so that you don't crash RStudio :) 
#saveRDS(reading_ses_data_byyear_models, "reading_ses_data_byyear_models.RDS")

# Need to know which one is the base level and which is modeled 
reading_ses_data_byyear_models$model[[2]]
reading_ses_data_byyear_models$model[[6]]

# Get coefficients 
reading_ses_data_byyear <- reading_ses_data_byyear_models %>%
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

# Format and save for tornado plot
reading_ses_data_byyear %>%
  select(test_year, division_name, disadvantaged, other) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Socioeconomic Status") %>%
  write_csv(file = "reading_tornado_ses.csv")

# 4 Ethnicity ----------------------------------------------------

# Handle missing values (similar to race)
reading_eth_data %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

reading_eth_data %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

reading_eth_data[is.na(reading_eth_data)] <- 0
reading_eth_data$total_count[reading_eth_data$pass_count == 0] <- 0  

# Prep for simulation
reading_eth_data %<>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0
         )
  )  

# Create nested data frame with only columns relevant to the model (one df per year)
reading_eth_data_byyear <- reading_eth_data %>% 
  select(level, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()

# Estimate models - takes time
reading_eth_data_byyear_models <- reading_eth_data_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so that you don't crash RStudio :) 
#saveRDS(reading_eth_data_byyear_models, "reading_eth_data_byyear_models.RDS")

# Need to know which one is the base level and which is modeled 
coef(reading_eth_data_byyear_models$model[[6]])
reading_eth_data_byyear_models$model[[2]]

# Get coefficients  
reading_eth_data_byyear <- reading_eth_data_byyear_models %>%
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

# Format and save for tornado plot
reading_eth_data_byyear %>%
  select(test_year, division_name, hispanic, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Ethnicity") %>%
  write_csv(file = "reading_tornado_eth.csv")


# 5 Reading Tornado -------------------------------------------------------

reading_tornado_race <- read_csv("reading_tornado_race.csv")
reading_tornado_ses <- read_csv("reading_tornado_ses.csv")
reading_tornado_eth <- read_csv("reading_tornado_eth.csv")

final_reading_tornado <- bind_rows(list(reading_tornado_race, reading_tornado_ses, reading_tornado_eth))

final_reading_tornado %>%
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
  write_csv("final_reading_tornado.csv")

# Not done: 
# ELL Simulation ----------------------------------------------------------

# ELL simulation may not be reasonably accurate. we will see. 
# there is ALOT of data missing here
# reading_ell_data %>%
#   mutate(missing = case_when(
#     is.na(pass_count) ~ 1,
#     TRUE ~ 0
#   )) %>%
#   group_by(test_year, level) %>%
#   summarize(missing = mean(missing)) %>%
#   spread(test_year, missing)
# 
# reading_ell_data %>%
#   filter(is.na(pass_count)) %>%
#   pull(division_name) %>%
#   unique()
# 
# reading_ell_data[is.na(reading_ell_data)] <- 0 
# reading_ell_data$total_count[reading_ell_data$pass_count == 0] <- 0  # If the numerator equals zero/is not measured then set the denominator to 0 as well to not be measured (make the data point missing as opposed to having noone pass the exam)
# 
# ## Make the simulated data
# names(reading_ell_data)
# 
# reading_ell_data_sim <- 
#   reading_ell_data %>%
#   uncount(total_count) %>%
#   group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
#   mutate(id = 1:n(),
#          pass = case_when(
#            id <= pass_count ~ 1,
#            TRUE ~ 0
#          )
#   )  
# 
# write_csv(reading_ell_data_sim, file = "dataprep/data/2005_2023_data/reading_ell_data_sim.csv")
# 
# reading_ell_data_sim <- read_csv("dataprep/data/2005_2023_data/reading_ell_data_sim.csv")
# 
# # Make nested datas
# reading_ell_data_sim_byyear <- reading_ell_data_sim %>% 
#   group_by(test_year) %>% 
#   nest()
# 
# # estimate models
# reading_ell_data_sim_byyear_models <- reading_ell_data_sim_byyear %>% 
#   mutate(model = map(data, yearly_model))
# 
# # predict out
# reading_ell_predicted <- reading_ell_data_sim_byyear_models %>% 
#   mutate(pred = map(model, ~ggpredict(.x, terms = c("level", "division_name"), type = "random")))
# 
# save(reading_race_predicted, reading_ses_predicted, reading_eth_predicted, reading_ell_predicted, file = "dataprep/data/2005_2023_data/reading_predicted_models.Rdata")
