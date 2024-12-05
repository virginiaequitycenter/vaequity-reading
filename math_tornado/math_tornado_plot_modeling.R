library(boxr)
library(lme4)
library(tidyverse)
library(magrittr)

# Note: because the data sets are large running the models all at once can crash RStudio. 
# As a workaround this script saves and exports each model so we can clear the RSession each 
# time, and then removes the df from the global environment. Future versions could modularize 
# this process with a function and DRY programming style. 

# 1 Get data ----

#box_auth() #set up your box app at: https://r-box.github.io/boxr/articles/boxr-app-interactive.html
#math <- box_read_csv(1489555264174) #2005_2023_math_complete_data hold for math 

math <- read_csv("dataprep/data/2024_complete_data_math.csv")


division_list <-
  math %>%
  select(division_name, division_use) %>%
  unique()

# Split into subgroups: race, socioeconomic status, and ethnicity
math_race_data <- math %>%
  filter(demographic == "Race")

math_ses_data <- math %>%
  filter(demographic == "Socioeconomics") 

math_eth_data <- math %>%
  filter(demographic == "Ethnicity") 

# ell_data <- math %>%
#   filter(demographic == "English Language") 

# 2 Race ---------------------------------------------------------

# Mechanism for missing values
math_race_data %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

math_race_data %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well 
# to not be measured (make the data point missing as opposed to having noone pass the exam)

math_race_data[is.na(math_race_data)] <- 0 
math_race_data$total_count[math_race_data$pass_count == 0] <- 0  

# Prep for simulation
math_race_data %<>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0
         )
  )  

# Create nested data frame with only columns relevant to the model (one df per year)
math_race_data_byyear <- math_race_data %>% 
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
math_race_data_byyear <- math_race_data_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so you don't crash R :)
saveRDS(math_race_data_byyear, "math_race_data_byyear.RDS")

# Get model coefficients 
math_race_data_byyear <- math_race_data_byyear %>%
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
math_race_data_byyear %>%
  select(test_year, division_name, black, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Race") %>%
  write_csv(., file = "math_tornado_race.csv")


# 3 SES ----------------------------------------------------------

# Handle missing values (similar to race)
math_ses_data %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

math_ses_data %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

math_ses_data[is.na(math_ses_data)] <- 0 
math_ses_data$total_count[math_ses_data$pass_count == 0] <- 0  

# Prep for simulation
math_ses_data %<>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0
         )
  )

# Create nested data frame with only columns relevant to the model (one df per year)
math_ses_data_byyear <- math_ses_data %>% 
  group_by(test_year) %>% 
  nest()

# Estimate models - takes time
math_ses_data_byyear_models <- math_ses_data_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so that you don't crash RStudio :) 
saveRDS(math_ses_data_byyear_models, "math_ses_data_byyear_models.RDS")

# Need to know which one is the base level and which is modeled
math_ses_data_byyear_models$model[[2]]
math_ses_data_byyear_models$model[[6]]

# Get model coefficients 
math_ses_data_byyear <- math_ses_data_byyear_models %>%
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
math_ses_data_byyear %>%
  select(test_year, division_name, disadvantaged, other) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Socioeconomic Status") %>%
  write_csv(file = "math_tornado_ses.csv")

# 4 Ethnicity ----------------------------------------------------

# Handle missing values (similar to race)
math_eth_data %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, level) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

math_eth_data %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

math_eth_data[is.na(math_eth_data)] <- 0 
math_eth_data$total_count[math_eth_data$pass_count == 0] <- 0  # If the numerator equals zero/is not measured then set the denominator to 0 as well to not be measured (make the data point missing as opposed to having noone pass the exam)

# Prep for simulation
math_eth_data %<>%
  uncount(total_count) %>%
  group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0
         )
  )  

# Create nested data frame with only columns relevant to the model (one df per year)
math_eth_data_byyear <- math_eth_data %>% 
  select(level, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()

# Estimate models
math_eth_data_byyear_models <- math_eth_data_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step so that you don't crash RStudio :) 
# saveRDS(math_eth_data_byyear_models, "math_eth_data_byyear_models.RDS")

# need to know which one is the base level and which is modeled
#coef(math_eth_data_byyear_models$model[[6]])
#math_eth_data_byyear_models$model[[2]]

# Get coefficients  
math_eth_data_byyear <- math_eth_data_byyear_models %>%
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
math_eth_data_byyear %>%
  select(test_year, division_name, hispanic, white) %>%
  gather(
    level, rate, -test_year, -division_name
  ) %>%
  mutate(demographic = "Ethnicity") %>%
  write_csv(file = "math_tornado_eth.csv")

# 5 Math Tornado-------------------------------------------------------

math_tornado_race <- read_csv("math_tornado_race.csv")
math_tornado_ses <- read_csv("math_tornado_ses.csv")
math_tornado_eth <- read_csv("math_tornado_eth.csv")

final_math_tornado <- bind_rows(list(math_tornado_race, math_tornado_ses, math_tornado_eth))

final_math_tornado %>%
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
  write_csv("math_tornado/final_math_tornado_2024.csv")

# Not done: ELL Simulation ----------------------------------------------------------

## ELL simulation may not be reasonably accurate. we will see.
# there is ALOT of data missing here
# ell_data %>%
#   mutate(missing = case_when(
#     is.na(pass_count) ~ 1,
#     TRUE ~ 0
#   )) %>%
#   group_by(test_year, level) %>%
#   summarize(missing = mean(missing)) %>%
#   spread(test_year, missing)
# 
# ell_data %>%
#   filter(is.na(pass_count)) %>%
#   pull(division_name) %>%
#   unique()
# 
# ell_data[is.na(ell_data)] <- 0 
# ell_data$total_count[ell_data$pass_count == 0] <- 0  # If the numerator equals zero/is not measured then set the denominator to 0 as well to not be measured (make the data point missing as opposed to having noone pass the exam)
# 
# ## Make the simulated data
# names(ell_data)
# 
# ell_data_sim <- 
#   ell_data %>%
#   uncount(total_count) %>%
#   group_by(division_name, level, grade, pass_rate, test_year, cohort, pass_count) %>%
#   mutate(id = 1:n(),
#          pass = case_when(
#            id <= pass_count ~ 1,
#            TRUE ~ 0
#          )
#   )  
# 
# write_csv(ell_data_sim, file = "dataprep/data/2023_ell_data_sim.csv")
# 
# ell_data_sim <- read_csv("dataprep/data/2023_ell_data_sim.csv")
# 
# # Make nested datawws
# ell_data_sim_byyear <- ell_data_sim %>% 
#   group_by(test_year) %>% 
#   nest()
# 
# # estimate models
# ell_data_sim_byyear_models <- ell_data_sim_byyear %>% 
#   mutate(model = map(data, yearly_model))
# 
# # predict out
# ell_pred <- ell_data_sim_byyear_models %>% 
#   mutate(pred = map(model, ~ggpredict(.x, terms = c("level", "division_name"), type = "random")))
# 
# save(race_predicted, ses_predicted, ell_pred, file = "dataprep/data/2023_predicted_models.Rdata")