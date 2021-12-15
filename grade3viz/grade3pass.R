library(tidyverse)
library(janitor)
# library(lubridate)
library(plotly)

grade3 <- read_csv("www/thirdgrade_alllstudents.csv")

# Proficient ----
grade3_prof <- grade3 %>% 
  clean_names() %>% 
  select(school_year, division_number, division_name, 
         pass_proficient_rate) %>% 
  mutate(year = str_sub(school_year, 6,10),
       # year = as.Date(year, "%Y"),
       # year = year(year),
       year = as.integer(year)) %>% 
  filter(year > 2012)

grade3_prof_all <- grade3 %>% 
  clean_names() %>% 
  group_by(school_year) %>% 
  summarize(totalpass = sum(pass_proficient_count, na.rm = TRUE),
            totalcount = sum(total_count, na.rm = TRUE)) %>% 
  mutate(pass_proficient_rate = round((totalpass/totalcount)* 100, 1)) %>% 
  mutate(year = str_sub(school_year, 6,10),
         # year = as.Date(year, "%Y"),
         # year = year(year),
         year = as.integer(year)) %>% 
  filter(year > 2012)
  
# ggplot(grade3_prof, aes(x = year, y = pass_proficient_rate, color = division_name)) + 
#   geom_line(data = grade3_pass_all, aes(x = year, y = pass_proficient_rate), 
#             color = "black", size = 2) +
#   geom_line() + 
#   guides(color = "none")

# With highlighted district
target_prof <- grade3_prof %>% filter(division_name == "Highland County") # choose target

# plot stock
g_prof <- grade3_prof %>% 
  ggplot(aes(x = year, y = pass_proficient_rate)) + 
  # bigger background line reflecting state-wide rate
  geom_line(data = grade3_prof_all, aes(x = year, y = pass_proficient_rate), 
            color = "black", size = 2) +
  # line traces for each jurisdiction in all panels
  geom_line(aes(group = division_name),
            size = 0.2, color = "grey70") +
  scale_x_continuous(name = "Year (spring)", breaks = seq(2013, 2021, 1)) +
  labs(y = "% Proficient") +
  # line trace in red for target jurisdiction 
  geom_line(data = target_prof, aes(y = pass_proficient_rate), color = "firebrick") +
  # rectangle over 2020 test
  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = 25, ymax = 85,
           alpha = 1, fill = "white") +
  annotate("text", x = 2020, y = 85, label = "No 2020 Test", size = 3) +
  # label state-wide rate
  annotate("segment", x = 2013.25, xend = 2014, y = 27, yend = 27, 
           color = "black", size = 2) +
  # annotate("text", x = 2014.74, y = 25, label = "State-wide Proficiency",
  #          size = 3) +
  theme_minimal()
ggplotly(g_prof) %>%
  layout(annotations = list(x = 2014.75, y = 25,
                            text = "State-wide Proficiency",
                            showarrow = F))

  
# Advanced ----
grade3_adv <- grade3 %>% 
  clean_names() %>% 
  select(school_year, division_number, division_name, 
         pass_advanced_rate) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.Date(year, "%Y"),
         year = year(year)) %>% 
  filter(year > 2012)

grade3_adv_all <- grade3 %>% 
  clean_names() %>% 
  group_by(school_year) %>% 
  summarize(totalpass = sum(pass_advanced_count, na.rm = TRUE),
            totalcount = sum(total_count, na.rm = TRUE)) %>% 
  mutate(pass_advanced_rate = round((totalpass/totalcount)* 100, 1)) %>% 
  mutate(year = str_sub(school_year, 6,10),
         year = as.Date(year, "%Y"),
         year = year(year)) %>% 
  filter(year > 2012)

# With highlighted district
target_adv <- grade3_adv %>% filter(division_name == "Charlottesville City") # choose target

# plot stock
g_adv <- grade3_adv %>% 
  ggplot(aes(x = year, y = pass_advanced_rate)) + 
  # bigger background line reflecting state-wide rate
  geom_line(data = grade3_adv_all, aes(x = year, y = pass_advanced_rate), 
            color = "black", size = 2) +
  # line traces for each jurisdiction in all panels
  geom_line(aes(group = division_name),
            size = 0.2, color = "grey70") +
  # line trace in red for target jurisdiction in panel
  geom_line(data = target_adv, aes(y = pass_advanced_rate), color = "firebrick") +
  scale_x_continuous(name = "Year (spring)", breaks = seq(2013, 2021, 1)) +
  labs(y = "% Advanced") +
  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = 0, ymax = 60,
           alpha = 1, fill = "white") +
  annotate("text", x = 2020, y = 60, label = "No 2020 Test", size = 3) +
  theme_minimal()
ggplotly(g_adv)

# NOTE: 2021 has ~ 20,000 fewer students taking test compared to 2019
