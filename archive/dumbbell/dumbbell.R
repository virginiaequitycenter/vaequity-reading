##New Tornado/Dumbbell Plot

install.packages("plotly")

library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)

##dumbbell plotly/ggplot

tornado <- read_csv("assets/data/2023_tornado.csv")

dumbbell_race <- tornado %>% 
  filter(demographic == "Race", test_year == 2022) %>% 
  pivot_wider(names_from = level, values_from = rate) %>% 
  ggplot(aes(group = fct_reorder(division_name, desc(rank)))) +
  geom_segment(aes(y = fct_reorder(division_name, desc(rank)), yend = fct_reorder(division_name, desc(rank)), x = black, xend = white), color = "grey") +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=black), color = 'blue', size = 2) +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=white), color = 'yellow', size = 2) +
  labs(x ="", y = "School Division") +  
  theme_classic() +
  theme(axis.text.y = element_text(size = 2))



dumbbell_disadvantaged <- tornado %>% 
  filter(demographic == "Socioeconomics", test_year == 2022) %>% 
  pivot_wider(names_from = level, values_from = rate) %>% 
  ggplot(aes(group = fct_reorder(division_name, desc(rank)))) +
  geom_segment(aes(y = fct_reorder(division_name, desc(rank)), yend = fct_reorder(division_name, desc(rank)), x = disadvantaged, xend = other), color = "grey") +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=disadvantaged), color = 'blue', size = 2) +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=other), color = 'yellow', size = 2) +
  labs(x ="", y = "School Division") +  
  theme_classic() +
  theme(axis.text.y = element_text(size = 2))

ggplotly(dumbbell_disadvantaged)


dumbbell_hispanic <- tornado %>% 
  filter(demographic == "Ethnicity", test_year == 2022) %>% 
  pivot_wider(names_from = level, values_from = rate) %>% 
  ggplot(aes(group = fct_reorder(division_name, desc(rank)))) +
  geom_segment(aes(y = fct_reorder(division_name, desc(rank)), yend = fct_reorder(division_name, desc(rank)), x = hispanic, xend = white), color = "grey") +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=hispanic), color = 'blue', size = 2) +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=white), color = 'yellow', size = 2) +
  labs(x ="", y = "School Division") +  
  theme_classic() +
  theme(axis.text.y = element_text(size = 2))

ggplotly(dumbbell_hispanic)


chart_race <- tornado %>% 
  filter(demographic == "Race") %>% 
  pivot_wider(names_from = level, values_from = rate) %>% 
  ggplot(aes(group = fct_reorder(division_name, desc(rank)))) +
  geom_segment(aes(y = fct_reorder(division_name, desc(rank)), yend = fct_reorder(division_name, desc(rank)), x = black, xend = white), color = "grey") +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=black), color = 'blue', size = 2) +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=white), color = 'yellow', size = 2) +
  facet_wrap(~ test_year) +
  labs(x ="", y = "") +  
  theme_classic() +
  theme(axis.text.y = element_text(size = 2))

ggplotly(chart_race)




### old charts from Michele 2023-04-17-----------------------------------------
tdf %>% 
  filter(demographic == "Race", test_year == 2022) %>% 
  pivot_wider(names_from = level, values_from = rate) %>% 
  ggplot(aes(group = fct_reorder(division_name, desc(rank)))) +
  geom_segment(aes(y = fct_reorder(division_name, desc(rank)), yend = fct_reorder(division_name, desc(rank)), x = black, xend = white), color = "grey") +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=black), color = 'orange', size = .01) +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=white), color = 'blue', size = .01) +
  #facet_wrap(~ test_year) +
  labs(x ="", y = "") +  
  theme_classic() +
  theme(axis.text.y = element_text(size = 2))


tdf %>% 
  filter(demographic == "Race") %>% 
  pivot_wider(names_from = level, values_from = rate) %>% 
  ggplot(aes(group = fct_reorder(division_name, desc(rank)))) +
  geom_segment(aes(y = fct_reorder(division_name, desc(rank)), yend = fct_reorder(division_name, desc(rank)), x = black, xend = white), color = "grey") +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=black), color = 'orange', size = .01) +
  geom_point(aes(y = fct_reorder(division_name, desc(rank)), x=white), color = 'blue', size = .01) +
  facet_wrap(~ test_year) +
  labs(x ="", y = "") +  
  theme_classic() +
  theme(axis.text.y = element_text(size = 2))