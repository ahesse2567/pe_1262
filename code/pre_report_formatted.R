library(tidyverse)

pre_report <- read_csv("data/processed/pre_report_manual.csv") %>% 
  select(-1)

long <- pre_report %>% 
  select(-c(time_2mi, speed_2mi, speed_2mi_ss, at, rc)) %>% 
  pivot_longer(cols = contains(c("at", "rc", "vo2"))) %>% 
  group_split(id)


pre_report %>% 
  select(-c(time_2mi, speed_2mi, speed_2mi_ss, at, rc)) %>% 
  pivot_longer(cols = contains("at")) %>% View
  pivot_longer(cols = contains("rc"))

long[[1]] %>% 
  pivot_wider(id_cols = contains("rc"),
              values_from = value)
