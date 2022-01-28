library(tidyverse)

vts <- read_csv("./data/threshold_data.csv") %>% 
  drop_na() #some people ran the two mile but didn't complete an exercise test

vts <- vts %>% 
  mutate(at = lubridate::hms(at),
         at = lubridate::hour(at) + (lubridate::minute(at)/60)) %>% 
  mutate(rc = lubridate::hms(rc),
         rc = lubridate::hour(rc) + (lubridate::minute(rc)/60))
