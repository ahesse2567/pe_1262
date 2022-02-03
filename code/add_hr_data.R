library(tidyverse); library(hms)

hr_data <- read_delim(file = "./data/hrv/2022-01-21 09-06-13.txt", delim = "/n", col_names = FALSE, show_col_types = FALSE) %>% 
  rename(rr = X1)

file_name <- "2022-01-21 09-06-13.txt"

clock_start <- str_extract(file_name, pattern = "\\s\\d{2}-\\d{2}-\\d{2}")
clock_start <- str_remove(clock_start, pattern = "\\s")

clock_hr <- str_split(clock_start, "-")[[1]][[1]] %>% as.numeric()
clock_min <- str_split(clock_start, "-")[[1]][[2]] %>% as.numeric()
clock_sec <- str_split(clock_start, "-")[[1]][[3]] %>% as.numeric()

clock_time <- hms(clock_sec, clock_min, clock_hr)

hr_data %>% 
  mutate(elapsed_time = cumsum(rr)/1000, 
         elapsed_time = hms(seconds = elapsed_time), 
         clock_time = as_hms(clock_time + elapsed_time), #convert from difftime to hms
         clock_time = round(clock_time, 0), #round times to allow merging with gas exchange data
         clock_time = as_hms(clock_time), #convert from difftime to hms
         hr = 60/(rr/1000)) %>% 
  select(-rr, -elapsed_time) %>% #not needed
  group_by(clock_time) %>% #Heart beats can occur multiple times per second, but that's more frequent than data in Breeze
  summarise(clock_time = mean(clock_time), hr = mean(hr)) %>% #Average data down to allow for merge with Breeze
  mutate(clock_time = as_hms(clock_time)) #switch clock_time back into hms




  




