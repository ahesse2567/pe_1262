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
         clock_time = as_hms(clock_time + elapsed_time),
         hr = 60/(rr/1000))
  




