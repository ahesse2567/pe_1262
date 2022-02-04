library(tidyverse)

bind_hr_data <- function(id, time_point) {
  id <- as.character(id)
  browser()
  file_list_hrv <- list.files("data/labeled_hrv/", full.names = TRUE)
  f_name_hrv <- file_list_hrv[str_which(file_list_hrv, id)]
  hr_data <- read_delim(file = f_name_hrv,
                       delim = "/t",
                       col_names = FALSE,
                       show_col_types = FALSE) %>% 
    rename(rr = X1)
  
  clock_start <- str_extract(f_name_hrv, pattern = "\\s\\d{2}-\\d{2}-\\d{2}")
  clock_start <- str_remove(clock_start, pattern = "\\s")
  
  clock_hr <- str_split(clock_start, "-")[[1]][[1]] %>% as.numeric()
  clock_min <- str_split(clock_start, "-")[[1]][[2]] %>% as.numeric()
  clock_sec <- str_split(clock_start, "-")[[1]][[3]] %>% as.numeric()
  
  clock_time <- hms(clock_sec, clock_min, clock_hr)
  
  hr_data <- hr_data %>% 
    mutate(elapsed_time = cumsum(rr)/1000, 
           elapsed_time = hms(seconds = elapsed_time), 
           clock_time = as_hms(clock_time + elapsed_time), #convert from difftime to hms
           clock_time = round(clock_time, 0), #round times to allow merging with gas exchange data
           clock_time = as_hms(clock_time), #convert from difftime to hms
           hr = 60/(rr/1000)) %>% 
    group_by(clock_time) %>% #Heart beats can occur multiple times per second, but that's more frequent than data in Breeze
    summarise(clock_time = mean(clock_time), hr = mean(hr), elapsed_time = mean(elapsed_time)) %>% #Average data down to allow for merge with Breeze
    mutate(clock_time = as_hms(clock_time)) #switch clock_time back into hms
  
  test_list <- list.files("data/processed/fixed_speeds_grades/",
                          full.names = TRUE)
  id_tp <- paste0(id, "_", time_point)
  test_name <- test_list[str_which(test_list, id_tp)]
  
  test <- read_csv(test_name)
  
  # Merges data frames and keeps all gas exchange data
  merged_test <- merge(test, hr_data, by = "clock_time", all.x = TRUE) %>% 
    as_tibble() %>% 
    select(-elapsed_time)
  
  f_name_out <- paste0("./data/processed/final/mar22_",
                       id, "_", time_point, ".csv")
  write_csv(merged_test, file = f_name_out)
}
