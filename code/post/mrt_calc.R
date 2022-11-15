library(tidyverse)
library(gasExchangeR)

rm(list = ls())

# does this work best as 2 functions? 1 calculates the mrt and the other
# calculates a time adjusted value?

source("code/computations/get_mrt.R")

cpet_hr_files <- list.files("data/2022/post/processed/cpet_hr/", full.names = TRUE)
cpet_no_hr_files <- list.files("data/2022/post/processed/cpet_csv/", full.names = TRUE)

# due to some HR data collection issues, not all CPETs had HR data.
# This isolates those tests.
cpet_only_files <-
  cpet_no_hr_files[!(basename(cpet_no_hr_files) %in% basename(cpet_hr_files))]

file_list <- c(cpet_hr_files,cpet_only_files) # combine file lists together

test_list_raw <- vector(mod = "list", length = length(file_list))

for(i in 1:length(file_list)) {
  test_list_raw[[i]] <- read_csv(file_list[i], show_col_types = FALSE)
}

test_list <- vector(mod = "list", length = length(test_list_raw))

test_list <- map(test_list_raw, avg_exercise_test,
    type = "breath",
    subtype = "rolling",
    time_col = "ex_time",
    roll_window = 7,
    roll_trim = 2)

vts_raw <- read_csv("data/2022/post/raw/threshold_data.csv",
                    show_col_types = FALSE)

# cleans up the times in the spreadsheet with AT and RC data
vts <- vts_raw %>% 
  mutate(at_time = lubridate::hms(at_time),
         at_time = lubridate::hour(at_time) + (lubridate::minute(at_time)/60)) %>% 
  mutate(rc_time = lubridate::hms(rc_time),
         rc_time = lubridate::hour(rc_time) + (lubridate::minute(rc_time)/60))

mrt_data <- numeric(length = length(file_list))

for(i in 1:length(mrt_data)) {
  id = file_list[[i]] %>% 
    str_extract("mar\\d{2}_\\d{3}")
  
  mrt_data[i] <- get_mrt(.data = test_list[[i]],
                         id = id,
                         vt_data = vts,
                         vt1_col = "at_time",
                         front_cutoff = 1) # cutoff the first min of data
}

mrt_data

ids = str_extract(file_list, "mar\\d{2}_\\d{3}")

mrt_tib <- tibble(id = ids, time_point = "post", mrt = mrt_data) %>% 
  arrange(id)

write_csv(mrt_tib, "data/2022/post/processed/mrt.csv")

