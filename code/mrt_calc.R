library(tidyverse)
library(gasExchangeR)

rm(list = ls())

# does this work best as 2 functions? 1 calcultes the mrt and the other
# calculates a time adjusted value?

source("code/get_mrt.R")

file_list <- list.files("data/processed/fixed_speeds_grades/",
                        full.names = TRUE)

test_list_raw <- vector(mod = "list", length = length(file_list))

for(i in 1:length(file_list)) {
  test_list_raw[[i]] <- read_csv(file_list[i])
}

test_list <- vector(mod = "list", length = length(test_list_raw))

test_list <- map(test_list_raw, avg_exercise_test,
    type = "breath",
    subtype = "rolling",
    time_col = "ex_time",
    roll_window = 7,
    trim = 2)

vts_raw <- read_csv("./data/threshold_data.csv", show_col_types = FALSE) %>% 
  drop_na() #some people ran the two mile but didn't complete an exercise test

#cleans up the times in the spreadsheet with AT and RC data
vts <- vts_raw %>% 
  mutate(at = lubridate::hms(at),
         at = lubridate::hour(at) + (lubridate::minute(at)/60)) %>% 
  mutate(rc = lubridate::hms(rc),
         rc = lubridate::hour(rc) + (lubridate::minute(rc)/60))

ids <- vts$id
mrt_data <- numeric(length = length(ids))

for(i in 1:length(mrt_data)) {
  mrt_data[i] <- get_mrt(.data = test_list[[i]],
                          id = ids[i],
                          vt_data = vts,
                          front_cutoff = 2)
}

mrt_data

mrt_tib <- tibble(id = ids, time_point = "pre", mrt = mrt_data)
mrt_tib

write_csv(mrt_tib, "data/processed/mrt.csv")

