library(tidyverse)
library(stringr)
library(lubridate)
library(gasExchangeR)

rm(list = ls())
source("code/tidying/tidy_gxt.R")
source("code/tidying/tidy_gxt.R")
source("code/tidying/add_stages.R")
source("code/tidying/correct_speed_grade.R")

file_list <- list.files("data/post/raw/cpet", full.names = TRUE)

ids <- str_extract(file_list, "mar\\d{2}_\\d{3}")
new_names <- paste0("data/post/processed/cpet_csv/", ids, "_post.csv")

vts_raw <- read_csv("data/post/raw/threshold_data.csv", show_col_types = FALSE)

vts <- vts_raw %>% 
  mutate(at_time = lubridate::hms(at_time),
         at_time = lubridate::hour(at_time) + (lubridate::minute(at_time)/60)) %>% 
  mutate(rc_time = lubridate::hms(rc_time),
         rc_time = lubridate::hour(rc_time) + (lubridate::minute(rc_time)/60))

for(i in 1:length(file_list)) {
  lines <- readLines(file_list[i])
  temp_df = read.table(textConnection(lines[-2]),
                       header = TRUE,
                       sep="\t",
                       na.strings = c("NA", "", " ")) %>% 
    tidy_gxt_rows() %>% 
    tidy_gxt_cols() %>% 
    add_stages() %>% 
    correct_speeds_grades(id = ids[i], vt_data = vts, time_point = "post")
  
  write_csv(temp_df, file = new_names[i], progress = FALSE)
}
