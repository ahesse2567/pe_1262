library(tidyverse)
library(progress)

rm(list = ls())

source("code/tidying/add_hr_func.R")
source("code/tidying/check_create_folder.R")

check_create_folder(year = 2022, time_point = "post", data_type = "cpet_hr")

file_list_hrv <- list.files("data/2022/post/processed/hrv/")

ids <- character(length = length(file_list_hrv))
for(i in 1:length(ids)) {
  ids[i] <- str_extract(file_list_hrv[i],"(?<=mar\\d{2}_)\\d{3}(?=_)")
}

pb <- progress_bar$new(total = length(ids))
pb$tick(0)
for(i in 1:length(ids)) {
  bind_hr_data(id = ids[i], year = 2022, time_point = "post")
  pb$tick()
}

