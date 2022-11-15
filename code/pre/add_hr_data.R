library(tidyverse); library(hms)

rm(list = ls())

source("code/add_hr_func.R")

file_list_hrv <- list.files("data/labeled_hrv/")

ids <- character(length = length(file_list_hrv))

for(i in 1:length(ids)) {
  ids[i] <- str_extract(file_list_hrv[i],"\\d{3}")
}

for(i in 1:length(ids)) {
  bind_hr_data(id = ids[i], time_point = "pre")
}

