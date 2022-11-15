library(tidyverse)

rm(list = ls())
source("code/remove_event_markers.R")

file_list <- list.files("data/raw", full.names = TRUE)
file_list

for(f in file_list) {
  clean_test(f)
}
