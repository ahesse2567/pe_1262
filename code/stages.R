library(tidyverse)

rm(list = ls())

pre_ramp_stage_lengths <- c(60, 60*5, rep(60*3, 3))
ramp_stage_lengths <- rep(15, 61 - length(pre_ramp_stage_lengths))

stage_lengths <- c(pre_ramp_stage_lengths, ramp_stage_lengths)

stages <- tibble(stage = 1:61,
                 stage_length = stage_lengths)

stages <- stages %>% 
  mutate(time_start = cumsum(stage_lengths) - stage_lengths[1],
         time_end = cumsum(stage_lengths)) %>% 
  mutate(stage_length = stage_length/60,
         time_start = time_start/60,
         time_end = time_start/60)

test <- read_csv("data/processed/mar22_105_pre.csv")

get_stage <- function(.x) {
  # make stages first
  pre_ramp_stage_lengths <- c(60, 60*5, rep(60*3, 3))
  ramp_stage_lengths <- rep(15, 61 - length(pre_ramp_stage_lengths))
  
  stage_lengths <- c(pre_ramp_stage_lengths, ramp_stage_lengths)
  
  stages <- tibble(stage = 1:61,
                   stage_length = stage_lengths)
  
  stages <- stages %>% 
    mutate(time_end = cumsum(stage_lengths),
           time_start = dplyr::lag(time_end, default = 0)) %>% 
    mutate(stage_length = stage_length/60,
           time_start = time_start/60,
           time_end = time_end/60)
  ts <- stages[["time_start"]] - .x
  ts <- stages[["time_end"]] - .x
  
}

(test$ex_time >= stages$time_start) & (test$ex_time < stages$time_end)

stage_num <- integer(length = nrow(test))
for(i in 1:nrow(test)) {
  idx <- which((test$ex_time[i] >= stages$time_start) & 
                 (test$ex_time[i] < stages$time_end))
  stage_num[i] <- stages$stage[idx]
}

test %>% 
  mutate(stage = stage_num) %>% 
  select(speed, grade, vo2, stage, ex_time) %>% View
