library(tidyverse)

rm(list = ls())

pre_ramp_stage_lengths <- c(60, 60*5, rep(60*3, 3))
ramp_stage_lengths <- rep(15, 61 - length(pre_ramp_stage_lengths))

stage_lengths <- c(pre_ramp_stage_lengths, ramp_stage_lengths)

stages <- tibble(stage = 1:61,
                 stage_length = stage_lengths)

stages <- stages %>% 
  mutate(time_start = cumsum(stage_lengths) - stage_lengths[1],
         time_end = cumsum(stage_lengths))
