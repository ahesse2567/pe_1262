library(tidyverse)

add_stages <- function(.df, time_col = "ex_time") {
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
  # browser()
  stage_num <- integer(length = nrow(.df))
  for(i in 1:nrow(.df)) {
    idx <- which((.df[[time_col]][i] >= stages$time_start) & 
                   (.df[[time_col]][i] < stages$time_end))
    stage_num[i] <- stages$stage[idx]
  }
  
  .df <- .df %>% 
    mutate(stage = stage_num)
  
  .df
  
}
