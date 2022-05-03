library(tidyverse)
library(gasExchangeR)

# does this work best as 2 functions? 1 calcultes the mrt and the other
# calculates a time adjusted value?

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

get_mrt <- function(.data,
                    id,
                    vt_data,
                    vt1_col = "at",
                    front_cutoff = 1,
                    neg_mrt = 0.5) {
  
  rec2_speed <- .data %>% 
    filter(stage == 5) %>% 
    select(fixed_speeds) %>% 
    summarize(start_speed = get_mode(fixed_speeds)) %>% 
    pull()
  max_speed <- max(.data$fixed_speeds)
  ramp_rate <- (max_speed - rec2_speed) / 8 # ramp lasts 8 minutes
  
  min_ramp_time <- .data %>% 
    filter(stage >= 6) %>% 
    select(ex_time) %>% 
    min()
  
  .data <- .data %>% 
    mutate(ramp_time = ex_time - min_ramp_time,
           ramp_speed = ramp_rate * ramp_time + rec2_speed)
  
  mrt_vo2 <- .data %>% 
    filter(stage == 4) %>% 
    filter(ex_time >= max(ex_time) - 0.5) %>% # take average of last 30 s
    summarize(avg_vo2 = mean(vo2)) %>% 
    pull()
  
  vt_data_test_idx <- which(vt_data$id == id)
  
  mrt_ramp_section <- .data %>%
    filter(ex_time >= (min_ramp_time + front_cutoff) & 
             ex_time < vt_data[vt_data_test_idx,][[vt1_col]])
  
  if(dim(mrt_ramp_section)[1] == 0) {
    return(NA)
  }
  
  mod <- lm(vo2 ~ ramp_speed + 1, data = mrt_ramp_section)
  
  mrt_speed <- .data %>% 
    filter(stage == 4) %>% 
    select(fixed_speeds) %>% 
    summarize(mrt_speed = get_mode(fixed_speeds)) %>% 
    pull()
  
  ramp_speed_mrt_vo2 <- (mrt_vo2 - mod$coefficients[1]) / mod$coefficients[2]
  speed_diff <- ramp_speed_mrt_vo2 - mrt_speed
  
  mrt <- (speed_diff / ramp_rate)
  if(mrt < 0) {
    mrt <- 0.5
  }
  
  mrt
}
