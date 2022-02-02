library(tidyverse)
library(gasExchangeR)

rm(list = ls())

# does this work best as 2 functions? 1 calcultes the mrt and the other
# calculates a time adjusted value?

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

get_mrt <- function(.data, id, vt_data, front_cutoff = 1) {
  # browser()
  
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
             ex_time < vt_data[vt_data_test_idx, ]$at)
  
  mod <- lm(vo2 ~ ramp_speed + 1, data = mrt_ramp_section)
  
  mrt_speed <- .data %>% 
    filter(stage == 4) %>% 
    select(fixed_speeds) %>% 
    summarize(mrt_speed = get_mode(fixed_speeds)) %>% 
    pull()
  
  ramp_speed_mrt_vo2 <- (mrt_vo2 - mod$coefficients[1]) / mod$coefficients[2]
  speed_diff <- ramp_speed_mrt_vo2 - mrt_speed
  
  mrt <- (speed_diff / ramp_rate)
  
  return(mrt)
}

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

ids <- vts$id
mrt_data <- numeric(length = length(ids))

for(i in 1:length(test_list_raw)) {
  mrt_data[i] <- get_mrt(.data = test_list[[i]],
                          id = ids[i],
                          vt_data = vts,
                          front_cutoff = 1)
}

mrt_data

mrt_tib <- tibble(id = ids, time_point = "pre", mrt = mrt_data)
mrt_tib

write_csv(mrt_tib, "data/processed/mrt.csv")

