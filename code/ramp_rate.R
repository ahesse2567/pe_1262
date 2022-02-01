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
           ramp_speed = ramp_rate * ramp_time + start_speed)
  
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
  
  mrt_speed <- test %>% 
    filter(stage == 4) %>% 
    select(fixed_speeds) %>% 
    summarize(mrt_speed = get_mode(fixed_speeds)) %>% 
    pull()
  
  ramp_speed_mrt_vo2 <- (mrt_vo2 - mod$coefficients[1]) / mod$coefficients[2]
  speed_diff <- ramp_speed_mrt_vo2 - mrt_speed
  
  mrt <- (speed_diff / ramp_rate)
  
  return(mrt)
}

test_raw <- read_csv("data/processed/fixed_speeds_grades/mar22_105_pre.csv")
test <- avg_exercise_test(test_raw, type = "breath", subtype = "rolling",
                          time_col = "ex_time", roll_window = 7, trim = 2)

get_mrt(test, id = 105, vt_data = vts, front_cutoff = 1)

# technically start_speed should be the speed at the second recovery pace
# but it seems to give answers that are too right shifted
rec2_speed <- test_raw %>% 
  filter(stage == 5) %>% 
  select(fixed_speeds) %>% 
  summarize(start_speed = get_mode(fixed_speeds)) %>% 
  pull()

ramp_start_speed <- test_raw %>% 
  filter(stage == 6) %>% 
  select(fixed_speeds) %>% 
  summarize(start_speed = get_mode(fixed_speeds)) %>% 
  pull()

# start_speed <- mean(c(rec2_speed, ramp_start_speed))
start_speed <- rec2_speed

max_speed <- max(test_raw$fixed_speeds)

ramp_rate <- (max_speed - rec2_speed) / 8 # ramp lasts 8 minutes

test <- avg_exercise_test(test_raw, type = "breath", subtype = "rolling",
                  time_col = "ex_time", roll_window = 7, trim = 2)

test <- test %>% 
  mutate(phase = case_when(stage == 1 ~ "warm-up",
                           stage == 2 ~ "marathon_pace_ss",
                           stage == 3 ~ "rec1",
                           stage == 4 ~ "mrt_ss",
                           stage == 5 ~ "rec2",
                           stage >= 6 ~ "ramp"))

min_ramp_time <- test %>% 
  filter(phase == "ramp") %>% 
  select(ex_time) %>% 
  min()

test <- test %>% 
  mutate(ramp_time = ex_time - min_ramp_time,
         ramp_speed = ramp_rate * ramp_time + start_speed)

ggplot(data = test[test["stage"] >= 5,],
       aes(x = ex_time, y = fixed_speeds)) +
  geom_point(aes(color = grade)) +
  geom_line(aes(y = ramp_speed)) +
  theme_bw()

# test %>% 
#   filter(stage >= 5) %>% 
#   View

mrt_vo2 <- test %>% 
  filter(phase == "mrt_ss") %>% 
  filter(ex_time >= max(ex_time) - 0.5) %>% 
  summarize(avg_vo2 = mean(vo2)) %>% 
  pull()

vts_raw <- read_csv("./data/threshold_data.csv", show_col_types = FALSE) %>% 
  drop_na() #some people ran the two mile but didn't complete an exercise test

#cleans up the times in the spreadsheet with AT and RC data
vts <- vts_raw %>% 
  mutate(at = lubridate::hms(at),
         at = lubridate::hour(at) + (lubridate::minute(at)/60)) %>% 
  mutate(rc = lubridate::hms(rc),
         rc = lubridate::hour(rc) + (lubridate::minute(rc)/60))

mar22_105_pre <- vts[vts["id"] == 105,]

mrt_ramp_105_pre <- test %>% 
  filter(ex_time >= min_ramp_time & ex_time < mar22_105_pre$at)

# remove first minute or ramp data b/c they're probably walking
lm_data <- mrt_ramp_105_pre[mrt_ramp_105_pre["ramp_time"] >= 1,]

mod <- lm(vo2 ~ ramp_speed + 1, data = lm_data)

mrt_speed <- test %>% 
  filter(phase == "mrt_ss") %>% 
  select(fixed_speeds) %>% 
  summarize(mrt_speed = get_mode(fixed_speeds)) %>% 
  pull()

ramp_speed_mrt_vo2 <- (mrt_vo2 - mod$coefficients[1]) / mod$coefficients[2]
speed_diff <- ramp_speed_mrt_vo2 - mrt_speed

mrt <- (speed_diff / ramp_rate)
mrt

vt1_mrt_ex_time <- mar22_105_pre$at - mrt
vt1_mrt_idx <- which.min(abs(test$ex_time - vt1_mrt_ex_time))

speed_vt1^-1*60
round(((speed_vt1^-1*60) %% 1)* 60,0)

ggplot(data = mrt_ramp_105_pre, aes(x = ramp_speed, y = vo2)) +
  geom_point() +
  geom_hline(yintercept = mrt_vo2, color = "red") +
  geom_vline(xintercept = mrt_speed, color = "red") +
  geom_vline(xintercept = ramp_speed_mrt_vo2) +
  geom_vline(xintercept = speed_vt1, color = "green") +
  geom_smooth(data= lm_data, method = "lm", se = FALSE) +
  geom_text(x = quantile(mrt_ramp_105_pre$ramp_speed, 0.2),
            y = quantile(mrt_ramp_105_pre$vo2, 0.6),
            label = paste("Speed at VT1:",
                          speed_vt1,
                          "mph\nPace at VT1:",
                          round(speed_vt1^-1*60,0),
                          "min",
                          round(((speed_vt1^-1*60) %% 1)* 60,0),
                          "sec/mi")) +
  theme_bw()

