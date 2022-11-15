library(tidyverse)
library(gasExchangeR)

rm(list = ls())

source("code/get_mrt.R")

test_raw <- read_csv("data/processed/fixed_speeds_grades/mar22_105_pre.csv")
test <- avg_exercise_test(test_raw, type = "breath", subtype = "rolling",
                          time_col = "ex_time", roll_window = 7, trim = 2)

rec2_speed <- test %>% 
  filter(stage == 5) %>% 
  select(fixed_speeds) %>% 
  summarize(start_speed = get_mode(fixed_speeds)) %>% 
  pull()

max_speed <- max(test$fixed_speeds)
ramp_rate <- (max_speed - rec2_speed) / 8 # ramp lasts 8 minutes

min_ramp_time <- test %>% 
  filter(stage >= 6) %>% 
  select(ex_time) %>% 
  min()

test <- test %>% 
  mutate(ramp_time = ex_time - min_ramp_time,
         ramp_speed = ramp_rate * ramp_time + rec2_speed)

vts_raw <- read_csv("./data/threshold_data.csv", show_col_types = FALSE) %>% 
  drop_na() #some people ran the two mile but didn't complete an exercise test

#cleans up the times in the spreadsheet with AT and RC data
vts <- vts_raw %>% 
  mutate(at = lubridate::hms(at),
         at = lubridate::hour(at) + (lubridate::minute(at)/60)) %>% 
  mutate(rc = lubridate::hms(rc),
         rc = lubridate::hour(rc) + (lubridate::minute(rc)/60))

front_cutoff = 1
mrt <- get_mrt(test, id = 105, vt_data = vts, front_cutoff = front_cutoff)
mrt

mar22_105_pre <- vts[vts["id"] == 105,]

vt1_mrt_ex_time <- mar22_105_pre$at - mrt
vt1_mrt_idx <- which.min(abs(test$ex_time - vt1_mrt_ex_time))

speed_vt1 <- round(test$ramp_speed[vt1_mrt_idx],1)

speed_vt1^-1*60
round(((speed_vt1^-1*60) %% 1)* 60, 0)


id <- 105
vt_data_test_idx <- which(vts$id == id)

mrt_ramp_section <- test %>%
  dplyr::filter(ex_time >= (min_ramp_time + front_cutoff) & 
           ex_time < vts[vt_data_test_idx, ]$at)

mrt_vo2 <- test %>% 
  filter(stage == 4) %>% 
  filter(ex_time >= max(ex_time) - 0.5) %>% # take average of last 30 s
  summarize(avg_vo2 = mean(vo2)) %>% 
  pull()

mod <- lm(vo2 ~ ramp_speed + 1, data = mrt_ramp_section)

mrt_speed <- test %>% 
  filter(stage == 4) %>% 
  select(fixed_speeds) %>% 
  summarize(mrt_speed = get_mode(fixed_speeds)) %>% 
  pull()

ramp_speed_mrt_vo2 <- (mrt_vo2 - mod$coefficients[1]) / mod$coefficients[2]
speed_diff <- ramp_speed_mrt_vo2 - mrt_speed
(speed_diff / ramp_rate) * 60

ggplot(data = mrt_ramp_section,
       aes(x = ramp_speed, y = vo2)) +
  geom_point() +
  geom_hline(yintercept = mrt_vo2, color = "red") +
  geom_vline(xintercept = mrt_speed, color = "red") +
  geom_vline(xintercept = ramp_speed_mrt_vo2) +
  geom_vline(xintercept = speed_vt1, color = "green") +
  geom_smooth(data= mrt_ramp_section, method = "lm", se = FALSE) +
  theme_bw()

# ggplot(data = test[test["stage"] >= 6 & test["stage"] <= 37,],
#        aes(x = ramp_speed, y = vo2)) +
#   geom_point() +
#   geom_hline(yintercept = mrt_vo2, color = "red") +
#   geom_vline(xintercept = mrt_speed, color = "red") +
#   geom_vline(xintercept = ramp_speed_mrt_vo2) +
#   geom_vline(xintercept = speed_vt1, color = "green") +
#   geom_smooth(data= mrt_ramp_section, method = "lm", se = FALSE) +
#   # geom_text(x = quantile(mrt_ramp_105_pre$ramp_speed, 0.2),
#   #           y = quantile(mrt_ramp_105_pre$vo2, 0.6),
#   #           label = paste("Speed at VT1:",
#   #                         speed_vt1,
#   #                         "mph\nPace at VT1:",
#   #                         round(speed_vt1^-1*60,0),
#   #                         "min",
#   #                         round(((speed_vt1^-1*60) %% 1)* 60,0),
#   #                         "sec/mi")) +
#   theme_bw()
