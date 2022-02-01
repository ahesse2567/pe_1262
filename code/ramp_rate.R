library(tidyverse)
library(gasExchangeR)

rm(list = ls())

test_raw <- read_csv("data/processed/fixed_speeds_grades/mar22_105_pre.csv")

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

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

idx <- which.min(abs(test$ramp_speed - ramp_speed_mrt_vo2))
# index of where the mrt vo2 and the vo2 vs. speed regression cross
# now need to go backwards by the mrt (~31 seconds in this example)
# need to use work rate eqution?


ggplot(data = mrt_ramp_105_pre, aes(x = ramp_speed, y = vo2)) +
  geom_point() +
  geom_hline(yintercept = mrt_vo2, color = "red") +
  geom_vline(xintercept = mrt_speed, color = "red") +
  geom_vline(xintercept = ramp_speed_mrt_vo2) +
  geom_smooth(data= lm_data, method = "lm", se = FALSE) +
  theme_bw()

