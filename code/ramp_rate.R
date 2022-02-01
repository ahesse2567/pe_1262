library(tidyverse)
library(gasExchangeR)

rm(list = ls())

test_raw <- read_csv("data/processed/fixed_speeds_grades/mar22_105_pre.csv")

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rec2_speed <- test_raw %>% 
  filter(stage == 5) %>% 
  select(fixed_speeds) %>% 
  summarize(start_speed = get_mode(fixed_speeds)) %>% 
  pull()

max_speed <- max(test_raw$fixed_speeds)

ramp_rate <- (max_speed - rec2_speed) / 8 # ramp lasts 8 minutes

test <- avg_exercise_test(test_raw, type = "breath", subtype = "rolling",
                  time_col = "ex_time", roll_window = 7, trim = 2)

ggplot(data = test, aes(x = ex_time, y = fixed_speeds)) +
  geom_point()

test <- test %>% 
  mutate(phase = case_when(stage < 6 ~ "pre-ramp",
                           stage >= 6 ~ "ramp"))

min_ramp_time <- test %>% 
  filter(phase == "ramp") %>% 
  select(ex_time) %>% 
  min()

test <- test %>% 
  mutate(ramp_time = ex_time - min_ramp_time,
         ramp_speed = ramp_rate * ramp_time + rec2_speed)

test["phase"] == "ramp"

ggplot(data = test[test["phase"] == "ramp",],
       aes(x = ex_time, y = fixed_speeds)) +
  geom_point() +
  geom_line(aes(y = ramp_speed)) +
  theme_bw()
