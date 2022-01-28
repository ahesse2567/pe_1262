library(tidyverse)

vts <- read_csv("./data/threshold_data.csv") %>% 
  drop_na() #some people ran the two mile but didn't complete an exercise test

vts <- vts %>% 
  mutate(at = lubridate::hms(at),
         at = lubridate::hour(at) + (lubridate::minute(at)/60)) %>% 
  mutate(rc = lubridate::hms(rc),
         rc = lubridate::hour(rc) + (lubridate::minute(rc)/60))

test <- read_csv("data/processed/stages/mar22_105_pre.csv")

mar22_105 <- vts[1,]

test$increment <- mar22_105$speed_2mi

fixed_speeds <- case_when(test$stage == 1 ~ 3,
          test$stage == 2 ~ round(mar22_105$speed_2mi*.75,1),
          test$stage == 3 ~ round(mar22_105$speed_2mi*0.3,1),
          test$stage == 4 ~ round(mar22_105$speed_2mi*0.635,1),
          test$stage == 5 ~ round(mar22_105$speed_2mi*0.45,1),
          test$stage %in% c(6:37) ~ 
            round((mar22_105$speed_2mi - mar22_105$speed_2mi*0.45) / 32 * (test$stage - 5) +
            mar22_105$speed_2mi*0.45, 1),
          test$stage > 37 ~ round(mar22_105$speed_2mi, 1)
          )
fixed_speeds
fixed_grades <- case_when(test$stage < 38 ~ 1,
                          test$stage >= 38 ~ 1 + (test$stage - 37) * 0.5)
fixed_grades
test %>% 
  mutate(fixed_speeds = fixed_speeds,
         fixed_grades = fixed_grades) %>% 
  select(contains("speed"),
         contains("grade"))

test %>% View

(mar22_105$speed_2mi - mar22_105$speed_2mi*0.45) / 32 * (test$stage - 5) +
  mar22_105$speed_2mi*0.45

stages <- 1:61

numerator <- (mar22_105$speed_2mi - mar22_105$speed_2mi*0.45)
denominator <- num_stages

numerator / denominator

round(numerator / denominator * (stages - 5) + mar22_105$speed_2mi*0.45, 1)

round(stage15 + mar22_105$speed_2mi*0.45, 1)
