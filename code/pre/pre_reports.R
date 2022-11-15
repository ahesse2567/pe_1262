library(tidyverse)
library(gasExchangeR)

rm(list = ls())

file_list <- list.files("data/processed/final/",
                        full.names = TRUE)

test_list_raw <- vector(mod = "list", length = length(file_list))

for(i in 1:length(file_list)) {
  test_list_raw[[i]] <- read_csv(file_list[i], show_col_types = FALSE)
}

test_list <- vector(mod = "list", length = length(test_list_raw))

test_list <- map(test_list_raw, avg_exercise_test,
                 type = "breath",
                 subtype = "rolling",
                 time_col = "ex_time",
                 roll_window = 7,
                 trim = 2)

mrt_data <- read_csv("data/processed/mrt.csv")
mrt_data <- mrt_data %>% 
  mutate(mrt = case_when(mrt < 0 ~ 0, mrt >= 0 ~ mrt))


vts_raw <- read_csv("./data/threshold_data.csv", show_col_types = FALSE) %>% 
  drop_na() #some people ran the two mile but didn't complete an exercise test

#cleans up the times in the spreadsheet with AT and RC data
vts <- vts_raw %>% 
  mutate(at = lubridate::hms(at),
         at = lubridate::hour(at) + (lubridate::minute(at)/60)) %>% 
  mutate(rc = lubridate::hms(rc),
         rc = lubridate::hour(rc) + (lubridate::minute(rc)/60))


rer_vo2max <- numeric(length = length(test_list))
vo2max <- numeric(length = length(test_list))
hr_vo2max <- numeric(length = length(test_list))
max_hr <- numeric(length = length(test_list))

speed_at <- numeric(length = length(test_list))
rer_at <- numeric(length = length(test_list))
vo2_at <- numeric(length = length(test_list))
pct_vo2_at <- numeric(length = length(test_list))
hr_at <- numeric(length = length(test_list))

rer_rc <- numeric(length = length(test_list))
vo2_rc <- numeric(length = length(test_list))
pct_vo2_rc <- numeric(length = length(test_list))
hr_rc <- numeric(length = length(test_list))


for(i in 1:length(test_list)) {
  vo2max[i] <- round(max(test_list[[i]][["vo2"]]),1)
  vo2max_idx <- which.max(test_list[[i]][["vo2"]])
  
  rer_vo2max[i] <- test_list[[i]][["rer"]][vo2max_idx]
  
  at_mrt <- (vts[i,"at"] - mrt_data[i,"mrt"]) %>% 
    pull()
  at_mrt_idx <- which.min(abs(test_list[[i]][["ex_time"]] - at_mrt))
  speed_at[i] <- round(test_list[[i]][["ramp_speed"]][at_mrt_idx],1)
  at_idx <- which.min(abs(test_list[[i]][["ex_time"]] - vts[["at"]][i]))
  rer_at[i] <- round(test_list[[i]][["rer"]][at_idx],2)
  vo2_at[i] <- round(test_list[[i]][["vo2"]][at_idx],1)
  pct_vo2_at[i] <- round(vo2_at[i] / vo2max[i] * 100, 1)
  
  rc_idx <- which.min(abs(test_list[[i]][["ex_time"]] - vts[["rc"]][i]))
  rer_rc[i] <- round(test_list[[i]][["rer"]][rc_idx],2)
  vo2_rc[i] <- round(test_list[[i]][["vo2"]][rc_idx],1)
  pct_vo2_rc[i] <- round(vo2_rc[i] / vo2max[i] * 100, 1)
  
  if(any(str_detect(colnames(test_list[[i]]), "hr"))) {
    hr_vo2max[i] <- round(test_list[[i]][["hr"]][vo2max_idx],0)
    max_hr[i] <- round(max(test_list[[i]][["hr"]]),0)
    hr_at[i] <- round(test_list[[i]][["hr"]][at_idx],0)
    hr_rc[i] <- round(test_list[[i]][["hr"]][rc_idx],0)
  } else {
    hr_vo2max[i] <- NA
    max_hr[i] <- NA
    hr_at[i] <- NA
    hr_rc[i] <- NA
  }
  
}

pre_report <- bind_cols(round(vts,1),
          vo2max = vo2max,
          rer_vo2max = rer_vo2max,
          hr_vo2max = hr_vo2max,
          speed_at = speed_at,
          rer_at = rer_at,
          vo2_at = vo2_at,
          pct_vo2_at = pct_vo2_at,
          hr_at = hr_at,
          rer_rc = rer_rc,
          vo2_rc = vo2_rc,
          pct_vo2_rc = pct_vo2_rc,
          hr_rc = hr_rc)

write.csv(pre_report, file = "data/processed/pre_report.csv",
          na = "", row.names = FALSE)
