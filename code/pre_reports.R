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

i <- 1

ggplot(data = test_list[[i]], aes(x = ex_time, y = vo2)) +
  geom_point()

for(i in 1:length(test_list)) {
  vo2max[i] <- round(max(test_list[[i]][["vo2"]]),1)
  vo2max_idx <- which.max(test_list[[i]][["vo2"]])
  
  rer_vo2max[i] <- test_list[[i]][["rer"]][vo2max_idx]
  hr_vo2max[i] <- test_list[[i]][["hr"]][vo2max_idx]
  max_hr[i] <- max(test_list[[i]][["hr"]])
  
  at_mrt <- (vts[i,"at"] - mrt_data[i,"mrt"]) %>% 
    pull()
  at_mrt_idx <- which.min(abs(test_list[[i]][["ex_time"]] - at_mrt))
  speed_at[i] <- round(test_list[[i]][["ramp_speed"]][at_mrt_idx],1)
  at_idx <- which.min(abs(test_list[[i]][["ex_time"]] - vts[["at"]][i]))
  rer_at[i] <- round(test_list[[i]][["rer"]][at_idx],2)
  vo2_at[i] <- round(test_list[[i]][["vo2"]][at_idx],1)
  pct_vo2_at[i] <- round(vo2_at[i] / vo2max[i] * 100, 1)
  hr_at[i] <- test_list[[i]][["hr"]][at_idx]
  
  rc_idx <- which.min(abs(test_list[[i]][["ex_time"]] - vts[["rc"]][i]))
  rer_rc[i] <- round(test_list[[i]][["rer"]][rc_idx],2)
  vo2_rc[i] <- round(test_list[[i]][["vo2"]][rc_idx],1)
  pct_vo2_rc[i] <- round(vo2_rc[i] / vo2max[i] * 100, 1)
  hr_rc[i] <- test_list[[i]][["hr"]][rc_idx]
}

pre_report <- bind_cols(vts,
          vo2max = vo2max,
          rer_vo2max = rer_vo2max,
          speed_at = speed_at,
          rer_at = rer_at,
          vo2_at = vo2_at,
          pct_vo2_at = pct_vo2_at,
          rer_rc = rer_rc,
          vo2_rc = vo2_rc,
          pct_vo2_rc = pct_vo2_rc)

write_csv(pre_report, file = "data/processed/pre_report.csv")
