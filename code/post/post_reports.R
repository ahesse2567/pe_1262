library(tidyverse)
library(gasExchangeR)

rm(list = ls())

# there's currently a problem with #15 (id 144)

cpet_hr_files <- list.files("data/2022/post/processed/cpet_hr/", full.names = TRUE)

cpet_only_files <- list.files("data/2022/post/processed/cpet_csv/",
                        full.names = TRUE)
cpet_no_hr_files <-
  cpet_only_files[!basename(cpet_only_files) %in% basename(cpet_hr_files)]

file_list <- c(cpet_hr_files, cpet_no_hr_files)

test_list_raw <- vector(mod = "list", length = length(file_list))

for(i in 1:length(file_list)) {
  test_list_raw[[i]] <- read_csv(file_list[i], show_col_types = FALSE)
  names(test_list_raw)[i] <- basename(file_list[i]) %>% 
    tools::file_path_sans_ext()
}

test_list <- vector(mod = "list", length = length(test_list_raw))

test_list <- map(test_list_raw, avg_exercise_test,
                 type = "breath",
                 subtype = "rolling",
                 time_col = "ex_time",
                 roll_window = 7,
                 roll_trim = 2)

mrt_data <- read_csv("data/2022/post/processed/mrt.csv", show_col_types = FALSE)

vts_raw <- read_csv("data/2022/post/raw/threshold_data.csv",
                    show_col_types = FALSE)

# cleans up the times in the spreadsheet with AT and RC data
vts <- vts_raw %>% 
  mutate(at_time = lubridate::hms(at_time),
         at_time = lubridate::hour(at_time) + (lubridate::minute(at_time)/60)) %>% 
  mutate(rc_time = lubridate::hms(rc_time),
         rc_time = lubridate::hour(rc_time) + (lubridate::minute(rc_time)/60)) %>% 
  filter(!is.na(at_time))

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
  vo2max[i] <- round(max(test_list[[i]][["vo2"]]), 1)
  vo2max_idx <- which.max(test_list[[i]][["vo2"]])
  
  rer_vo2max[i] <- test_list[[i]][["rer"]][vo2max_idx]
  
  at_mrt <- (vts[i,"at_time"] - mrt_data[i,"mrt"]) %>% 
    pull()
  
  if(is.na(at_mrt)) {
    speed_at[i] <- NA
    rer_at[i] <- NA
    vo2_at[i] <- NA
    pct_vo2_at <- NA
  } else {
    at_mrt_idx <- which.min(abs(test_list[[i]][["ex_time"]] - at_mrt))
    speed_at[i] <- round(test_list[[i]][["ramp_speed"]][at_mrt_idx],1)
    at_idx <- which.min(abs(test_list[[i]][["ex_time"]] - vts[["at_time"]][i]))
    rer_at[i] <- round(test_list[[i]][["rer"]][at_idx],2)
    vo2_at[i] <- round(test_list[[i]][["vo2"]][at_idx],1)
    pct_vo2_at[i] <- round(vo2_at[i] / vo2max[i] * 100, 1)
  }
  
  if(length(which.min(abs(test_list[[i]][["ex_time"]] - vts[["rc_time"]][i]))) == 0) {
    rer_rc[i] <- NA
    vo2_rc[i] <- NA
    pct_vo2_rc <- NA
  } else {
    rc_idx <- which.min(abs(test_list[[i]][["ex_time"]] - vts[["rc_time"]][i]))
    rer_rc[i] <- round(test_list[[i]][["rer"]][rc_idx],2)
    vo2_rc[i] <- round(test_list[[i]][["vo2"]][rc_idx],1)
    pct_vo2_rc[i] <- round(vo2_rc[i] / vo2max[i] * 100, 1) 
  }
  
  if(any(str_detect(colnames(test_list[[i]]), "hr"))) {
    hr_vo2max[i] <- round(test_list[[i]][["hr"]][vo2max_idx],0)
    max_hr[i] <- round(max(test_list[[i]][["hr"]], na.rm = TRUE), 0)
    hr_at[i] <- round(test_list[[i]][["hr"]][at_idx],0)
    hr_rc[i] <- round(test_list[[i]][["hr"]][rc_idx],0)
  } else {
    hr_vo2max[i] <- NA
    max_hr[i] <- NA
    hr_at[i] <- NA
    hr_rc[i] <- NA
  }
  
}

post_report <- bind_cols(
  vts,
  hrmax = max_hr,
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
  hr_rc = hr_rc
) %>%
  select(-contains(c("retest")))

write.csv(post_report, file = "data/post/processed/post_report.csv",
          na = "", row.names = FALSE)
