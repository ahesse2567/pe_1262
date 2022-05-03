library(tidyverse)
library(stringr)

tidy_gxt_rows <- function(.df) {
  start_ex_idx <- which(.df == "Start Exercise", arr.ind = TRUE)
  at_idx <- which(.df == "AT", arr.ind = TRUE)
  rc_idx <- which(.df == "RC", arr.ind = TRUE)
  vo2max_idx <- which(.df == "V02 Max", arr.ind = TRUE)
  rec_idx <- which(.df == "Start Recovery", arr.ind = TRUE)
  
  if(length(start_ex_idx) > 0) {
    start_row <- start_ex_idx[1] + 1
  } else {
    start_row <- 1
  }
  
  if (length(rec_idx) == 0) {
    end_row <- nrow(.df)
  } else {
    end_row <- rec_idx[1] - 1 # row just before rec_idx is last row
  }
  
  .df <- .df[start_row:end_row,]
  
  ventilatory_events <- rbind(at_idx, rc_idx, vo2max_idx) %>% 
    as_tibble() %>% 
    select(row) %>% 
    pull()
  
  if(length(ventilatory_events) > 0) {
    .df <- .df[-ventilatory_events,]
  }
  
  .df
}


tidy_gxt_cols <- function(.df,
                         drop_hr = TRUE,
                         slim_cols = FALSE) {
  # clean col names first and get rid of pre-exercise time
  .df <- .df %>%
    clean_names() %>%
    filter(!is.na(ex_time))
  
  # remove heart rate columns since we often import those later from RR intervals
  if (drop_hr == TRUE) {
    .df <- .df %>%
      select(-contains("heart"),-contains("hr"))
  }
  
  # fix time format if needed
  non_clock_time_cols <- str_which(colnames(.df), "time(?!_clock)")
  
  .df <- .df %>%
    # select(-contains("clock")) %>%
    separate(`time`, into = c("m1", "s1"), sep = ":") %>%
    separate(ex_time, into = c("m2", "s2"), sep = ":") %>%
    separate(time_clock,
             into = c("h3", "m3", "s3"),
             sep = ":") %>%
    mutate(across(where(is.character), as.numeric)) %>%
    mutate(time = (m1 + (s1 / 60)), .keep = "unused") %>%
    mutate(ex_time = (m2 + (s2 / 60)), .keep = "unused") %>%
    mutate(clock_time = hms::hms(s3, m3, h3), .keep = "unused")
  
  vo2_cols <- grep(pattern = "vo2", x = colnames(.df))
  
  larger_vo2 <- .df %>%
    select(all_of(vo2_cols)) %>%
    summarize(across(.cols = everything(), mean)) %>%
    which.max(.)
  
  larger_vo2_col <- vo2_cols[larger_vo2]
  
  if (slim_cols == TRUE) {
    #trim cols down for WinBreak
    which_vco2 <- grep(pattern = "vco2", colnames(.df))
    which_ve <- grep(pattern = "ve", colnames(.df))
    which_time <- grep(pattern = "time", colnames(.df))
    
    cols_to_keep <-
      c(which_time, larger_vo2_col, which_vco2, which_ve)
    
    cleaned_test <- .df[, cols_to_keep] %>%
      arrange("time")
    
    colnames(cleaned_test) <- c("Time", "VO2", "VCO2", "VE")
    
  } else {
    colnames(.df)[larger_vo2_col] <- "vo2_abs" #clarify vo2_abs vs. vo2
    
    cleaned_test <- .df %>% #sort by time, just to make sure
      arrange("time")
  }
  
  cleaned_test
}
