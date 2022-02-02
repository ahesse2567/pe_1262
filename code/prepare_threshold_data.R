library(tidyverse)

rm(list = ls())

vts_raw <- read_csv("./data/threshold_data.csv", show_col_types = FALSE) %>% 
  drop_na() #some people ran the two mile but didn't complete an exercise test

#cleans up the times in the spreadsheet with AT and RC data
vts <- vts_raw %>% 
  mutate(at = lubridate::hms(at),
         at = lubridate::hour(at) + (lubridate::minute(at)/60)) %>% 
  mutate(rc = lubridate::hms(rc),
         rc = lubridate::hour(rc) + (lubridate::minute(rc)/60))

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#Now make this into a function
  # supply an ID number for which test to find
  # vt_data should be a data frame with data by ID for each participant
  # Could eventually add arguments for year and pre/post, aren't needed right now
fix_speeds_grades <- function(id, vt_data, time_point) {
  # browser()
  time_point = match.arg(time_point, choices = c("pre", "post"))
  
  #read in test data
  file_name <- paste0("data/processed/stages/mar22_",
                      as.character(id), "_", time_point, ".csv")
  test <- read_csv(file_name, show_col_types = FALSE)
  
  # match test data to VT data
  test_idx <- which(vt_data$id == id)
  
  #Creates a vector with corrected speeds
  fixed_speeds <- case_when(
    test$stage == 1 ~ 3,
    #walking
    test$stage == 2 ~ round(vt_data[test_idx, ]$speed_2mi *
                              .75, 1), #marathon pace
    test$stage == 3 ~ round(vt_data[test_idx, ]$speed_2mi *
                              0.3, 1), #recovery 1
    test$stage == 4 ~ round(vt_data[test_idx, ]$speed_2mi *
                              0.635, 1), #steady state for adjustment with MRT
    test$stage == 5 ~ round(vt_data[test_idx, ]$speed_2mi *
                              0.45, 1),# recovery 2
    test$stage %in% c(6:37) ~ #between stage 6 and stage 37 is a gradual increase in speed
      round((vt_data[test_idx, ]$speed_2mi - vt_data[test_idx, ]$speed_2mi *
               0.45) / 32 * (test$stage - 5) +
              vt_data[test_idx, ]$speed_2mi * 0.45,
            1
      ),
    test$stage > 37 ~ round(vt_data[test_idx, ]$speed_2mi, 1) #anything beyond stage 37 is always 2 mile speed
  )
  
  #Creates a vector with corrected grades
  fixed_grades <- case_when(test$stage < 38 ~ 1, #most of the test is at 1%
                            test$stage >= 38 ~ 1 + (test$stage - 37) * 0.5) #after stage 37 the incline increases
  
  #add fixed speeds and grades to the test
  test <- test %>% 
    mutate(fixed_speeds = fixed_speeds,
           fixed_grades = fixed_grades)
  
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
  
  #return dataframe with corrected speeds and grades
  return(test)
}

#Confirm function works for one test
fix_speeds_grades(105, vt_data = vts, time_point = "pre") %>% View

file_list <- list.files("data/processed/stages/", full.names = TRUE)

for(i in 1:length(file_list)) {
  f_name <- file_list[i]
  test <- suppressMessages((read_csv(f_name)))
  id <- str_extract(f_name, "\\d{3}")
  test_fixed <- fix_speeds_grades(id, vt_data = vts, time_point = "pre")
  f_name_out <- paste0("mar22_", id, "_pre.csv")
  write_csv(test_fixed,
            file = paste0("data/processed/fixed_speeds_grades/",f_name_out))
}


