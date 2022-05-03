library(tidyverse)

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Now make this into a function
# supply an ID number for which test to find
# vt_data should be a data frame with data by ID for each participant
# Could eventually add arguments for year and pre/post, aren't needed right now
correct_speeds_grades <- function(.df, id, vt_data, time_point) {
  # browser()
  time_point = match.arg(time_point,
                         choices = c("pre", "post"),
                         several.ok = FALSE)
  
  # match test data to VT data
  .df_idx <- which(vt_data$id == id)
  
  #Creates a vector with corrected speeds
  fixed_speeds <- case_when(
    .df$stage == 1 ~ 3,
    #walking
    .df$stage == 2 ~ round(vt_data[.df_idx, ]$speed_2mi *
                              .75, 1), #marathon pace
    .df$stage == 3 ~ round(vt_data[.df_idx, ]$speed_2mi *
                              0.3, 1), #recovery 1
    .df$stage == 4 ~ round(vt_data[.df_idx, ]$speed_2mi *
                              0.635, 1), #steady state for adjustment with MRT
    .df$stage == 5 ~ round(vt_data[.df_idx, ]$speed_2mi *
                              0.45, 1),# recovery 2
    .df$stage %in% c(6:37) ~ #between stage 6 and stage 37 is a gradual increase in speed
      round((vt_data[.df_idx, ]$speed_2mi - vt_data[.df_idx, ]$speed_2mi *
               0.45) / 32 * (.df$stage - 5) +
              vt_data[.df_idx, ]$speed_2mi * 0.45,
            1
      ),
    .df$stage > 37 ~ round(vt_data[.df_idx, ]$speed_2mi, 1) #anything beyond stage 37 is always 2 mile speed
  )
  
  #Creates a vector with corrected grades
  fixed_grades <- case_when(.df$stage < 38 ~ 1, #most of the test is at 1%
                            .df$stage >= 38 ~ 1 + (.df$stage - 37) * 0.5) #after stage 37 the incline increases
  
  #add fixed speeds and grades to the test
  .df <- .df %>% 
    mutate(fixed_speeds = fixed_speeds,
           fixed_grades = fixed_grades)
  
  rec2_speed <- .df %>% 
    filter(stage == 5) %>% 
    select(fixed_speeds) %>% 
    summarize(start_speed = get_mode(fixed_speeds)) %>% 
    pull()
  max_speed <- max(.df$fixed_speeds)
  ramp_rate <- (max_speed - rec2_speed) / 8 # ramp lasts 8 minutes
  
  min_ramp_time <- .df %>% 
    filter(stage >= 6) %>% 
    select(ex_time) %>% 
    min()
  
  .df <- .df %>% 
    mutate(ramp_time = ex_time - min_ramp_time,
           ramp_speed = ramp_rate * ramp_time + rec2_speed)
  
  #return dataframe with corrected speeds and grades
  .df
}
