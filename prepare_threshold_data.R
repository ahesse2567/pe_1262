library(tidyverse)

vts <- read_csv("./data/threshold_data.csv", show_col_types = FALSE) %>% 
  drop_na() #some people ran the two mile but didn't complete an exercise test

#cleans up the times in the spreadsheet with AT and RC data
vts <- vts %>% 
  mutate(at = lubridate::hms(at),
         at = lubridate::hour(at) + (lubridate::minute(at)/60)) %>% 
  mutate(rc = lubridate::hms(rc),
         rc = lubridate::hour(rc) + (lubridate::minute(rc)/60))

#Read in a sample test
test <- read_csv("data/processed/stages/mar22_105_pre.csv", show_col_types = FALSE)

#Selects the row in vts that corresponds to the sample test
mar22_105 <- vts[1,]

#Create a new column with 2 mile speed
  #Do we need this?
# test$increment <- mar22_105$speed_2mi

#Creates a vector with corrected speeds
fixed_speeds <- case_when(test$stage == 1 ~ 3, #walking 
          test$stage == 2 ~ round(mar22_105$speed_2mi*.75,1), #marathon pace
          test$stage == 3 ~ round(mar22_105$speed_2mi*0.3,1), #recovery 1
          test$stage == 4 ~ round(mar22_105$speed_2mi*0.635,1), #steady state for adjustment with MRT
          test$stage == 5 ~ round(mar22_105$speed_2mi*0.45,1), #recovery 2
          test$stage %in% c(6:37) ~ #between stage 6 and stage 37 is a gradual increase in speed
            round((mar22_105$speed_2mi - mar22_105$speed_2mi*0.45) / 32 * (test$stage - 5) +
            mar22_105$speed_2mi*0.45, 1),
          test$stage > 37 ~ round(mar22_105$speed_2mi, 1)#anything beyond stage 37 is always 2 mile speed
          )

#make sure speeds are right
fixed_speeds

#Creates a vector with corrected grades
fixed_grades <- case_when(test$stage < 38 ~ 1, #most of the test is at 1%
                          test$stage >= 38 ~ 1 + (test$stage - 37) * 0.5) #after stage 37 the incline increases
#make sure grades are right
fixed_grades

#add fixed speeds and grades to the test
test <- test %>% 
  mutate(fixed_speeds = fixed_speeds,
         fixed_grades = fixed_grades)


#Now make this into a function
  #supply an ID number for which test to find
  #vt_data should be a data frame with data by ID for each participant
  #Could eventually add arguments for year and pre/post, aren't needed right now
fix_speeds_grades <- function(id, vt_data){
  
  #read in test data
  file_name <- paste0("data/processed/stages/mar22_", as.character(id), "_pre.csv")
  test <- read_csv(file_name, show_col_types = FALSE)
  
  #match test data to VT data
  test_idx <- which(vt_data$id == id)
  vt_data[test_idx,]
  
  
  #Creates a vector with corrected speeds
  fixed_speeds <- case_when(test$stage == 1 ~ 3, #walking 
                            test$stage == 2 ~ round(mar22_105$speed_2mi*.75,1), #marathon pace
                            test$stage == 3 ~ round(mar22_105$speed_2mi*0.3,1), #recovery 1
                            test$stage == 4 ~ round(mar22_105$speed_2mi*0.635,1), #steady state for adjustment with MRT
                            test$stage == 5 ~ round(mar22_105$speed_2mi*0.45,1), #recovery 2
                            test$stage %in% c(6:37) ~ #between stage 6 and stage 37 is a gradual increase in speed
                              round((mar22_105$speed_2mi - mar22_105$speed_2mi*0.45) / 32 * (test$stage - 5) +
                                      mar22_105$speed_2mi*0.45, 1),
                            test$stage > 37 ~ round(mar22_105$speed_2mi, 1)#anything beyond stage 37 is always 2 mile speed
  )
  
  #Creates a vector with corrected grades
  fixed_grades <- case_when(test$stage < 38 ~ 1, #most of the test is at 1%
                            test$stage >= 38 ~ 1 + (test$stage - 37) * 0.5) #after stage 37 the incline increases
  
  #add fixed speeds and grades to the test
  test <- test %>% 
    mutate(fixed_speeds = fixed_speeds,
           fixed_grades = fixed_grades)
  
  #return dataframe with corrected speeds and grades
  return(test)
}


#Confirm function works for one test
fix_speeds_grades(105, vt_data = vts)

# (mar22_105$speed_2mi - mar22_105$speed_2mi*0.45) / 32 * (test$stage - 5) +
#   mar22_105$speed_2mi*0.45
# 
# stages <- 1:61
# 
# numerator <- (mar22_105$speed_2mi - mar22_105$speed_2mi*0.45)
# denominator <- num_stages
# 
# numerator / denominator
# 
# round(numerator / denominator * (stages - 5) + mar22_105$speed_2mi*0.45, 1)
# 
# round(stage15 + mar22_105$speed_2mi*0.45, 1)
