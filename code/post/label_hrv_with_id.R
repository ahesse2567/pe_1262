##### UPDATE FOR POST DATA ####

library(tidyverse); library(hms); library(janitor)
rm(list = ls())
source("code/tidying/check_create_folder.R")

# First, associate an ID number with each HRV file

# TODO get testing order file

join_hrv_data <- read_csv("data/2022/post/sp2022_marathon_post_testing_order.csv",
                          show_col_types = FALSE) %>% 
  clean_names() %>% 
  filter(is.na(retest)) %>% 
  select(-comments, -retest) %>% 
  drop_na() #some people are missing data, will have to use HRs written on their collection sheets

# Should probably coerce this to a different data type but this will work for now
yr <- str_extract(join_hrv_data$date, "\\d{4}") # year
mon <- str_extract(join_hrv_data$date, "^\\d{1}") # month
day <- str_extract(join_hrv_data$date, "\\/\\d{1,2}") %>%
  str_remove(., "/") %>%
  as.numeric() # day

# make sure months are all two digits
for (i in 1:length(mon)) {
  mon[i] <- as.character(mon[i])
  if (nchar(mon[i]) < 2) {
    mon[i] <- paste0("0", mon[i])
  }
}

# make sure days are all two digits
if (class(day) == "numeric") {
  day <- sprintf("%02d", day)
}

#date is now in a format that matches the name of the HRV files
hrv_date <- paste(yr, mon, day, sep = "-")

#Next, fix the time so it's in the same format
hrv_time <- str_replace_all(join_hrv_data$exact_hrv_start_time, ":", "-")

join_hrv_data$hrv_date_time <- paste(hrv_date, hrv_time, sep = " ")

hrv_files <- list.files("data/2022/post/raw/hrv") %>% 
  str_remove(".txt")

check_create_folder(year = 2022, time_point = "post", data_type = "hrv")

for (i in 1:nrow(join_hrv_data)){
  #which of the HRV files matches the master spreadsheet
  files_idx <- which(hrv_files == join_hrv_data[i,]$hrv_date_time)
  
  if (join_hrv_data[i,]$hrv_date_time == hrv_files[files_idx]){
    
    new_file_name <- paste0("data/2022/post/processed/hrv/", "mar22_",
                            join_hrv_data$id[i], "_",
                            hrv_files[files_idx], ".txt")
    old_file_name <- paste0("data/2022/post/raw/hrv/",
                            hrv_files[files_idx],
                            ".txt")
    
    file.copy(from = old_file_name, to = new_file_name)
    
  } else {
    
    print("error")
    print(i)
  }
  
}
