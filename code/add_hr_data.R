library(tidyverse); library(hms)

rm(list = ls())

source("code/add_hr_func.R")

file_list_hrv <- list.files("data/labeled_hrv/")

ids <- character(length = length(file_list_hrv))

for(i in 1:length(ids)) {
  ids[i] <- str_extract(file_list_hrv[i],"\\d{3}")
}

for(i in 1:length(ids)) {
  bind_hr_data(id = ids[i], time_point = "pre")
}

#First, associate an ID number with each HRV file

join_hrv_data <- read_csv("data/sp2022_marathon_pre_testing_order.csv", show_col_types = FALSE) %>% 
  select(-Comments) %>% 
  drop_na() #some people are missing data, will have to use HRs written on their collection sheets

#Should probably coerce this to a different data type but this will work for now
yr <- str_extract(join_hrv_data$Date, "\\d{4}") #year
mon <- str_extract(join_hrv_data$Date, "^\\d{1}") #month
day <- str_extract(join_hrv_data$Date, "\\/\\d{1,2}") %>% str_remove(., "/") %>% as.numeric() #date

#make sure months are all two digits
for (i in 1:length(mon)){
  if (length(mon[i]) < 2){
    mon[i] <- paste0("0", mon[i])
  }
}

#make sure days are all two digits
for (i in 1:length(day)){
  if (day[i] < 10){
    day[i] <- paste0("0", day[i])
  }
}

#date is now in a format that matches the name of the HRV files
hrv_date <- paste(yr, mon, day, sep = "-")

#Next, fix the time so it's in the same format
hrv_time <- str_replace_all(join_hrv_data$`Exact start time`, ":", "-")

join_hrv_data$hrv_date_time <- paste(hrv_date, hrv_time, sep = " ")

hrv_files <- list.files("data/raw_hrv/")

hrv_files <- str_remove(hrv_files, ".txt")

for (i in 1:nrow(join_hrv_data)){
  
  #which of the HRV files matches the master spreadsheet
  files_idx <- which(hrv_files == join_hrv_data[i,]$hrv_date_time)
  
  if (join_hrv_data[i,]$hrv_date_time == hrv_files[files_idx]){
    
    new_file_name <- paste0("data/labeled_hrv/", "mar22_", join_hrv_data$ID[i], "_", hrv_files[files_idx], ".txt")
    old_file_name <- paste0("data/raw_hrv/", hrv_files[files_idx], ".txt")
    
    file.copy(from = old_file_name, to = new_file_name)
    
  }else{
    
    print("error")
    print(i)
  }

  
  
}


#Next steps:
#Write a loop that matches each HRV file to the corresponding ID number, labels it, and writes it as a new file to labeled_hrv
#Then, match each labeled HRV file with its corresponding data frame by ID number
#Then, bind the HR data to each data frame using the code for the single test above

