library(tidyverse); library(hms)

hr_data <- read_delim(file = "./data/raw_hrv/2022-01-25 09-54-58.txt", delim = "/n", col_names = FALSE, show_col_types = FALSE) %>% 
  rename(rr = X1)

file_name <- "2022-01-25 09-54-58.txt"

clock_start <- str_extract(file_name, pattern = "\\s\\d{2}-\\d{2}-\\d{2}")
clock_start <- str_remove(clock_start, pattern = "\\s")

clock_hr <- str_split(clock_start, "-")[[1]][[1]] %>% as.numeric()
clock_min <- str_split(clock_start, "-")[[1]][[2]] %>% as.numeric()
clock_sec <- str_split(clock_start, "-")[[1]][[3]] %>% as.numeric()

clock_time <- hms(clock_sec, clock_min, clock_hr)

hr_data <- hr_data %>% 
  mutate(elapsed_time = cumsum(rr)/1000, 
         elapsed_time = hms(seconds = elapsed_time), 
         clock_time = as_hms(clock_time + elapsed_time), #convert from difftime to hms
         clock_time = round(clock_time, 0), #round times to allow merging with gas exchange data
         clock_time = as_hms(clock_time), #convert from difftime to hms
         hr = 60/(rr/1000)) %>% 
  group_by(clock_time) %>% #Heart beats can occur multiple times per second, but that's more frequent than data in Breeze
  summarise(clock_time = mean(clock_time), hr = mean(hr), elapsed_time = mean(elapsed_time)) %>% #Average data down to allow for merge with Breeze
  mutate(clock_time = as_hms(clock_time)) #switch clock_time back into hms


# ggplot(data = hr_data, aes(x = as_hms(elapsed_time), y = hr)) + geom_point()

df <- read_csv(file = "data/processed/fixed_speeds_grades/mar22_159_pre.csv")

str(df)

#Merges data frames and keeps all gas exchange data
merge(df, hr_data, by = "clock_time", all.x = TRUE) %>% 
  as_tibble() %>% 
  ggplot(data = .) + 
  geom_point(aes(x = ex_time, y = hr/max(hr)), color = "red") +
  geom_point(aes(x = ex_time, y = vo2/max(vo2)), color = "blue") +
  geom_point(aes(x = ex_time, y = speed/max(speed)), color = "green")
  

#First, associate an ID number with each HRV file

join_hrv_data <- read_csv("data/sp2022_marathon_pre_testing_order.csv", show_col_types = FALSE) %>% 
  select(-Comments) %>% 
  drop_na() #some people are missing data, will have to use HRs written on their collection sheets

#Should probably coerce this to a different data type but this will work for now
yr <- str_extract(join_hrv_data$Date, "\\d{4}") #year
mon <- str_extract(join_hrv_data$Date, "^\\d{1}") #month
day <- str_extract(join_hrv_data$Date, "\\/\\d{1,2}") %>% str_remove(., "/") %>% as.numeric() #date

for (i in 1:length(mon)){
  if (length(mon[i]) < 2){
    mon[i] <- paste0("0", mon[i])
  }
}

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

#Next steps:
#Write a loop that matches each HRV file to the corresponding ID number, labels it, and writes it as a new file to labeled_hrv
#Then, match each labeled HRV file with its corresponding data frame by ID number
#Then, bind the HR data to each data frame using the code for the single test above

