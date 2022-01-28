library(tidyverse); library(janitor)

#a helper function for clean_test
tidy_test <- function(df, slim_cols=FALSE){
  
  df <- df %>%
    select(-contains("heart"), -contains("HR")) %>% #not needed, usually has bad data
    drop_na()
  
  df <- df %>%
    separate(Time, into = c("M1", "S1"), sep = ":") %>%
    separate(Ex.Time, into = c("M2", "S2"), sep = ":") %>% 
    separate(Time..Clock., into = c("M3", "S3"), sep = ":") %>% 
    mutate(across(where(is.character), as.numeric)) %>%
    mutate(time = (M1 + (S1/60)), .keep = "unused") %>% 
    mutate(ex_time = (M2 + (S2/60)), .keep = "unused") %>% 
    mutate(clock_time = (M3 + (S3/60)), .keep = "unused")
  
  vo2_cols <- grep(pattern = "VO2", x = colnames(df))
  
  larger_vo2 <- df %>%
    select(vo2_cols) %>%  
    summarize(across(.cols = everything(), mean)) %>%
    which.max(.)
  
  larger_vo2_col <- vo2_cols[larger_vo2]
  
  which_vco2 <- grep(pattern = "VCO2", colnames(df))
  
  which_ve <- grep(pattern = "VE", colnames(df))
  
  which_time <- grep(pattern = "time", colnames(df))
  
  cols_to_keep <- c(which_time, larger_vo2_col, which_vco2, which_ve)
  
  if (slim_cols == TRUE){ #trim cols down for WinBreak
    cleaned_test <- df[,cols_to_keep] %>%
      arrange("time")
    
    colnames(cleaned_test) <- c("Time", "VO2", "VCO2", "VE")
    
  } else{
    
    colnames(df)[larger_vo2_col] <- "vo2_abs" #clarify vo2_abs vs. vo2
    
    colnames(df) <- tolower(colnames(df)) #make all lowercase
    
    cleaned_test <- df %>% #sort by time, just to make sure
      arrange("time")
  }
  
  return(cleaned_test)
}


#a function to clean exercise tests
  #supply file name with path
clean_test <- function(x){
  
  test <- read.table(file = x, sep = "\t", na.strings = c("NA", ""), header = TRUE)
  file_name <- x
  file_name <- str_remove(file_name, "./data/raw/")
  file_name <- str_remove(file_name, ".txt")
  
  test <- janitor::remove_empty(test, which = "cols") #cols first to get rid of empty columns
  
  #keep data from "Start Exercise" to "Start Recovery"
  start_row <- which(test$Time == "Start Exercise")
  end_row <- which(test$Time == "Start Recovery")
  
  if (length(end_row) == 0){ #length of 0 happens when test doesn't have start recovery row 
    end_row <- nrow(test)
  }
  
  if(end_row < 10){ #if Breeze sets Start Recovery too early
    end_row <- nrow(test)
    
  }
  
  rest_data <- test[1:start_row,] %>% #Data before the start of exercise, no longer needed
    drop_na()
  
  exc_test <- test[start_row:end_row,] %>% #exercise data
    drop_na()
  
  #Process test for analysis in R
    
  cleaned_test <- tidy_test(exc_test, slim_cols = FALSE) #for analysis in R
    print(nrow(cleaned_test))
    
    #write exercise data
  write_csv(cleaned_test, file = paste0("./data/processed/", file_name, ".csv"), eol="\r\n")
    #don't need to add more of the file path here because it's still in file_name from import
    
  
  
}

#Sample code for using clean_test function
# txt_files <- list.files("./data/raw/", full.names = TRUE)
# clean_test(txt_files[[1]])
