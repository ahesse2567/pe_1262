library(tidyverse)

rm(list = ls())

post_report <- read_csv("data/2022/post/processed/post_report.csv",
                        show_col_types = FALSE)

long <- post_report %>% 
  select(-c(time_2mi, speed_2mi, speed_2mi_ss, at_time, rc_time)) %>% 
  rename(vo2_vo2max = vo2max,
         pctvo2_at = pct_vo2_at,
         pctvo2_rc = pct_vo2_rc) %>% 
  pivot_longer(cols = contains(c("at", "rc", "vo2"))) %>% 
  separate(name, into = c("var", "intensity"), sep = "_") %>% 
  group_split(id) 


store_data <- vector(mode = "list", length = length(long))

for (i in 1:length(long)){
  
  id <- unique(long[[i]]$id)
  
  temp_obj <- long[[i]] %>% 
    pivot_wider(id_cols = var, names_from = intensity, values_from = value) %>% 
    # rownames_to_column() %>%
    mutate(across(everything(), as.character)) %>% 
    t()
    
  colnames(temp_obj) <- temp_obj[1,]
  
  temp_obj <- temp_obj[-1,] %>% as_tibble()
  
  temp_obj <- temp_obj %>% 
    mutate(across(everything(), as.numeric)) %>% 
    mutate(id = id) %>% 
    mutate(intensity = c("at", "rc", "vo2max")) %>% 
    relocate(id, intensity) %>% 
    rename(`% VO2max` = pctvo2)

  store_data[[i]] <- temp_obj
}


out <- bind_rows(store_data) %>% 
  janitor::clean_names()

write_csv(out, file = "data/2022/post/processed/mar22_post-report_formatted.csv")


