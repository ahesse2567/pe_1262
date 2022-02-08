library(tidyverse)

pre_report <- read_csv("data/processed/pre_report_manual.csv") %>% 
  select(-1)

long <- pre_report %>% 
  select(-c(time_2mi, speed_2mi, speed_2mi_ss, at, rc)) %>% 
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


bind_rows(store_data) %>% 
  write_csv(., file = "data/mar22_prereport_formatted.csv")

# pre_report %>% 
#   select(-c(time_2mi, speed_2mi, speed_2mi_ss, at, rc)) %>% 
#   pivot_longer(cols = contains("at")) %>% View
#   pivot_longer(cols = contains("rc"))
# 
# long[[1]] %>% 
#   pivot_wider(id_cols = contains("rc"),
#               values_from = value)
