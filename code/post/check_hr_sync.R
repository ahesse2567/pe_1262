library(tidyverse)
rm(list = ls())

cpet_hr_files <- list.files("data/2022/post/processed/cpet_hr/",
                            full.names = TRUE)

test_list <- map(cpet_hr_files, read_csv, show_col_types = FALSE)

file_names <- basename(cpet_hr_files) %>% 
  tools::file_path_sans_ext()

names(test_list) <- file_names

plot_list = vector(mode = "list", length = length(test_list))
for (i in 1:length(test_list)) {
  hr_plot <- ggplot(data = test_list[[i]],
                    aes(x = clock_time, y = hr / max(hr, na.rm = TRUE))) +
    geom_point() +
    geom_line(aes(y = fixed_speeds / max(fixed_speeds, na.rm = TRUE)), color = "green") +
    geom_line(aes(y = fixed_grades / max(fixed_grades, na.rm = TRUE)), color = "orange") +
    theme_minimal() +
    ggtitle(label = names(test_list[i]))
  plot_list[[i]] <- hr_plot
}

for (p in plot_list){
  print(p)
}
