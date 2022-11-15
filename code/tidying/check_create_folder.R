# hopefully this works on Windows computers, too

check_create_folder <- function(year, time_point, data_type) {
  folder_name <- paste("data", year, time_point, "processed", data_type, sep = "/")
  if (!file.exists(folder_name)) {
    dir.create(folder_name)
  }
}