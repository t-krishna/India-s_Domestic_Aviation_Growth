# Data Loading and Preparation
# Load libraries

library(readxl)
library(janitor)
library(tidyverse)

# Loading Data

folder_path <- "C:/Users/tulas/Documents/Airports"
file_list <- list.files(path = folder_path, pattern = "*.xlsx", full.names = TRUE)

all_data_list <- list()

# Loop through each file and read it, skipping the first and last two rows

for (file in file_list) {
  temp_data <- read_excel(file, sheet = 1, skip = 2) %>%  # Skip first 2 rows
    clean_names() %>%
    mutate(file_name = basename(file)) # capture file name for reference
  
  temp_data <- temp_data[1:(nrow(temp_data) - 2), ] # Skip last 2 rows
  
  all_data_list[[length(all_data_list) + 1]] <- temp_data
}

combined_data <- bind_rows(all_data_list)

head(combined_data)

# Capturing Month and Year from file name

combined_data <- combined_data %>%
  mutate(month_year = str_extract(file_name, "\\b[A-Z]+\\s\\d{4}\\b")
          )
head(combined_data$month_year)

colnames(combined_data)