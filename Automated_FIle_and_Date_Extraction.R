# Data Loading and Preparation
# Load libraries

library(readxl)
library(janitor)
library(tidyverse)

# Loading Data

folder_path <- "C:/Users/tulas/Documents/Airports1"
file_list <- list.files(path = folder_path, pattern = "*.xlsx", full.names = TRUE)

all_data_list <- list()

# Loop through each file and read it, skipping the first and last two rows

all_data_list <- list()  # Make sure list is initialized

for (file in file_list) {
  temp_data <- read_excel(file, sheet = 1, skip = 2, col_names = TRUE) %>%
    janitor::clean_names() %>%
    mutate(file_name = basename(file))
  
  temp_data <- temp_data[1:(nrow(temp_data) - 2), ]
  
  # Force convert key passenger/freight columns to numeric
  normalize_cols <- c("s_no",
                    "passengers_from_city_2", "passengers_to_city_2",
                    "freight_from_city_2", "freight_to_city_2",
                    "post_from_city_2", "post_to_city_2",
                    "mail_from_city_2", "mail_to_city_2")
  for (col in normalize_cols) {
    if (col %in% names(temp_data)) {
      if (col == "s_no") {
        temp_data[[col]] <- as.character(temp_data[[col]])  # keep s_no as character
      } else {
        temp_data[[col]] <- suppressWarnings(as.numeric(temp_data[[col]]))
      }
    }
  }
  
  
  all_data_list[[length(all_data_list) + 1]] <- temp_data
}

combined_data <- dplyr::bind_rows(all_data_list)

head(combined_data)

# Capturing Month and Year from file name

combined_data <- combined_data %>%
  mutate(
    # Extract month and year string from the filename (case-insensitive, optional comma)
    month_year_str = str_extract(file_name,
                                 regex("(January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[,]?\\s*\\d{4}", ignore_case = TRUE)
    ),
    
    # Clean up: remove commas and extra spaces
    month_year_str = str_replace_all(month_year_str, ",", ""),
    month_year_str = str_trim(month_year_str),
    
    # Convert to proper Date object (1st of the month)
    month_year = parse_date_time(month_year_str, orders = c("b Y", "B Y"))
  )

head(combined_data %>% select(file_name, month_year_str, month_year))
summary(combined_data$month_year)

#mergeing and removeing duplicates

combined_data <- combined_data %>%
  mutate(
    passengers_to_city_2 = coalesce(passengers_to_city_2, passenger_to_city_2, passeneger_to_city_2),
    passengers_from_city_2 = coalesce(passengers_from_city_2, passenger_from_city_2),
    city_1 = coalesce(city_1, city1),
    city_2 = coalesce(city_2, city2),
    sl_no = coalesce(sl_no, s_no)
  ) %>%
  select(-c(passenger_to_city_2, passeneger_to_city_2, passenger_from_city_2, city1, city2, s_no))

summary(combined_data)