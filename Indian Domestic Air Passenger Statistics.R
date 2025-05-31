# Introduction This report analyzes the Domestic Passenger Traffic for India in the year 2024.
# We explore the top routes, traffic patterns, and perform clustering analysis.

# Load libraries

library(readxl)
library(janitor)
library(tidyverse)

# Data Cleaning
# Reaplacing null values with 0

clean_data <- combined_data %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# Replacing NA values with 0

clean_data <- clean_data %>%
  mutate(across(
    c(passengers_to_city_2, passengers_from_city_2,
      freight_to_city_2, freight_from_city_2,
      mail_to_city_2, mail_from_city_2),
    ~ replace_na(., 0)
  ))

# Removing SNo column

clean_data <- clean_data %>%
  select(-s_no)

clean_data <- clean_data %>%
  select(city_1, city_2, passengers_to_city_2, passengers_from_city_2,
         freight_to_city_2, freight_from_city_2,
         mail_to_city_2, mail_from_city_2, month_year)

# Formating data types

clean_numeric <- function(x) {
  x <- as.character(x) # ensure it's character for str_replace_all
  x <- str_trim(x)     # remove leading/trailing whitespace
  x <- str_replace_all(x, ",", "")
  x <- na_if(x, "-")
  x <- na_if(x, "N/A")
  x <- na_if(x, "")
  as.numeric(x)
}

clean_data <- clean_data %>%
  mutate(
    passengers_to_city_2 = clean_numeric(passengers_to_city_2),
    passengers_from_city_2 = clean_numeric(passengers_from_city_2),
    freight_to_city_2 = clean_numeric(freight_to_city_2),
    freight_from_city_2 = clean_numeric(freight_from_city_2),
    mail_to_city_2 = clean_numeric(mail_to_city_2),
    mail_from_city_2 = clean_numeric(mail_from_city_2)
  )

clean_data <- clean_data %>% mutate(country = "India")

# Restructuring data-set into Origin, Destination, Total Passengers format

origin_city <- c(clean_data$city_1, clean_data$city_2)
destination_city <- c(clean_data$city_2, clean_data$city_1)
passengers <- c(clean_data$passengers_to_city_2, clean_data$passengers_from_city_2)
freight <- c(clean_data$freight_to_city_2, clean_data$freight_from_city_2)
mail <- c(clean_data$mail_to_city_2, clean_data$mail_from_city_2)
month_year_new <- c(clean_data$month_year, clean_data$month_year)
country_new <- c(clean_data$country, clean_data$country)

Final_data <- data.frame(origin_city, destination_city, passengers, freight, mail, month_year = month_year_new, country = country_new)

Final_data <- Final_data %>%
  mutate(
    passengers = as.numeric(passengers),
    freight = as.numeric(freight),
    mail = as.numeric(mail)
  )

# Removing unnecessary spaces between words

Final_data <- Final_data %>%
  mutate(
    origin_city = trimws(origin_city),
    destination_city = trimws(destination_city)
  )

# Converting names into Uppercase

Final_data_clean <- Final_data %>%
  mutate(
    origin_city = toupper(trimws(origin_city)),
    destination_city = toupper(trimws(destination_city))
  )

# Mapping table values

city_name_mapping <- c(
  "RAJKOT INTERNATIONAL AIRP" = "RAJKOT INTERNATIONAL AIRPORT",
  "AYODHYA INTERNATIONAL AI" = "AYODHYA INTERNATIONAL AIRPORT"
)

Final_data_clean <- Final_data_clean %>%
  mutate(
    origin_city = recode(origin_city, !!!city_name_mapping),
    destination_city = recode(destination_city, !!!city_name_mapping)
  )

Final_data_clean <- Final_data_clean %>%
  mutate(
    month_year = as.Date(paste0("01-", month_year), format = "%d-%b %Y")
  )

# Analysis
# Total passengers on each route
# Top 10 Routes

top_10_routes <- Final_data_clean %>%
  group_by(origin_city, destination_city) %>%
  summarise(
    total_passengers = sum(passengers, na.rm = TRUE), .groups = 'drop'
  ) %>%
  arrange(desc(total_passengers)) %>%
  slice_head(n = 10)

# View results
top_10_routes

# Top 20 Routes (unidirectional and bidirectional)

top_20_routes <- Final_data_clean %>%
  group_by(origin_city, destination_city) %>%
  summarise(
    total_passengers = sum(passengers, na.rm = TRUE), .groups = 'drop'
  ) %>%
  arrange(desc(total_passengers)) %>%
  slice_head(n = 20)

top_20_routes #Top 20 unidirectional routes

top_20_bidirectional_routes <- Final_data_clean %>%
  rowwise() %>%
  mutate(
    route = paste(sort(c(origin_city, destination_city)), collapse = " - ")
  ) %>%
  ungroup() %>%
  group_by(route) %>%
  summarise(
    total_passengers = sum(passengers, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_passengers)) %>%
  slice_head(n = 20)

top_20_bidirectional_routes #Top 20 bidirectional routes

# Visulaisation
# Top 20 Routes
# Create a route label (Origin ➔ Destination)
top_20_unidirectional_routes <- top_20_routes %>%
  mutate(route = paste(origin_city, "➔", destination_city))

ggplot(top_20_unidirectional_routes, aes(x = reorder(route, total_passengers), y = total_passengers)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 20 Unidirectional Routes by Passenger Traffic",
    x = "Route (Origin ➔ Destination)",
    y = "Total Passengers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  scale_y_continuous(labels = scales::comma)


# Top 20 bidirectional routes

top_20_bidirectional <- Final_data_clean %>%
  mutate(route = ifelse(origin_city < destination_city, 
                        paste(origin_city, "↔", destination_city), 
                        paste(destination_city, "↔", origin_city))) %>%
  group_by(route) %>%
  summarise(total_passengers = sum(passengers, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_passengers)) %>%
  slice_head(n = 20)

# PLOT

ggplot(top_20_bidirectional, aes(x = reorder(route, total_passengers), y = total_passengers)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip to horizontal bars
  labs(
    title = "Top 20 Bidirectional Domestic Flight Routes by Passenger Traffic",
    x = "Route",
    y = "Total Passengers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  scale_y_continuous(labels = scales::comma)

# Export data for visualisation in PowerBI
# Create a folder to save CSVs export_folder \<-
"C:/Users/tulas/Documents/Airports/Exports" dir.create(export_folder,
showWarnings = FALSE)

# Export files to the folder

write.csv(Final_data_clean, file.path(export_folder,
"Cleaned_Final_CityPair_Data.csv"), row.names = FALSE)
write.csv(top_10_routes, file.path(export_folder, "Top_10_Routes.csv"),
row.names = FALSE) write.csv(top_20_routes, file.path(export_folder,
"Top_20_Routes.csv"), row.names = FALSE)
write.csv(top_20_bidirectional_routes, file.path(export_folder,
"Top_20_Bidirectional_Routes.csv"), row.names = FALSE)
write.csv(bidirectional_routes, file.path(export_folder,