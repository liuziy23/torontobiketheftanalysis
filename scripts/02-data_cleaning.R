#### Preamble ####
# Purpose: Processes the raw dataset
# Author: Ziyi Liu
# Date: 18 April 2024
# Contact: jacqueline.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Required to have raw data downloaded.
# Any other information required? No.


#### Workspace setting ####
library(ggplot2)
library(readr)


#### Process data ####
### Load raw data ###
data <- read_csv("inputs/data/Bicycle_Thefts_Open_Data.csv")

# Assuming the data has already been read and pre-processed
# Load necessary library
library(dplyr)

# Update the 'data' data frame by filtering and mutating specific columns
data <- data %>%
  # Filter rows where the 'OCC_YEAR' column value is greater than 2013
  filter(OCC_YEAR > 2013) %>%
  # Add new columns with conditional values and transformations
  mutate(
    # Create 'value' column: 'high' if 'BIKE_COST' is over 1000, otherwise 'low'
    value = if_else(BIKE_COST > 1000, "high", "low"),
    # Create 'location' column: 'inside' if 'PREMISES_TYPE' is Commercial or Apartment, otherwise 'outside'
    location = case_when(
      PREMISES_TYPE %in% c("Commercial", "Apartment") ~ "inside",
      TRUE ~ "outside"
    ),
    # Convert 'OCC_MONTH' to a factor with specific levels representing months in order
    OCC_MONTH = factor(OCC_MONTH, levels = c(
      "January", "February", "March", "April",
      "May", "June", "July", "August", "September",
      "October", "November", "December"
    )),
    # Recode 'BIKE_MAKE': keep as is if occurrence > 600, else label as 'Other'
    BIKE_MAKE = if_else(BIKE_MAKE %in% names(which(table(BIKE_MAKE) > 600)), BIKE_MAKE, "Other"),
    # Recode 'BIKE_COLOUR': keep as is if occurrence > 1000, else label as 'Other'
    BIKE_COLOUR = if_else(BIKE_COLOUR %in% names(which(table(BIKE_COLOUR) > 1000)), BIKE_COLOUR, "Other"),
    # Convert 'OCC_HOUR' to a factor
    OCC_HOUR = factor(OCC_HOUR),
    # Ensure 'value' is a factor for categorical analysis
    value = factor(value)
  ) %>%
  # Filter out rows where 'value' is NA to ensure data integrity
  filter(!is.na(value))

# The transformed data frame is now stored in 'data' and is ready for further analysis

#### Save and export data ####
write_csv(data, "outputs/data/cleaned_data.csv")

