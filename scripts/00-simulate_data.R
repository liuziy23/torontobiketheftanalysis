#### Preamble ####
# Purpose: Simulates the data desired.
# Author: Ziyi Liu
# Data: 20 April 2024
# Contact: jacqueline.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to know where to get analysis dataset (Toronto bike theft data).
# Any other information required? None.


#### Workspace setting ####
library(tidyverse)


library(dplyr)
library(stringi) # for generating random strings

set.seed(1)

# Simulating the data
simulated_bike_data <- tibble(
  # Simulate bike ID
  bike_id = 1:200,
  # Simulate occurrence year between 2014 and 2023
  OCC_YEAR = sample(2014:2023, 200, replace = TRUE),
  # Simulate bike cost between 100 and 5000
  BIKE_COST = runif(200, min = 100, max = 5000),
  # Simulate premises type
  PREMISES_TYPE = sample(c("Commercial", "Apartment", "Public Space", "Other"), 200, replace = TRUE),
  # Simulate occurrence month
  OCC_MONTH = sample(month.name, 200, replace = TRUE),
  # Simulate bike make with some common and other random makes
  BIKE_MAKE = sample(c("Trek", "Giant", "Specialized", stri_rand_strings(50, 7)), 200, replace = TRUE),
  # Simulate bike colour
  BIKE_COLOUR = sample(c("Red", "Blue", "Black", "Green", "Yellow", "Other"), 200, replace = TRUE),
  # Simulate occurrence hour
  OCC_HOUR = sample(0:23, 200, replace = TRUE)
) %>%
  mutate(
    value = if_else(BIKE_COST > 1000, "high", "low"),
    location = case_when(
      PREMISES_TYPE %in% c("Commercial", "Apartment") ~ "inside",
      TRUE ~ "outside"
    ),
    OCC_MONTH = factor(OCC_MONTH, levels = month.name),
    BIKE_MAKE = if_else(BIKE_MAKE %in% names(which(table(BIKE_MAKE) > 10)), BIKE_MAKE, "Other"),
    BIKE_COLOUR = if_else(BIKE_COLOUR %in% names(which(table(BIKE_COLOUR) > 30)), BIKE_COLOUR, "Other"),
    OCC_HOUR = factor(OCC_HOUR),
    value = factor(value)
  ) %>%
  filter(!is.na(value))

# View the head of the simulated data
head(simulated_bike_data)

