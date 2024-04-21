#### Preamble ####
# Purpose: Test simulated data
# Author: Ziyi Liu
# Data: 20 April 2024
# Contact: jacqueline.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Create simulated data before this
# Any other required needed? None.


#### Workspace setting ####
library(tidyverse)

# test 1: Check if 'OCC_MONTH' is stored as a factor
test_1 <- simulated_bike_data$OCC_MONTH |> 
  class() == "factor"

# test 2: Check if the maximum 'OCC_YEAR' is 2023
test_2 <- simulated_bike_data$OCC_YEAR |> 
  max() == 2023

# test 3: Check if 'BIKE_COST' is stored as numeric
test_3 <- simulated_bike_data$BIKE_COST |> 
  class() == "numeric"

# test 4: Check the number of unique categories in 'value'
num_uniq_values <-
  simulated_bike_data$value |>
  unique()
test_4 <- length(num_uniq_values) == 2

# Output results of tests
list(
  test_1 = test_1,
  test_2 = test_2,
  test_3 = test_3,
  test_4 = test_4
)
