#### Preamble ####
# Purpose: Model development
# Author: Ziyi Liu
# Date: 18 April 2024
# Contact: jacqueline.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Required to know where to get Toronto Bicycle Theft data.
# Any other information required? No.


#### Workspace setting ####
library(tidyverse)
library(modelsummary)


#### Load data ####
data <- read_csv("outputs/data/cleaned_data.csv")


# Convert the 'value' variable to a binary numerical variable where 'high' is 1 and otherwise 0
data$value <- ifelse(data$value == "high", 1, 0)  

# Ensure 'OCC_HOUR' is numeric for analysis
data$OCC_HOUR <- as.numeric(data$OCC_HOUR)

# Convert 'OCC_MONTH' to a numerical value with 'January' as 1 through 'December' as 12
data <- data %>% 
  mutate(OCC_MONTH = case_when(
    OCC_MONTH == "January" ~ 1,
    OCC_MONTH == "February" ~ 2,
    OCC_MONTH == "March" ~ 3,
    OCC_MONTH == "April" ~ 4,
    OCC_MONTH == "May" ~ 5,
    OCC_MONTH == "June" ~ 6,
    OCC_MONTH == "July" ~ 7,
    OCC_MONTH == "August" ~ 8,
    OCC_MONTH == "September" ~ 9,
    OCC_MONTH == "October" ~ 10,
    OCC_MONTH == "November" ~ 11,
    OCC_MONTH == "December" ~ 12
  ))

# Prepare the model matrix for the logistic regression, excluding the first column which is the intercept
X <- model.matrix(value ~ location + BIKE_MAKE + OCC_HOUR + OCC_MONTH + BIKE_COLOUR, data = data)[, -1]

# Set up the response variable for the logistic regression
Y <- data$value

# Load the glmnet package for regularization techniques
library(glmnet)

# Perform cross-validation to determine the optimal lambda for LASSO regularization
cv.model <- cv.glmnet(x = X, y = Y, family = "binomial", alpha = 1)

# Extract the optimal lambda that minimizes cross-validation error
l.min <- cv.model$lambda.min

# Fit the LASSO model using the optimal lambda found in cross-validation
lasso.model <- glmnet(x = X, y = Y, family = "binomial", alpha = 1, lambda = l.min)

# Extract the coefficients from the fitted LASSO model
a <- as.matrix(lasso.model$beta)

# Create a summary data frame of terms and their coefficients from the LASSO model
lasso.sum <- data.frame(Term = rownames(a), Coefficient = as.numeric(a))

# Fit a logistic regression model with specified predictors without regularization
logit.model <- glm(value ~ location + BIKE_MAKE + BIKE_COLOUR, data = data)

#### Save and export model ####
# LOGISTIC MODEL
saveRDS(
  logit.model,
  file = "outputs/models/logit_model.rds"
)


