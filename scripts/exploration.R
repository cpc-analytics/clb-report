# exploration.R

# Libraries
library(readxl)  
library(dplyr)   
library(tidyr)   

# Load the data
forum_data <- read_excel("data/discussion-forum-data.xlsx")
variable_descriptions <- read_excel("data/discussion-forum-data.xlsx", sheet = 2)

# Print variable names and count in a table
variables <- colnames(forum_data)  
num_variables <- length(variables)

cat("List of Variables:\n")
print(data.frame(Variable = variables))
cat("\nTotal number of variables:", num_variables, "\n\n")

# Check how full each variable is (percentage of filled/valid values, i.e., non-missing)
variable_completeness <- forum_data %>%
  summarise(across(everything(), ~round(sum(!is.na(.)) / n() * 100, 2))) %>%
  gather(key = "variable", value = "filled_percentage")

# Identify variables that are filled less than 80%
variables_below_80 <- variable_completeness %>%
  filter(filled_percentage < 80)  # Filter to only show variables below 80% filled

# Output the filtered table of variables
cat("Variables with less than 80% filled data:\n")
print(variables_below_80)

# Calculate the sheet-wide percentage of entries that are filled
total_filled <- sum(!is.na(forum_data))  # Count of all non-missing values
total_entries <- nrow(forum_data) * ncol(forum_data)  # Total number of entries in the entire sheet
sheet_completeness_percentage <- round((total_filled / total_entries) * 100, 2)  # Percentage of filled entries

# Output the total sheet-wide filled percentage
cat("\nTotal sheet filled percentage:", sheet_completeness_percentage, "%")
