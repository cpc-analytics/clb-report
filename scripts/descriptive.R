# descriptive.R

# Libraries
library(dplyr)
library(tidyr)
library(readxl)
library(ip2location)
library(reticulate)

# Load the dataset using readxl
forum_data <- read_excel("data/discussion-forum-data.xlsx")

# Add dummy columns for testing
forum_data <- forum_data %>%
  mutate(
    sex = sample(c("Woman", "Man", "Non-binary"), nrow(forum_data), replace = TRUE), # Random sex assignment
    age = sample(c(15, 25, 35, 45, 55), nrow(forum_data), replace = TRUE),          # Random age assignment
    country = sample(c("Country1", "Country2", "Country3"), nrow(forum_data), replace = TRUE), # Random country
    new_member = sample(c(TRUE, FALSE), nrow(forum_data), replace = TRUE, prob = c(0.1, 0.9))  # Random new member flag
  )

# Number of members
num_members <- nrow(forum_data)

# Number of new members (dummy calculation for now)
num_new_members <- sum(forum_data$new_member)

# Members by sex
members_by_sex <- forum_data %>%
  count(sex, name = "Count") %>%
  complete(sex = c("Woman", "Man", "Non-binary"), fill = list(Count = 0)) # Ensure all categories are shown

# Members by age group
age_groups <- forum_data %>%
  mutate(
    age_group = case_when(
      age < 18 ~ "<18",
      age >= 18 & age < 30 ~ "18-29",
      age >= 30 & age < 50 ~ "30-49",
      age >= 50 ~ "50+"
    )
  ) %>%
  count(age_group, name = "Count") %>%
  complete(age_group = c("<18", "18-29", "30-49", "50+"), fill = list(Count = 0)) # Ensure all groups are shown

# Add dummy change values for testing
change_values <- c(
  "+10%", "-5%", "",
  members_by_sex %>% mutate(Change = c("+2%", "-3%", "+1%")) %>% pull(Change),
  "",
  age_groups %>% mutate(Change = c("+1%", "+3%", "-2%", "0%")) %>% pull(Change)
)

# Create a summary table for demographics
demographics_table <- data.frame(
  Metric = c(
    "Number of Members",
    "Number of New Members",
    "Members by Sex",
    paste(" - ", members_by_sex$sex),
    "Members by Age Group",
    paste(" - ", age_groups$age_group)
  ),
  Value = c(
    num_members,
    num_new_members,
    "",
    members_by_sex$Count,
    "",
    age_groups$Count
  ),
  Change = change_values # Include the dummy change column
)

# Populate the country column in forum_data with a weighted random distribution
set.seed(123)  # For reproducibility
forum_data <- forum_data %>%
  mutate(
    country = sample(
      c("Germany", "Switzerland", "USA", "Canada", "UK", "France", "India", "China", "Australia", "Brazil"), 
      n(), 
      replace = TRUE, 
      prob = c(0.4, 0.2, 0.2, 0.05, 0.05, 0.03, 0.02, 0.02, 0.015, 0.015) # Higher emphasis on Germany, Switzerland, and USA
    )
  )

# Calculate number of users per country and their percentage, sorted by Number of Users descending
countries_table <- forum_data %>%
  count(country, name = "Number of Users") %>%
  mutate(Percentage = round(`Number of Users` / sum(`Number of Users`) * 100, 1)) %>%
  arrange(desc(`Number of Users`)) %>%
  mutate(Percentage = paste0(Percentage, "%"))


# Count emails by domain (who.int vs others)
email_table <- forum_data %>%
  mutate(domain = sub(".*@", "", email)) %>% # Extract domain from email
  mutate(domain_category = ifelse(domain == "who.int", "WHO", "Non-WHO")) %>%
  count(domain_category, name = "Number of Emails") %>%
  mutate(Percentage = paste0(round(`Number of Emails` / sum(`Number of Emails`) * 100, 1), "%"))



# Categorize users based on last seen, focusing only on active users
engagement_table <- forum_data %>%
  mutate(
    last_seen_category = case_when(
      as.Date(Sys.Date()) - as.Date(last_seen_at) <= 30 ~ "Active",
      as.Date(Sys.Date()) - as.Date(last_seen_at) > 30 & as.Date(Sys.Date()) - as.Date(last_seen_at) <= 60 ~ "Dormant",
      TRUE ~ "Uninterested"
    )
  ) %>%
  count(last_seen_category, name = "Number of Users") %>%
  complete(last_seen_category = c("Active", "Dormant", "Uninterested"), fill = list(`Number of Users` = 0)) %>%
  mutate(Percentage = paste0(round(`Number of Users` / sum(`Number of Users`) * 100, 1), "%"))


# Calculate engagement averages and pivot the table for two-column format
engagement_averages <- forum_data %>%
  summarise(
    `Average Time per User (minutes)` = round(mean(time_read / 60, na.rm = TRUE), 2),
    `Average Topics Read` = round(mean(topics_entered, na.rm = TRUE), 2),
    `Average Posts Read` = round(mean(posts_read_count, na.rm = TRUE), 2),
    `Average Likes Given` = round(mean(likes_given, na.rm = TRUE), 2),
    `Average Topics Created` = round(mean(topic_count, na.rm = TRUE), 2),
    `Average Posts Created` = round(mean(post_count, na.rm = TRUE), 2),
    `Average Likes Received` = round(mean(likes_received, na.rm = TRUE), 2)
  ) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Value")

# Threshold analysis for time spent
threshold_analysis <- forum_data %>%
  mutate(
    time_category = case_when(
      time_read >= 15 * 60 ~ "15 min/month or more",
      time_read < 1 * 60 ~ "Less than 1 min in total",
      TRUE ~ "Other"
    )
  ) %>%
  count(time_category, name = "Number of Users") %>%
  mutate(Percentage = paste0(round(`Number of Users` / sum(`Number of Users`) * 100, 1), "%"))