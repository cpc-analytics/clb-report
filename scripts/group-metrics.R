# Table 1

data1 <- data1 %>%
  mutate(last_seen_at = as.Date(last_seen_at))

# Debug: Print the active period range
print(paste("Previous Week Date:", previous_week_date))
print(paste("Current Week Date:", current_week_date))

# Filter for active users in the past week
active_users_debug <- data1 %>%
  filter(last_seen_at >= as.Date(previous_week_date) & last_seen_at <= as.Date(current_week_date))

# Print active user count
print(paste("Number of active users:", nrow(active_users_debug)))

print(head(active_users_debug))


# Function to process group memberships
process_groups <- function(data) {
  data %>%
    select(group_names, last_seen_at) %>%
    filter(!is.na(group_names)) %>%
    mutate(
      group_names = str_replace_all(group_names, "trust_level_[0-9];?", ""),  # Remove trust levels
      last_seen_at = as.Date(last_seen_at)  # Ensure Date format
    ) %>%
    separate_rows(group_names, sep = ";") %>%  # Split multiple group memberships
    filter(group_names != "")  # Remove empty values
}

# Process groups for total members
groups_total <- process_groups(data1) %>%
  count(group_names, name = "Total Users")

# Process groups for active users (last seen in the past week)
groups_active <- process_groups(active_users_debug) %>%
  count(group_names, name = "Active Users")

# Merge total users and active users
groups_engagement <- merge(groups_total, groups_active, by = "group_names", all.x = TRUE)
groups_engagement[is.na(groups_engagement)] <- 0  # Replace NA with 0

# Calculate percentage of active users per group
groups_engagement <- groups_engagement %>%
  mutate(Active_Percentage = round((`Active Users` / `Total Users`) * 100, 1)) %>%
  arrange(desc(Active_Percentage))  # Sort by engagement rate

# Rename columns dynamically
colnames(groups_engagement) <- c("Group", "Total Users", "Active Users", "Active (%)")





# Load required libraries
library(dplyr)
library(kableExtra)

# Create a new categorized table
groups_engagement_categorized <- groups_engagement %>%
  select(Group, `Active (%)`) %>%
  mutate(Engagement_Level = case_when(
    `Active (%)` >= 75 ~ "Highly Engaged (75%+)",
    `Active (%)` >= 50 ~ "Moderately Engaged (50-75%)",
    `Active (%)` >= 25 ~ "Low Engagement (25-50%)",
    TRUE ~ "Minimal Engagement (0-25%)"
  ))

# Combine groups into a single row per category
category_table <- groups_engagement_categorized %>%
  group_by(Engagement_Level) %>%
  summarise(Groups = paste(Group, collapse = "; ")) %>%
  arrange(desc(Engagement_Level))  # Ensure correct order

# Display the table using kable
category_table %>%
  kbl(
    format = "html",
    align = "l",
    col.names = c("Category", "Groups")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14
  ) %>%
  column_spec(2, width = "20em") %>%
  row_spec(0, bold = TRUE, color = "#FFFFFF", background = "#0073e6")  # Header row styling
