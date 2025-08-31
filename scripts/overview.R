# Overview

data1 <- data1 %>% filter(!(username %in% c("discobot", "system")))
data2 <- data2 %>% filter(!(username %in% c("discobot", "system")))

# Extract the dates for the two datasets from file_info
current_month_date <- file_info %>%
  filter(file == latest_file$file) %>%
  pull(date)

previous_month_date <- file_info %>%
  filter(file == one_month_prior_file$file) %>%
  pull(date)

# Function to Calculate Monthly Metrics
calculate_monthly_metrics <- function(data1, data2, current_month_date, previous_month_date) {
  # Total users in the latest dataset
  total_users <- nrow(data1)
  
  # New users joined in the last month
  users_joined_last_month <- nrow(data1) - nrow(data2)
  
  # Ensure last_seen_at is in POSIXct format
  data1 <- data1 %>%
    mutate(last_seen_at = as.POSIXct(last_seen_at, tz = "UTC"),
           last_seen_date = as.Date(last_seen_at))
  
  data2 <- data2 %>%
    mutate(last_seen_at = as.POSIXct(last_seen_at, tz = "UTC"),
           last_seen_date = as.Date(last_seen_at))
  
  # Align activity window with Engagement metrics: 30 days before reference
  reference_date_current <- as.Date(current_month_date)
  reference_date_previous <- as.Date(previous_month_date)
  
  users_active_last_month <- data1 %>%
    filter(last_seen_date >= reference_date_current - 30) %>%
    nrow()
  
  users_active_previous_month <- data2 %>%
    filter(last_seen_date >= reference_date_previous - 30) %>%
    nrow()
  
  # Calculate percentage change in active users
  percent_change_active_users <- ifelse(
    users_active_previous_month == 0,
    NA,
    round(((users_active_last_month - users_active_previous_month) / users_active_previous_month) * 100, 1)
  )
  
  # Return named list of metrics
  list(
    total_users = total_users,
    users_joined_last_month = users_joined_last_month,
    users_active_last_month = users_active_last_month,
    users_active_previous_month = users_active_previous_month,
    percent_change_active_users = percent_change_active_users,
    current_month_date = current_month_date
  )
}

# Calculate the metrics
metrics <- calculate_monthly_metrics(data1, data2, current_month_date, previous_month_date)

# Explicitly extract values
total_users <- metrics$total_users
users_joined_last_month <- metrics$users_joined_last_month
users_active_last_month <- metrics$users_active_last_month
users_active_previous_month <- metrics$users_active_previous_month
percent_change_active_users <- metrics$percent_change_active_users
current_month_date <- metrics$current_month_date

# Ensure created_at is parsed and usable
if ("created_at" %in% names(data1)) {
  data1 <- data1 %>%
    mutate(
      created_at = as.POSIXct(created_at, tz = "UTC"),
      created_date = as.Date(created_at)
    )
  
  # Filter to users who joined during the past month
  joined_this_month <- data1 %>%
    filter(created_date >= previous_month_date & created_date <= current_month_date)
  
  # Daily join counts
  daily_joins <- joined_this_month %>%
    count(created_date, name = "new_users")
  
  # Day with highest new joins
  peak_join_day <- daily_joins %>%
    filter(new_users == max(new_users)) %>%
    slice(1)  # just in case of ties
  
  # Count days with more than 10 new users
  spike_days <- daily_joins %>%
    filter(new_users > 10)
  
} else {
  peak_join_day <- NULL
  spike_days <- NULL
}
