
# Remove system accounts from both datasets for consistency
data1 <- data1 %>% filter(!(username %in% c("discobot", "system")))
data2 <- data2 %>% filter(!(username %in% c("discobot", "system")))


# TABLE 1 – MONTHLY

# Function to calculate key metrics for a dataset
calculate_metrics <- function(data) {
  data %>%
    summarise(
      total_users = n(),
      avg_time_per_user = mean(time_read, na.rm = TRUE) / 60,  # Convert seconds to minutes
      avg_topics_read = mean(topics_entered, na.rm = TRUE),
      avg_posts_read = mean(posts_read_count, na.rm = TRUE),
      avg_likes_given = mean(likes_given, na.rm = TRUE),
      avg_topics_created = mean(topic_count, na.rm = TRUE),
      avg_posts_created = mean(post_count, na.rm = TRUE),
      avg_likes_received = mean(likes_received, na.rm = TRUE)
    ) %>%
    as.data.frame()
}

# Calculate metrics for current and previous month datasets
metrics_data1 <- calculate_metrics(data1)
metrics_data2 <- calculate_metrics(data2)

# Calculate number of new users in each snapshot
new_users_1 <- sum(!data1$id %in% data2$id)
new_users_2 <- sum(!data2$id %in% data1$id)

# Add new users to the metric summaries
metrics_data1$new_users <- new_users_1
metrics_data2$new_users <- new_users_2

# Function to calculate percentage change
calculate_percentage_change <- function(new, old) {
  ifelse(old == 0, NA, round(((new - old) / old) * 100, 1))
}

# Compute percentage changes for each metric
percent_change_values <- mapply(calculate_percentage_change, metrics_data1, metrics_data2)
percent_change <- as.data.frame(t(percent_change_values))
colnames(percent_change) <- paste0("percent_change_", colnames(metrics_data1))

# Assign individual variables for RMarkdown inline use
for (col in colnames(metrics_data1)) {
  assign(paste0(col, "_1"), metrics_data1[[col]], envir = .GlobalEnv)
  assign(paste0(col, "_2"), metrics_data2[[col]], envir = .GlobalEnv)
  assign(paste0("percent_change_", col), percent_change[[paste0("percent_change_", col)]], envir = .GlobalEnv)
}

# Formatting functions
format_numbers <- function(x, decimals = 1) {
  ifelse(x %% 1 == 0, format(x, big.mark = ","), format(round(x, decimals), nsmall = decimals, big.mark = ","))
}

format_percent <- function(x) {
  paste0(round(as.numeric(x), 1), "%")
}

# Format the date labels for the columns
month_of_data1 <- format(current_month_date, "%d %b %Y")
month_of_data2 <- format(previous_month_date, "%d %b %Y")

metrics_table <- data.frame(
  Metric = c(
    "Time spent (mins)",
    "Topics entered",
    "Posts read",
    "Likes given",
    "Topics created",
    "Posts created",
    "Likes received"
  ),
  `Avg per User on {DATE1}` = c(
    format_numbers(avg_time_per_user_1, 1),
    format_numbers(avg_topics_read_1, 1),
    format_numbers(avg_posts_read_1, 1),
    format_numbers(avg_likes_given_1, 1),
    format_numbers(avg_topics_created_1, 1),
    format_numbers(avg_posts_created_1, 1),
    format_numbers(avg_likes_received_1, 1)
  ),
  `Avg per User on {DATE2}` = c(
    format_numbers(avg_time_per_user_2, 1),
    format_numbers(avg_topics_read_2, 1),
    format_numbers(avg_posts_read_2, 1),
    format_numbers(avg_likes_given_2, 1),
    format_numbers(avg_topics_created_2, 1),
    format_numbers(avg_posts_created_2, 1),
    format_numbers(avg_likes_received_2, 1)
  ),
  `Monthly Change` = c(
    format_percent(percent_change_avg_time_per_user),
    format_percent(percent_change_avg_topics_read),
    format_percent(percent_change_avg_posts_read),
    format_percent(percent_change_avg_likes_given),
    format_percent(percent_change_avg_topics_created),
    format_percent(percent_change_avg_posts_created),
    format_percent(percent_change_avg_likes_received)
  )
)

# Update column names dynamically with "Avg per User — Month" format
colnames(metrics_table) <- c(
  "Metric (AVG)",
  format(current_month_date, "%d %b %Y"),
  format(previous_month_date, "%d %b %Y"),
  "Monthly Change"
)







# TABLE 2 – Monthly Activity Groups

# Ensure last_seen_at is a Date object
data1 <- data1 %>% mutate(last_seen_at = as.Date(ymd_hms(last_seen_at)))
data2 <- data2 %>% mutate(last_seen_at = as.Date(ymd_hms(last_seen_at)))

reference_date <- as.Date(current_month_date)

# Function to classify users into activity groups
categorize_activity <- function(data, reference_date) {
  data %>%
    mutate(
      last_seen_date = as.Date(last_seen_at),
      activity_group = case_when(
        last_seen_date >= reference_date - 30 ~ "Active",
        last_seen_date >= reference_date - 60 & last_seen_date < reference_date - 30 ~ "Dormant",
        last_seen_date < reference_date - 60 ~ "Uninterested",
        TRUE ~ "Unknown"
      )
    ) %>%
    count(activity_group) %>%
    rename(Category = activity_group, Users = n)
}

# Apply classification
activity_data1 <- categorize_activity(data1, reference_date)
activity_data2 <- categorize_activity(data2, reference_date %m-% months(1))

# Merge and calculate change
activity_table <- merge(activity_data1, activity_data2, by = "Category", all = TRUE, suffixes = c("_data1", "_data2"))
activity_table[is.na(activity_table)] <- 0

activity_table$Change <- ifelse(
  activity_table$Users_data2 == 0, "N/A",
  paste0(round(((activity_table$Users_data1 - activity_table$Users_data2) / activity_table$Users_data2) * 100, 1), "%")
)

# Add total row
activity_total_row <- data.frame(
  Category = "Total",
  Users_data1 = sum(activity_table$Users_data1),
  Users_data2 = sum(activity_table$Users_data2),
  Change = if (sum(activity_table$Users_data2) == 0) "N/A" else
    paste0(round(((sum(activity_table$Users_data1) - sum(activity_table$Users_data2)) / sum(activity_table$Users_data2)) * 100, 1), "%")
)

activity_table <- bind_rows(activity_table, activity_total_row)

# Rename columns with month
colnames(activity_table) <- c(
  "Category",
  format(current_month_date, "%d %b %Y"),
  format(previous_month_date, "%d %b %Y"),
  "Monthly Change"
)







# TABLE 3 – Monthly Engagement Levels

# Convert and clean up columns
data1 <- data1 %>% mutate(time_read = as.numeric(time_read), time_read_min = time_read / 60)
data2 <- data2 %>% mutate(time_read = as.numeric(time_read), time_read_min = time_read / 60)

data1 <- data1 %>% mutate(created_at = as.Date(ymd_hms(created_at)))
data2 <- data2 %>% mutate(created_at = as.Date(ymd_hms(created_at)))

# Categorization function
categorize_time_spent <- function(data, reference_date) {
  data %>%
    mutate(
      months_active = as.numeric(difftime(reference_date, created_at, units = "days")) / 30,
      months_active = ifelse(months_active < 1, 1, months_active),
      time_read_per_month = time_read_min / months_active,
      engagement_group = case_when(
        time_read_per_month >= 15 ~ "Highly engaged (15+ min/month)",
        time_read_per_month >= 2 & time_read_per_month < 15 ~ "Moderately engaged (2-15 min/month)",
        time_read_per_month < 2 ~ "Barely engaged (under 2 min/month)",
        TRUE ~ "Unknown"
      )
    ) %>%
    count(engagement_group) %>%
    rename(Category = engagement_group, Users = n)
}

# Apply function
engagement_data1 <- categorize_time_spent(data1, reference_date)
engagement_data2 <- categorize_time_spent(data2, reference_date %m-% months(1))

# Merge, fill NAs, calculate changes
engagement_table <- merge(engagement_data1, engagement_data2, by = "Category", all = TRUE, suffixes = c("_data1", "_data2"))
engagement_table[is.na(engagement_table)] <- 0

engagement_table$Change <- ifelse(
  engagement_table$Users_data2 == 0, "N/A",
  paste0(round(((engagement_table$Users_data1 - engagement_table$Users_data2) / engagement_table$Users_data2) * 100, 1), "%")
)

# Add total row
engagement_total_row <- data.frame(
  Category = "Total",
  Users_data1 = sum(engagement_table$Users_data1),
  Users_data2 = sum(engagement_table$Users_data2),
  Change = if (sum(engagement_table$Users_data2) == 0) "N/A" else
    paste0(round(((sum(engagement_table$Users_data1) - sum(engagement_table$Users_data2)) / sum(engagement_table$Users_data2)) * 100, 1), "%")
)

engagement_table <- bind_rows(engagement_table, engagement_total_row)

# Explicit sorting (before renaming columns)
engagement_table$Category <- factor(engagement_table$Category, levels = c(
  "Highly engaged (15+ min/month)",
  "Moderately engaged (2-15 min/month)",
  "Barely engaged (under 2 min/month)",
  "Total"
))

engagement_table <- engagement_table %>% arrange(Category)

# Rename columns with month
colnames(engagement_table) <- c(
  "Category",
  format(current_month_date, "%d %b %Y"),
  format(previous_month_date, "%d %b %Y"),
  "Monthly Change"
)


