# ─────────────────────────────────────────────────────────────────────────────
# Pre-filter secretariat users
data1_secret <- data1 %>% filter(!grepl("CollabSecretariat", group_names, fixed = TRUE))
data2_secret <- data2 %>% filter(!grepl("CollabSecretariat", group_names, fixed = TRUE))

# ─────────────────────────────────────────────────────────────────────────────
# Shared Helper Functions

calculate_metrics <- function(data) {
  data %>%
    summarise(
      total_users = n(),
      avg_time_per_user = mean(time_read, na.rm = TRUE) / 60,
      avg_topics_read = mean(topics_entered, na.rm = TRUE),
      avg_posts_read = mean(posts_read_count, na.rm = TRUE),
      avg_likes_given = mean(likes_given, na.rm = TRUE),
      avg_topics_created = mean(topic_count, na.rm = TRUE),
      avg_posts_created = mean(post_count, na.rm = TRUE),
      avg_likes_received = mean(likes_received, na.rm = TRUE)
    ) %>%
    as.data.frame()
}

calculate_percentage_change <- function(new, old) {
  ifelse(old == 0, NA, round(((new - old) / old) * 100, 1))
}

format_numbers <- function(x, decimals = 1) {
  ifelse(x %% 1 == 0, format(x, big.mark = ","), format(round(x, decimals), nsmall = decimals, big.mark = ","))
}

format_percent <- function(x) {
  paste0(round(as.numeric(x), 1), "%")
}

# ─────────────────────────────────────────────────────────────────────────────
# Engagement Table (SECRETARIAT-FILTERED)

metrics_data1_secret <- calculate_metrics(data1_secret)
metrics_data2_secret <- calculate_metrics(data2_secret)

new_users_1_secret <- sum(!data1_secret$id %in% data2_secret$id)
new_users_2_secret <- sum(!data2_secret$id %in% data1_secret$id)

metrics_data1_secret$new_users <- new_users_1_secret
metrics_data2_secret$new_users <- new_users_2_secret

percent_change_values_secret <- mapply(calculate_percentage_change, metrics_data1_secret, metrics_data2_secret)
percent_change_secret <- as.data.frame(t(percent_change_values_secret))
colnames(percent_change_secret) <- paste0("percent_change_", colnames(metrics_data1_secret))

for (col in colnames(metrics_data1_secret)) {
  assign(paste0("secret_", col, "_1"), metrics_data1_secret[[col]], envir = .GlobalEnv)
  assign(paste0("secret_", col, "_2"), metrics_data2_secret[[col]], envir = .GlobalEnv)
  assign(paste0("secret_percent_change_", col), percent_change_secret[[paste0("percent_change_", col)]], envir = .GlobalEnv)
}

metrics_table_secret <- data.frame(
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
    format_numbers(secret_avg_time_per_user_1, 1),
    format_numbers(secret_avg_topics_read_1, 1),
    format_numbers(secret_avg_posts_read_1, 1),
    format_numbers(secret_avg_likes_given_1, 1),
    format_numbers(secret_avg_topics_created_1, 1),
    format_numbers(secret_avg_posts_created_1, 1),
    format_numbers(secret_avg_likes_received_1, 1)
  ),
  `Avg per User on {DATE2}` = c(
    format_numbers(secret_avg_time_per_user_2, 1),
    format_numbers(secret_avg_topics_read_2, 1),
    format_numbers(secret_avg_posts_read_2, 1),
    format_numbers(secret_avg_likes_given_2, 1),
    format_numbers(secret_avg_topics_created_2, 1),
    format_numbers(secret_avg_posts_created_2, 1),
    format_numbers(secret_avg_likes_received_2, 1)
  ),
  `Monthly Change` = c(
    format_percent(secret_percent_change_avg_time_per_user),
    format_percent(secret_percent_change_avg_topics_read),
    format_percent(secret_percent_change_avg_posts_read),
    format_percent(secret_percent_change_avg_likes_given),
    format_percent(secret_percent_change_avg_topics_created),
    format_percent(secret_percent_change_avg_posts_created),
    format_percent(secret_percent_change_avg_likes_received)
  )
)

colnames(metrics_table_secret) <- c(
  "Metric (AVG)",
  format(current_month_date, "%d %b %Y"),
  format(previous_month_date, "%d %b %Y"),
  "Monthly Change"
)



# Activity Table – Excluding Secretariat

activity_data1_secret <- categorize_activity(data1_secret, reference_date)
activity_data2_secret <- categorize_activity(data2_secret, reference_date %m-% months(1))

activity_table_secret <- merge(activity_data1_secret, activity_data2_secret, by = "Category", all = TRUE, suffixes = c("_data1", "_data2"))
activity_table_secret[is.na(activity_table_secret)] <- 0

activity_table_secret$Change <- ifelse(
  activity_table_secret$Users_data2 == 0, "N/A",
  paste0(round(((activity_table_secret$Users_data1 - activity_table_secret$Users_data2) / activity_table_secret$Users_data2) * 100, 1), "%")
)

activity_total_row_secret <- data.frame(
  Category = "Total",
  Users_data1 = sum(activity_table_secret$Users_data1),
  Users_data2 = sum(activity_table_secret$Users_data2),
  Change = if (sum(activity_table_secret$Users_data2) == 0) "N/A" else
    paste0(round(((sum(activity_table_secret$Users_data1) - sum(activity_table_secret$Users_data2)) / sum(activity_table_secret$Users_data2)) * 100, 1), "%")
)

activity_table_secret <- bind_rows(activity_table_secret, activity_total_row_secret)

colnames(activity_table_secret) <- c(
  "Category",
  format(current_month_date, "%d %b %Y"),
  format(previous_month_date, "%d %b %Y"),
  "Monthly Change"
)


# Engagement Table – Excluding Secretariat

engagement_data1_secret <- categorize_time_spent(data1_secret, reference_date)
engagement_data2_secret <- categorize_time_spent(data2_secret, reference_date %m-% months(1))

engagement_table_secret <- merge(engagement_data1_secret, engagement_data2_secret, by = "Category", all = TRUE, suffixes = c("_data1", "_data2"))
engagement_table_secret[is.na(engagement_table_secret)] <- 0

engagement_table_secret$Change <- ifelse(
  engagement_table_secret$Users_data2 == 0, "N/A",
  paste0(round(((engagement_table_secret$Users_data1 - engagement_table_secret$Users_data2) / engagement_table_secret$Users_data2) * 100, 1), "%")
)

engagement_total_row_secret <- data.frame(
  Category = "Total",
  Users_data1 = sum(engagement_table_secret$Users_data1),
  Users_data2 = sum(engagement_table_secret$Users_data2),
  Change = if (sum(engagement_table_secret$Users_data2) == 0) "N/A" else
    paste0(round(((sum(engagement_table_secret$Users_data1) - sum(engagement_table_secret$Users_data2)) / sum(engagement_table_secret$Users_data2)) * 100, 1), "%")
)

engagement_table_secret <- bind_rows(engagement_table_secret, engagement_total_row_secret)

engagement_table_secret$Category <- factor(engagement_table_secret$Category, levels = c(
  "Highly engaged (15+ min/month)",
  "Moderately engaged (2-15 min/month)",
  "Barely engaged (under 2 min/month)",
  "Total"
))

engagement_table_secret <- engagement_table_secret %>% arrange(Category)

colnames(engagement_table_secret) <- c(
  "Category",
  format(current_month_date, "%d %b %Y"),
  format(previous_month_date, "%d %b %Y"),
  "Monthly Change"
)
