library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)

# Base path
file_path <- "data/HERA/"

# Helper to get latest file of a given type
get_latest_file <- function(prefix) {
  files <- list.files(file_path, pattern = paste0("^", prefix, "-\\d{6}\\.csv$"))
  dates <- str_match(files, paste0("^", prefix, "-(\\d{6})\\.csv$"))[, 2]
  dates <- dates[!is.na(dates)]
  latest <- dates[which.max(ymd(paste0("20", dates)))]
  list(file = paste0(prefix, "-", latest, ".csv"), date = ymd(paste0("20", latest)))
}

# Load each dataset from latest version
user_info   <- get_latest_file("user-list")
signups_info <- get_latest_file("signups")
posts_info   <- get_latest_file("posts")
likes_info   <- get_latest_file("likes")
topics_info  <- get_latest_file("topics")

# Use the most recent of all file dates as the report date
report_date <- max(user_info$date, signups_info$date, posts_info$date, likes_info$date, topics_info$date)

# Read data
users   <- read_csv(file.path(file_path, user_info$file))
signups <- read_csv(file.path(file_path, signups_info$file))
posts   <- read_csv(file.path(file_path, posts_info$file))
likes   <- read_csv(file.path(file_path, likes_info$file))
topics  <- read_csv(file.path(file_path, topics_info$file))

# Prepare signups monthly
signups <- signups %>% mutate(Day = as.Date(Day))
signups_monthly <- signups %>%
  mutate(Month = format(Day, "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Total_Signups = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  arrange(Month)

signups_monthly_filtered <- signups_monthly %>%
  mutate(Month = as.Date(paste0(Month, "-01")))

total_signups <- sum(signups_monthly$Total_Signups)

# Prepare WHO email proportions
filtered_users <- users %>%
  filter(!(email %in% c("discobot_email", "no_email"))) %>%
  mutate(domain = ifelse(grepl("@who\\.int$", email), "WHO Members", "Non-WHO Members")) %>%
  count(domain) %>%
  mutate(percentage = paste0(round(n / sum(n) * 100, 1), "%")) %>%
  arrange(desc(domain)) %>%
  mutate(y_position = cumsum(n) - (n / 2))

# Prepare monthly activity
posts_monthly <- posts %>%
  mutate(Month = format(as.Date(Day), "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Total_Posts = sum(Count, na.rm = TRUE), .groups = "drop")

likes_monthly <- likes %>%
  mutate(Month = format(as.Date(Day), "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Total_Likes = sum(Count, na.rm = TRUE), .groups = "drop")

topics_monthly <- topics %>%
  mutate(Month = format(as.Date(Day), "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Total_Topics = sum(Count, na.rm = TRUE), .groups = "drop")

combined_monthly <- full_join(posts_monthly, likes_monthly, by = "Month") %>%
  full_join(topics_monthly, by = "Month") %>%
  mutate(Month = as.Date(paste0(Month, "-01")))

combined_monthly_with_total <- combined_monthly %>%
  summarise(Month = as.Date(NA),
            Total_Posts = sum(Total_Posts, na.rm = TRUE),
            Total_Likes = sum(Total_Likes, na.rm = TRUE),
            Total_Topics = sum(Total_Topics, na.rm = TRUE)) %>%
  bind_rows(combined_monthly)

# Cumulative values
cumulative_data <- combined_monthly %>%
  arrange(Month) %>%
  mutate(
    Cumulative_Topics = cumsum(Total_Topics),
    Cumulative_Posts = cumsum(Total_Posts),
    Cumulative_Likes = cumsum(Total_Likes)
  )
