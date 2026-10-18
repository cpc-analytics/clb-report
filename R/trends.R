# Monthly signups, posts, likes and topics from the forum statistics exports
# (data/forum-stats/<type>-YYYY-MM-DD.csv). These files are daily totals (Day, Count)
# and hold no member-level data. The latest file of each type is used.
# Defines forum_stats_date, signups_monthly_filtered and combined_monthly.

library(dplyr)
library(readr)
library(stringr)
library(lubridate)

stats_dir <- cfg$dir_forum_stats

# Latest file of a given type, e.g. "posts-2026-01-31.csv"
get_latest_file <- function(prefix) {
  pattern <- paste0("^", prefix, "-(\\d{4}-\\d{2}-\\d{2})\\.csv$")
  files <- list.files(stats_dir, pattern = pattern)
  if (length(files) == 0) stop("No '", prefix, "' file found in ", stats_dir, call. = FALSE)
  dates <- ymd(str_match(files, pattern)[, 2])
  list(file = file.path(stats_dir, files[which.max(dates)]), date = max(dates))
}

signups_info <- get_latest_file("signups")
posts_info   <- get_latest_file("posts")
likes_info   <- get_latest_file("likes")
topics_info  <- get_latest_file("topics")

# Date the forum statistics are current to
forum_stats_date <- max(signups_info$date, posts_info$date, likes_info$date, topics_info$date)

monthly_total <- function(info, name) {
  read_csv(info$file, show_col_types = FALSE) %>%
    mutate(Month = format(as.Date(Day), "%Y-%m")) %>%
    group_by(Month) %>%
    summarise(!!name := sum(Count, na.rm = TRUE), .groups = "drop") %>%
    arrange(Month)
}

signups_monthly_filtered <- monthly_total(signups_info, "Total_Signups") %>%
  mutate(Month = as.Date(paste0(Month, "-01")))

combined_monthly <- monthly_total(posts_info, "Total_Posts") %>%
  full_join(monthly_total(likes_info, "Total_Likes"), by = "Month") %>%
  full_join(monthly_total(topics_info, "Total_Topics"), by = "Month") %>%
  mutate(Month = as.Date(paste0(Month, "-01")))
