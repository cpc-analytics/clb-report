# Loads the latest weekly snapshot and the one closest to a month earlier.
# Reads data/clean/ only (see ingest-data.R). Defines:
#   data1, data2                      latest snapshot, previous snapshot
#   snapshot_date, previous_snapshot_date
#   snapshots_index                   file and date of every weekly snapshot
#   total_users, users_joined_last_month

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
library(readr)
library(stringr)
library(lubridate)

source(here::here("R", "read-data.R"))

snapshots_index <- list_snapshots()

latest_snap <- slice_tail(snapshots_index, n = 1)
target_date <- latest_snap$date %m-% months(1)

# Snapshot closest to one month before the latest, not after it
previous_snap <- snapshots_index %>%
  filter(date <= target_date) %>%
  slice_tail(n = 1)

if (nrow(previous_snap) == 0) {
  stop("No snapshot from a month or more before ", format(latest_snap$date),
       ", so no monthly comparison is possible.", call. = FALSE)
}

snapshot_date          <- latest_snap$date
previous_snapshot_date <- previous_snap$date

gap_days <- as.integer(snapshot_date - previous_snapshot_date)
if (gap_days < cfg$snapshot_gap_min_days) {
  warning("Gap between snapshots is only ", gap_days, " days. Check the export dates.")
} else if (gap_days > cfg$snapshot_gap_max_days) {
  warning("Gap between snapshots is ", gap_days, " days. Too long for a monthly comparison.")
}

data1 <- read_snapshot(latest_snap$file)    # current
data2 <- read_snapshot(previous_snap$file)  # about one month earlier

if (!identical(names(data1), names(data2))) {
  message("The two snapshots have different columns:")
  message("  only in latest:   ", paste(setdiff(names(data1), names(data2)), collapse = ", "))
  message("  only in previous: ", paste(setdiff(names(data2), names(data1)), collapse = ", "))
}

# Headline counts. A new member is an account created since the previous snapshot.
total_users             <- nrow(data1)
users_joined_last_month <- new_members(data1, previous_snapshot_date, snapshot_date)
