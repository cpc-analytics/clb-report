# File readers for the cleaned data. Requires config.R and metrics.R.
# Nothing here reads raw exports (see ingest-data.R).

library(readr)
library(stringr)
library(dplyr)

list_snapshots <- function() {
  files <- list.files(cfg$dir_clean, pattern = "^users-\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No snapshots in ", cfg$dir_clean, ". Run scripts/ingest-data.R first.", call. = FALSE)
  }
  tibble(
    file = files,
    date = as.Date(str_extract(basename(files), "\\d{4}-\\d{2}-\\d{2}"))
  ) %>%
    arrange(date)
}

read_snapshot <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  stop_if_unexpected(df, cfg$clean_weekly_columns, basename(path))
  df
}

# Manually maintained figures (data/manual/<name>.csv). NULL when the file is absent.
# Counts only. Expected files and columns:
#   cop-activity.csv   Tags or Community, Topics, Posts, Likes  (Communities page, top CoPs table)
#   repositories.csv   group, metric, value                     (Engagement page, repository figures)
read_manual <- function(name) {
  path <- file.path(cfg$dir_manual, paste0(name, ".csv"))
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE)
}

# Date of the latest users export, formatted for page headers
latest_data_date_label <- function() fmt_date(max(list_snapshots()$date))
