# Load data

# Directory
data_dir <- "data/."

# List all files matching the naming pattern
files <- list.files(data_dir, pattern = "week-\\d{2}-\\d{2}-\\d{2}", full.names = TRUE)

# Extract dates from filenames
file_info <- tibble(
  file = files,
  date = str_extract(file, "\\d{2}-\\d{2}-\\d{2}") %>% 
    as.Date(format = "%y-%m-%d")
)

# Find the most recent file
latest_file <- file_info %>% 
  arrange(desc(date)) %>%
  slice(1)

# Target date: approximately one month before the latest
target_date <- latest_file$date %m-% months(1)

# Find the file closest to one month before (but not after)
one_month_prior_file <- file_info %>%
  filter(date <= target_date) %>%
  arrange(desc(date)) %>%
  slice(1)

# Calculate the gap in days
gap_days <- as.integer(latest_file$date - one_month_prior_file$date)

# Warn if the gap is too small or too large
if (gap_days < 21) {
  warning(paste("⚠️ Date gap is quite small (", gap_days, " days). Is this a mistake?", sep = ""))
} else if (gap_days > 40) {
  warning(paste("⚠️ Date gap is quite large (", gap_days, " days). Might not be a good monthly comparison.", sep = ""))
}

# Load the data
data1 <- read_csv(latest_file$file)          # current week
data2 <- read_csv(one_month_prior_file$file) # one month prior

# Check if both datasets have the same variables
columns_match <- all(names(data1) == names(data2))

if (columns_match) {
  cat("")
} else {
  cat("The datasets have different variables.\n")
  
  # Print the difference if there are mismatches
  missing_in_data1 <- setdiff(names(data2), names(data1))
  missing_in_data2 <- setdiff(names(data1), names(data2))
  
  if (length(missing_in_data1) > 0) {
    cat("Variables in dataset 2 but not in dataset 1:\n", missing_in_data1, "\n")
  }
  if (length(missing_in_data2) > 0) {
    cat("Variables in dataset 1 but not in dataset 2:\n", missing_in_data2, "\n")
  }
}
