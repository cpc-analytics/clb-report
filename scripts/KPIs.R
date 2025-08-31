# ---- Load ALL weekly datasets separately for KPI trends with filtering ----

library(tidyverse)
library(lubridate)

data_dir <- "data/."
files <- list.files(data_dir, pattern = "week-\\d{2}-\\d{2}-\\d{2}", full.names = TRUE)

file_info <- tibble(
  file = files,
  date = str_extract(files, "\\d{2}-\\d{2}-\\d{2}") %>% 
    as.Date(format = "%y-%m-%d")
) %>% 
  arrange(date)

# Load and filter all datasets, exclude unwanted usernames
all_data_kpi <- file_info %>% 
  mutate(
    data = map(file, ~ read_csv(.x) %>% 
                 filter(!(username %in% c("discobot", "system"))))
  ) %>% 
  select(date, data) %>% 
  deframe()


# ---- KPI: 90-day Active Member Proportion ----

calc_active_90d <- function(df, ref_date) {
  df <- df %>%
    mutate(
      last_seen_at = as.POSIXct(last_seen_at, tz = "UTC"),
      last_seen_date = as.Date(last_seen_at),
      active_90d = last_seen_date >= (ref_date - 90)
    )
  
  active_count <- sum(df$active_90d, na.rm = TRUE)
  total_count <- nrow(df)
  prop_active <- ifelse(total_count > 0, active_count / total_count, NA_real_)
  kpi_met <- prop_active > 0.30
  
  tibble(
    week = ref_date,
    active_90d_count = active_count,
    total_user_count = total_count,
    prop_active_90d = round(prop_active, 3),
    kpi_met = kpi_met
  )
}

kpi_weekly <- map_dfr(
  names(all_data_kpi),
  ~ calc_active_90d(all_data_kpi[[.x]], as.Date(.x))
)

print(kpi_weekly)


# ---- KPI: 12-Month Active Member Proportion ----

# Re-list files and file_info (if needed, can reuse above variables)
files <- list.files(data_dir, pattern = "week-\\d{2}-\\d{2}-\\d{2}", full.names = TRUE)

file_info <- tibble(
  file = files,
  date = str_extract(files, "\\d{2}-\\d{2}-\\d{2}") %>% 
    as.Date(format = "%y-%m-%d")
) %>% 
  arrange(date)

# Load all data, exclude system users, keep only id and last_seen_at
all_data_kpi <- file_info %>% 
  mutate(
    data = map(file, ~ read_csv(.x) %>% 
                 filter(!(username %in% c("discobot", "system"))) %>% 
                 select(id, last_seen_at))
  ) %>% 
  select(date, data) %>% 
  deframe()

# Combine all weeks data into one dataframe with 'week' column indicating the week date
combined_df <- imap_dfr(all_data_kpi, ~ {
  .x %>% mutate(week = as.Date(.y))
})

# Convert last_seen_at to Date for clarity
combined_df <- combined_df %>%
  mutate(last_seen_date = as.Date(as.POSIXct(last_seen_at, tz = "UTC"))) %>%
  select(id, week, last_seen_date)

# Pivot data wider: each user is a row, each week a column, value = last_seen_date for that user-week
wide_logins <- combined_df %>%
  group_by(id, week) %>%
  summarize(last_seen_date = max(last_seen_date, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = week, values_from = last_seen_date)

print(wide_logins)

# Get the week column names (dates)
week_cols <- names(wide_logins)[-1]  # exclude 'id'
week_dates <- as.Date(week_cols)

# Create *_days_since columns for each week
for (i in seq_along(week_cols)) {
  week_col <- week_cols[i]
  date_value <- week_dates[i]
  
  new_col_name <- paste0(week_col, "_days_since")
  
  wide_logins[[new_col_name]] <- as.numeric(date_value - wide_logins[[week_col]])
}

# Identify all *_days_since columns
days_since_cols <- names(wide_logins)[str_detect(names(wide_logins), "_days_since$")]

# Classify long-term active members: active at least once every 90 days across all weeks
wide_logins <- wide_logins %>%
  rowwise() %>%
  mutate(long_term_active = all(c_across(all_of(days_since_cols)) <= 90, na.rm = TRUE)) %>%
  ungroup()

# Calculate long-term member retention summary
long_term_retention_summary <- wide_logins %>%
  summarize(
    active_members = sum(long_term_active, na.rm = TRUE),
    total_members = sum(!is.na(long_term_active)),
    retention_rate = round(active_members / total_members, 3)
  )

# Check if retention target (30%) is met
retention_threshold <- 0.30
retention_goal_met <- long_term_retention_summary$retention_rate >= retention_threshold


# ---- Extract Email Domains and Count Unique Institutes ----

# Assuming 'data1' contains the relevant user data with an 'email' column
email_domains <- data1 %>%
  filter(!is.na(email)) %>%
  mutate(
    domain = tolower(str_extract(email, "(?<=@)[^>]+")),
    domain = str_remove_all(domain, "^mail\\.|^smtp\\.|^webmail\\.")  # remove common subdomain noise
  ) %>%
  filter(!str_detect(domain, "@"), domain != "")  # extra safety

# Count unique domains
unique_domain_count <- n_distinct(email_domains$domain)

# Top 5 most common domains
top_domains <- email_domains %>%
  count(domain, sort = TRUE) %>%
  slice_head(n = 5)


# ---- Interconnectedness of Members ----

# Prepare 'data1_groups': parse and clean group memberships
data1_groups <- data1 %>%
  select(id, group_names) %>%
  mutate(
    groups = str_split(group_names, ";")
  ) %>%
  mutate(
    # Remove groups starting with "trust_level_"
    groups = map(groups, ~ .x[!str_starts(.x, "trust_level_")]),
    group_count = map_int(groups, length),
    multi_group_member = group_count > 1
  )

# Summarize multi-group membership
multi_group_summary <- data1_groups %>%
  summarize(
    total_members = n(),
    multi_group_members = sum(multi_group_member, na.rm = TRUE),
    percent_multi_group = round(multi_group_members / total_members, 3)
  )

# Count number of unique users per group (excluding "trust_level_" groups)
node_sizes <- data1 %>%
  select(id, group_names) %>%
  mutate(groups = str_split(group_names, ";")) %>%
  unnest(groups) %>%
  filter(!str_starts(groups, "trust_level_")) %>%
  distinct(id, groups) %>%
  count(groups, name = "member_count")
