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

# Calculate 12-month consistent engagement for each week
kpi_12month_weekly <- map_dfr(names(all_data_kpi), function(week_name) {
  week_date <- as.Date(week_name)
  current_week_users <- all_data_kpi[[week_name]]  # Users in THIS week's dataset
  
  # Get 12-month window
  window_start <- week_date - 365
  
  # Find all weeks within the 12-month window
  relevant_weeks <- names(all_data_kpi)[as.Date(names(all_data_kpi)) >= window_start & 
                                          as.Date(names(all_data_kpi)) <= week_date]
  
  # Get historical activity for all users across the 12-month window
  all_activity_in_window <- map_dfr(relevant_weeks, function(w) {
    all_data_kpi[[w]] %>%
      select(id, last_seen_at) %>%
      mutate(
        week = as.Date(w),
        last_seen_date = as.Date(as.POSIXct(last_seen_at, tz = "UTC"))
      )
  })
  
  # For each user in the CURRENT week, check their 12-month consistency
  current_week_analysis <- current_week_users %>%
    select(id) %>%
    left_join(
      all_activity_in_window %>%
        group_by(id) %>%
        summarize(
          login_dates = list(sort(unique(last_seen_date))),
          .groups = "drop"
        ),
      by = "id"
    ) %>%
    mutate(
      # If no historical data, create empty list
      login_dates = map(login_dates, ~ if(is.null(.x)) character(0) else .x),
      
      consistent_active = map_lgl(login_dates, function(dates) {
        if (length(dates) == 0) return(FALSE)
        
        # Add the reference week date as the "end point"
        all_dates <- c(dates, week_date)
        all_dates <- sort(unique(all_dates))
        
        # Check if any gap between consecutive logins exceeds 90 days
        gaps <- diff(all_dates)
        max_gap <- max(gaps, na.rm = TRUE)
        
        return(max_gap <= 90)
      })
    )
  
  # Calculate summary for this week
  summary_week <- current_week_analysis %>%
    summarize(
      week = week_date,
      consistent_active_members = sum(consistent_active, na.rm = TRUE),
      total_members = n(),  # This is now the count from current week only
      retention_rate_12month = round(consistent_active_members / total_members, 3),
      kpi_met_12month = retention_rate_12month >= 0.30,
      .groups = "drop"
    )
  
  return(summary_week)
})

# Display only the latest week's results
latest_week_12month <- kpi_12month_weekly %>%
  filter(week == max(week))

print(latest_week_12month)

long_term_retained_members <- latest_week_12month$consistent_active_members
long_term_total_members <- latest_week_12month$total_members
percentage_retained <- paste0(round(latest_week_12month$retention_rate_12month * 100, 1), "%")






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

all_unique_domains <- email_domains %>%
  distinct(domain) %>%
  arrange(domain) %>%
  pull(domain)

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
