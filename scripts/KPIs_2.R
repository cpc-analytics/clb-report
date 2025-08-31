library(tidyverse)
library(lubridate)

data_dir <- "data/."

# ---- Get list of weekly files and extract dates ----
files <- list.files(data_dir, pattern = "week-\\d{2}-\\d{2}-\\d{2}", full.names = TRUE)

file_info <- tibble(
  file = files,
  date = str_extract(files, "\\d{2}-\\d{2}-\\d{2}") %>% 
    as.Date(format = "%y-%m-%d")
) %>% 
  arrange(date)

# ---- Load minimal columns for 90-day Active Member KPI ----
all_data_kpi_90d <- file_info %>% 
  mutate(data = map(file, ~ read_csv(.x) %>% 
                      filter(!(username %in% c("discobot", "system"))) %>% 
                      select(id, last_seen_at))) %>% 
  select(date, data) %>% 
  deframe()

# ---- Load full datasets for Engagement KPI (keep all needed columns) ----
all_data_kpi_engagement <- file_info %>% 
  mutate(data = map(file, ~ read_csv(.x) %>% 
                      filter(!(username %in% c("discobot", "system"))))) %>% 
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
  names(all_data_kpi_90d),
  ~ calc_active_90d(all_data_kpi_90d[[.x]], as.Date(.x))
)

print(kpi_weekly)

# ---- KPI: Engagement Types ----
categorize_time_spent <- function(data, reference_date) {
  data %>%
    mutate(
      time_read = as.numeric(time_read),
      time_read_min = time_read / 60,
      created_at = as.Date(ymd_hms(created_at)),
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
    rename(Category = engagement_group, Users = n) %>%
    mutate(Week = reference_date)
}

# Apply engagement KPI calculation on full datasets with needed columns
engagement_all_weeks <- imap_dfr(all_data_kpi_engagement, ~ {
  categorize_time_spent(.x, as.Date(.y))
})


engagement_wide <- engagement_all_weeks %>%
  pivot_wider(names_from = Week, values_from = Users, values_fill = 0)

print(engagement_all_weeks)
print(engagement_wide)




# ---- KPI: Member Activity Levels ---

# Load all data, exclude system users, keep id, last_seen_at, last_posted_at
all_data_kpi_posts <- file_info %>% 
  mutate(data = map(file, ~ read_csv(.x) %>% 
                      filter(!(username %in% c("discobot", "system"))) %>% 
                      select(id, last_seen_at, last_posted_at))) %>% 
  select(date, data) %>% 
  deframe()

# ---- KPI: Monthly Active Posters ----

calc_monthly_active_posters <- function(df, ref_date) {
  df <- df %>%
    mutate(
      last_posted_at = as.POSIXct(last_posted_at, tz = "UTC"),
      last_posted_date = as.Date(last_posted_at),
      active_last_30d = last_posted_date >= (ref_date - 30)
    )
  
  active_posters <- sum(df$active_last_30d, na.rm = TRUE)
  total_users <- nrow(df)
  prop_active <- ifelse(total_users > 0, active_posters / total_users, NA_real_)
  
  tibble(
    week = ref_date,
    active_posters = active_posters,
    total_users = total_users,
    prop_active_posters = round(prop_active, 3)
  )
}

monthly_posters_kpi <- map_dfr(
  names(all_data_kpi_posts),
  ~ calc_monthly_active_posters(all_data_kpi_posts[[.x]], as.Date(.x))
) %>%
  arrange(week)

print(monthly_posters_kpi)


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