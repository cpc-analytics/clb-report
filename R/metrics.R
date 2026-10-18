# Shared calculation functions. Data in, numbers out, no file access.
# Every page and report calls these so a metric has exactly one definition.
# Settings (windows, thresholds) come from config.R.

library(dplyr)
library(tidyr)
library(tibble)

# ---- Data checks -----------------------------------------------------------

# Names that must never appear in a file the report reads
identifier_pattern <- "^(email|username|user_name|name|full_name|ip|ip_address|registration_ip_address)$"

stop_if_identifiers <- function(df, what = "data") {
  hit <- grep(identifier_pattern, names(df), ignore.case = TRUE, value = TRUE)
  if (length(hit) > 0) {
    stop("Identifier column(s) in ", what, ": ", paste(hit, collapse = ", "),
         ". Run scripts/ingest-data.R and read from data/clean/ only.", call. = FALSE)
  }
  invisible(df)
}

# Stricter check for files with a fixed layout: refuses any column not on the list
stop_if_unexpected <- function(df, allowed, what = "data") {
  extra <- setdiff(names(df), allowed)
  if (length(extra) > 0) {
    stop("Unexpected column(s) in ", what, ": ", paste(extra, collapse = ", "),
         ". Run scripts/ingest-data.R and read from data/clean/ only.", call. = FALSE)
  }
  invisible(df)
}

# ---- Formatting ------------------------------------------------------------

fmt_n <- function(x) {
  formatC(round(x), format = "f", digits = 0, big.mark = ",")
}

# x is already on the 0-100 scale
fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "N/A", paste0(formatC(x, format = "f", digits = digits), "%"))
}

fmt_num <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

# Dates, no leading zero. Default is WHO style: "6 October 2026"; abbreviate = TRUE gives
# "6 Oct 2026" (table headers). text = TRUE gives "October 6, 2026" for running text.
fmt_date <- function(d, abbreviate = FALSE, text = FALSE) {
  d <- as.Date(d)
  day <- as.integer(format(d, "%d"))
  if (text) return(paste0(format(d, "%B"), " ", day, ", ", format(d, "%Y")))
  paste(day, format(d, if (abbreviate) "%b" else "%B"), format(d, "%Y"))
}

# Signed percentage change for tables: "+4.4%", "-0.7%", "N/A"
fmt_change <- function(x, digits = 1) {
  ifelse(is.na(x), "N/A",
         paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = digits), "%"))
}

# Percentage change, 0-100 scale. NA when the baseline is missing or zero.
pct_change <- function(new, old) {
  ifelse(is.na(old) | old == 0, NA_real_, (new - old) / old * 100)
}

# ---- Dates -----------------------------------------------------------------

# Calendar date of a timestamp, in UTC
parse_date_utc <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
  as.Date(lubridate::parse_date_time(x, orders = c("ymd HMS", "ymd HM", "ymd"),
                                     tz = "UTC", quiet = TRUE))
}

# ---- Activity --------------------------------------------------------------

# Share of members seen within `days` before ref_date
active_share <- function(df, ref_date, days = cfg$active_days) {
  total  <- nrow(df)
  active <- sum(df$last_seen_date >= ref_date - days, na.rm = TRUE)
  tibble(total = total, active = active,
         share = if (total > 0) active / total else NA_real_)
}

# Share of members who posted within `days` before ref_date
posting_share <- function(df, ref_date, days = cfg$posting_days) {
  total  <- nrow(df)
  posted <- sum(df$last_posted_date >= ref_date - days, na.rm = TRUE)
  tibble(total = total, posted = posted,
         share = if (total > 0) posted / total else NA_real_)
}

# Members created after prev_date and up to (including) cur_date
new_members <- function(df, prev_date, cur_date) {
  sum(df$created_date > prev_date & df$created_date <= cur_date, na.rm = TRUE)
}

activity_group <- function(df, ref_date) {
  seen <- df$last_seen_date
  case_when(
    seen >= ref_date - cfg$active_days  ~ "Active",
    seen >= ref_date - cfg$dormant_days ~ "Dormant",
    seen <  ref_date - cfg$dormant_days ~ "Uninterested",
    TRUE                                ~ "Unknown"
  )
}

# Reading minutes per month of account age (minimum one month).
# Missing reading time counts as zero, missing creation date as a one-month-old account.
engagement_group <- function(df, ref_date) {
  months_active <- pmax(as.numeric(ref_date - df$created_date) / cfg$days_per_month,
                        1, na.rm = TRUE)
  per_month <- (coalesce(as.numeric(df$time_read), 0) / 60) / months_active
  case_when(
    per_month >= cfg$engagement_high     ~ "High",
    per_month >= cfg$engagement_moderate ~ "Moderate",
    TRUE                                 ~ "Barely"
  )
}

# Counts per level, in level order. An "Unknown" row is added only if present.
count_levels <- function(x, levels) {
  if (any(x == "Unknown", na.rm = TRUE)) levels <- c(levels, "Unknown")
  tibble(level = levels, n = as.integer(table(factor(x, levels = levels))))
}

# ---- Retention -------------------------------------------------------------

# Share of members (ids) with no login gap above retention_gap_days across the
# retention window ending at ref_date. `snapshots` is a named list (names are
# snapshot dates) of data frames with id and last_seen_date.
# Returns NA when the snapshots do not cover enough history.
calc_retention <- function(snapshots, ref_date, ids) {
  ids <- unique(ids)
  empty <- tibble(total = length(ids), retained = NA_integer_, share = NA_real_)

  snap_dates <- as.Date(names(snapshots))
  in_window  <- snap_dates >= ref_date - cfg$retention_window_days & snap_dates <= ref_date
  if (!any(in_window)) return(empty)
  if (as.integer(ref_date - min(snap_dates[in_window])) < cfg$retention_min_history_days) {
    return(empty)
  }

  history <- bind_rows(lapply(snapshots[in_window], function(s) s[, c("id", "last_seen_date")])) %>%
    filter(id %in% ids, !is.na(last_seen_date)) %>%
    distinct(id, last_seen_date)

  # ref_date closes the last gap. Members with no login on record are not retained.
  gaps <- bind_rows(history, tibble(id = unique(history$id), last_seen_date = ref_date)) %>%
    distinct(id, last_seen_date) %>%
    arrange(id, last_seen_date) %>%
    group_by(id) %>%
    summarise(max_gap = if (n() > 1) max(as.numeric(diff(last_seen_date))) else 0,
              .groups = "drop")

  retained <- sum(gaps$max_gap <= cfg$retention_gap_days)
  tibble(total = length(ids), retained = retained, share = retained / length(ids))
}

# ---- Communities -----------------------------------------------------------

# Number of Communities of Practice each member belongs to (0 included).
# Only direct tags listed in cop_tags count.
cop_membership_counts <- function(df, cop_tags) {
  per_member <- df %>%
    select(id, group_names) %>%
    separate_rows(group_names, sep = ";") %>%
    filter(group_names %in% cop_tags) %>%
    distinct(id, group_names) %>%
    count(id, name = "n_cops")

  df %>%
    distinct(id) %>%
    left_join(per_member, by = "id") %>%
    mutate(n_cops = coalesce(n_cops, 0L))
}

# ---- Institutions and geography --------------------------------------------

# "N+" rounded down to a multiple of institutions_round_down_to; exact count below that
format_institutions <- function(n) {
  step <- cfg$institutions_round_down_to
  if (n < step) as.character(n) else paste0(floor(n / step) * step, "+")
}

institution_summary <- function(df) {
  with_domain <- df %>% filter(!is.na(email_domain))
  who <- sum(with_domain$is_who, na.rm = TRUE)
  tibble(
    unique_domains  = n_distinct(with_domain$email_domain),
    who_members     = who,
    non_who_members = nrow(with_domain) - who,
    who_share       = if (nrow(with_domain) > 0) who / nrow(with_domain) else NA_real_
  )
}

# Country counts and WHO region summary from the country-per-member file
# (column `country`) joined to the region lookup (COUNTRY, ISO3, who_region).
summarise_geo <- function(ip, country_codes) {
  ip <- ip %>%
    rename(COUNTRY = country) %>%
    left_join(country_codes, by = "COUNTRY") %>%
    rename(iso_3_code = ISO3)

  with_region <- ip %>% filter(!is.na(COUNTRY), !is.na(who_region))

  countries_per_region <- with_region %>%
    distinct(COUNTRY, who_region) %>%
    count(who_region, name = "n_countries")
  members_per_region <- with_region %>%
    count(who_region, name = "n_members")

  list(
    ip             = ip,
    n_countries    = n_distinct(with_region$COUNTRY),
    n_regions      = n_distinct(with_region$who_region),
    region_summary = full_join(countries_per_region, members_per_region, by = "who_region") %>%
      arrange(desc(n_members)),
    user_counts    = ip %>%
      filter(!is.na(iso_3_code)) %>%
      group_by(iso_3_code, COUNTRY) %>%
      summarise(user_count = n(), .groups = "drop")
  )
}

# ---- Month-over-month tables -----------------------------------------------

# Two-snapshot comparison of level counts with change column and total row.
# c1 and c2 come from count_levels(); labels maps level -> display name.
compare_levels <- function(c1, c2, labels, date1, date2) {
  out <- full_join(c1, c2, by = "level", suffix = c("_1", "_2")) %>%
    mutate(across(c(n_1, n_2), ~ coalesce(.x, 0L)))
  out <- bind_rows(out, tibble(level = "Total", n_1 = sum(out$n_1), n_2 = sum(out$n_2)))
  out %>%
    mutate(
      Category = ifelse(level %in% names(labels), unname(labels[level]), level),
      Change   = fmt_change(pct_change(n_1, n_2))
    ) %>%
    select(Category, n_1, n_2, Change) %>%
    setNames(c("Category", fmt_date(date1, TRUE), fmt_date(date2, TRUE), "Monthly Change"))
}

# Average activity per member for two snapshots, with change.
compare_averages <- function(d1, d2, date1, date2) {
  avg <- function(d) {
    c(`Time spent (mins)` = mean(d$time_read, na.rm = TRUE) / 60,
      `Topics entered`    = mean(d$topics_entered, na.rm = TRUE),
      `Posts read`        = mean(d$posts_read_count, na.rm = TRUE),
      `Likes given`       = mean(d$likes_given, na.rm = TRUE),
      `Topics created`    = mean(d$topic_count, na.rm = TRUE),
      `Posts created`     = mean(d$post_count, na.rm = TRUE),
      `Likes received`    = mean(d$likes_received, na.rm = TRUE))
  }
  a1 <- avg(d1)
  a2 <- avg(d2)
  tibble(
    `Metric (AVG)` = names(a1),
    v1             = fmt_num(a1),
    v2             = fmt_num(a2),
    Change         = fmt_change(pct_change(a1, a2))
  ) %>%
    setNames(c("Metric (AVG)", fmt_date(date1, TRUE), fmt_date(date2, TRUE), "Monthly Change"))
}

# The three tables of the Engagement section for one population.
build_monthly_tables <- function(d1, d2, date1, date2) {
  list(
    metrics = compare_averages(d1, d2, date1, date2),
    activity = compare_levels(
      count_levels(activity_group(d1, date1), activity_levels),
      count_levels(activity_group(d2, date2), activity_levels),
      labels = c(), date1, date2),
    engagement = compare_levels(
      count_levels(engagement_group(d1, date1), engagement_levels),
      count_levels(engagement_group(d2, date2), engagement_levels),
      labels = engagement_labels, date1, date2)
  )
}

# Card color class: green when the value meets the target
kpi_class <- function(value, target) {
  if (!is.na(value) && value >= target) "stats-number-green" else "stats-number-white"
}
