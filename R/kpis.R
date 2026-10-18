# KPI series across all weekly snapshots. Requires load-data.R.
# Defines:
#   kpi_weekly             active members per snapshot
#   monthly_posters_kpi    posting activity per snapshot
#   engagement_weekly      engagement levels per snapshot (counts and shares)
#   retention_latest       long-term retention at the latest snapshot
#   institutions           distinct e-mail domains and WHO share at the latest snapshot
#   latest_kpi             latest value of each KPI, in percent

if (!exists("snapshots_index")) source(here::here("R", "load-data.R"))

all_snapshots <- snapshots_index$file %>%
  setNames(as.character(snapshots_index$date)) %>%
  lapply(read_snapshot)

# ---- Active members --------------------------------------------------------
kpi_weekly <- bind_rows(lapply(names(all_snapshots), function(d) {
  s <- active_share(all_snapshots[[d]], as.Date(d))
  tibble(week = as.Date(d), active_count = s$active, total_user_count = s$total,
         prop_active = s$share)
})) %>%
  mutate(kpi_met = prop_active >= cfg$target_active)

# ---- Posting activity ------------------------------------------------------
monthly_posters_kpi <- bind_rows(lapply(names(all_snapshots), function(d) {
  s <- posting_share(all_snapshots[[d]], as.Date(d))
  tibble(week = as.Date(d), active_posters = s$posted, total_users = s$total,
         prop_active_posters = s$share)
}))

# ---- Engagement levels -----------------------------------------------------
engagement_weekly <- bind_rows(lapply(names(all_snapshots), function(d) {
  count_levels(engagement_group(all_snapshots[[d]], as.Date(d)), engagement_levels) %>%
    mutate(week = as.Date(d), share = n / sum(n)) %>%
    rename(users = n)
}))

# ---- Long-term retention (latest snapshot) ---------------------------------
retention_latest <- calc_retention(all_snapshots, snapshot_date, data1$id)

# ---- Institutions (latest snapshot) ----------------------------------------
institutions        <- institution_summary(data1)
unique_domain_count <- institutions$unique_domains
institutions_label  <- format_institutions(unique_domain_count)

# Latest values, as percentages
latest_kpi <- list(
  active    = kpi_weekly %>% slice_max(week, n = 1) %>% pull(prop_active) * 100,
  posting   = monthly_posters_kpi %>% slice_max(week, n = 1) %>% pull(prop_active_posters) * 100,
  retention = retention_latest$share * 100,
  engagement_mod_high = engagement_weekly %>%
    filter(week == max(week), level %in% c("High", "Moderate")) %>%
    summarise(s = sum(share)) %>% pull(s) * 100
)
