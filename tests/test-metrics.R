# Checks for the shared metric functions, on small made-up data. No data files are read.
# Run from the project root: Rscript tests/test-metrics.R
suppressMessages({library(dplyr); library(tidyr); library(tibble)})
source(here::here("R", "config.R")); source(here::here("R", "metrics.R"))
ref <- as.Date("2026-06-30")

# fmt
stopifnot(fmt_n(100000) == "100,000", fmt_n(1234.6) == "1,235", fmt_pct(12.345) == "12.3%", fmt_pct(NA) == "N/A")
stopifnot(is.na(pct_change(5, 0)), abs(pct_change(110, 100) - 10) < 1e-9)

# dates
stopifnot(parse_date_utc("2026-01-02 23:59:59") == as.Date("2026-01-02"))
stopifnot(parse_date_utc(as.POSIXct("2026-01-02 23:59:59", tz = "UTC")) == as.Date("2026-01-02"))

# activity
df <- tibble(id = 1:5,
  last_seen_date = ref - c(0, 90, 91, 180, 181),
  last_posted_date = ref - c(0, 30, 31, NA, 400),
  created_date = ref - c(10, 100, 400, 800, 20),
  time_read = c(0, 60*15*4, 60*2*14, NA, 60*100),
  topics_entered = 1, posts_read_count = 1, likes_given = 1, topic_count = 0, post_count = 0, likes_received = 0)
a <- active_share(df, ref); stopifnot(a$active == 2, a$total == 5)
p <- posting_share(df, ref); stopifnot(p$posted == 2)
stopifnot(identical(activity_group(df, ref), c("Active","Active","Dormant","Dormant","Uninterested")))
stopifnot(new_members(df, ref - 30, ref) == 2)  # created 10 and 20 days ago

# count_levels / compare_levels
cl <- count_levels(c("Active","Active","Dormant"), activity_levels)
stopifnot(cl$n == c(2L,1L,0L))
tabs <- build_monthly_tables(df, df[1:4,], ref, ref - 30)

# retention
mk <- function(d, seen) tibble(id = 1:3, last_seen_date = as.Date(seen))
snaps <- list()
dates <- seq(as.Date("2025-06-01"), as.Date("2026-06-01"), by = "week")
for (i in seq_along(dates)) {
  d <- dates[i]
  # id1 seen every week, id2 seen only first week then never, id3 seen weekly except one 100-day hole
  seen <- c(d, dates[1], if (d > dates[1] + 150 || d < dates[1] + 40) d else dates[1] + 40)
  snaps[[as.character(d)]] <- mk(d, seen)
}
r <- calc_retention(snaps, max(dates), 1:3)
stopifnot(r$retained == 1)   # only id1; id3 gap ~110d > 90
short <- snaps[as.character(tail(dates, 10))]
stopifnot(is.na(calc_retention(short, max(dates), 1:3)$share))

# cop counts
u <- tibble(id = c(1,2,3,4), group_names = c("a;b;trust_level_1", "a", NA, "c;a;a"))
cc <- cop_membership_counts(u, c("a","b"))
stopifnot(identical(cc$n_cops, c(2L,1L,0L,1L)))

# institutions
inst <- institution_summary(tibble(email_domain = c("who.int","x.org",NA,"x.org"), is_who = c(TRUE,FALSE,FALSE,FALSE)))
stopifnot(inst$unique_domains == 2, inst$who_members == 1, inst$non_who_members == 2)

# geo
ip <- tibble(country = c("A","A","B","C",NA))
cc <- tibble(COUNTRY = c("A","B","C"), ISO3 = c("AAA","BBB","CCC"), who_region = c("R1","R1",NA))
g <- summarise_geo(ip, cc)
stopifnot(g$n_countries == 2, g$n_regions == 1, g$region_summary$n_members == 3, nrow(g$user_counts) == 3)
cat("ALL OK\n")
