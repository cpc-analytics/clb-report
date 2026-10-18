# Monthly engagement tables for the Engagement section. Requires load-data.R.
# Each population gets three tables (average activity, activity groups, engagement
# levels) comparing the latest snapshot with the previous one.

without_secretariat <- function(df) {
  df %>% filter(!grepl(cfg$secretariat_group, group_names, fixed = TRUE))
}

tables_all    <- build_monthly_tables(data1, data2, snapshot_date, previous_snapshot_date)
tables_no_sec <- build_monthly_tables(without_secretariat(data1), without_secretariat(data2),
                                      snapshot_date, previous_snapshot_date)
