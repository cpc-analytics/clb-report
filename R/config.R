# Central settings for the report. Every threshold, window and path used by more
# than one script lives here so that definitions cannot drift between pages.

cfg <- list(
  # Folders. Raw exports stay in data/raw/; everything else reads data/clean/ (written by ingest-data.R).
  # Set CLB_RAW_DIR to keep raw exports outside the repository folder.
  dir_raw    = Sys.getenv("CLB_RAW_DIR", unset = here::here("data", "raw")),
  dir_clean  = here::here("data", "clean"),
  dir_manual = here::here("data", "manual"),
  dir_reference   = here::here("data", "reference"),    # community tags, country codes
  dir_forum_stats = here::here("data", "forum-stats"),  # daily forum totals

  # Columns a cleaned weekly file may contain. Anything else is refused on read.
  clean_weekly_columns = c(
    "id", "created_date", "last_seen_date", "last_posted_date", "time_read",
    "topics_entered", "posts_read_count", "likes_given", "topic_count", "post_count",
    "likes_received", "group_names", "email_domain", "is_who"
  ),

  # Member definition, applied once at ingest: system accounts and placeholder
  # e-mail rows are not members.
  system_usernames   = c("discobot", "system"),
  placeholder_emails = c("discobot_email", "no_email"),

  # Group tag that marks Secretariat staff (used for the "without Secretariat" view)
  secretariat_group = "CollabSecretariat",
  who_domain        = "who.int",

  # Activity windows, in days before the snapshot date
  active_days  = 90,    # Active: seen within this window
  dormant_days = 180,   # Dormant: seen within this window but not active
  posting_days = 30,    # Posting activity: posted within this window

  # Long-term retention: no login gap above retention_gap_days across the
  # retention_window_days before the snapshot. At least retention_min_history_days
  # of snapshot history are needed, otherwise the value is not reported.
  retention_gap_days         = 90,
  retention_window_days      = 365,
  retention_min_history_days = 351,

  # Engagement: reading minutes per month of account age
  days_per_month      = 30,
  engagement_high     = 15,
  engagement_moderate = 2,

  # KPI targets (share of members)
  target_active    = 0.30,
  target_retention = 0.30,

  # Random seed for the community network layout, so the map does not change between renders
  layout_seed = 1,

  # Length of the trend charts, in days
  trend_window_days = 84,

  # Snapshot comparison: warn when the previous snapshot is this far from one month back
  snapshot_gap_min_days = 21,
  snapshot_gap_max_days = 40,

  # Institutions are shown as "N+", rounded down to a multiple of this
  institutions_round_down_to = 50,

  # Survey: trend arrows show "no change" below these absolute differences
  trend_threshold_points = 3,
  trend_threshold_count  = 2,

  # Community Pulse Survey rounds. Column numbers refer to the raw export
  # (header in row 1). Only these columns are kept at ingest; free-text and
  # identifying columns are dropped. language = NA means the export has no
  # language column, languages_note is then used as is.
  survey_rounds = list(
    list(id = "2025-06", label = "June 2025",
         raw = "pulse-survey-2025-06.csv",
         columns = c(Satisfaction = 8, Recommended = 9, Network = 10, Analysis = 11,
                     Events = 12, Access = 13, Knowledge = 14, Interaction = 15),
         language = NA_integer_,
         languages_note = "English (41), French (5), and Spanish (3)"),
    list(id = "2025-12", label = "December 2025",
         raw = "pulse-survey-2025-12.csv",
         columns = c(Satisfaction = 7, Recommended = 8, Network = 9, Analysis = 10,
                     Events = 11, Access = 12, Knowledge = 13, Interaction = 14),
         language = 6L,
         languages_note = NA_character_)
  )
)

# Display labels built from the thresholds above, so text and tables never
# disagree with the calculation.
activity_levels <- c("Active", "Dormant", "Uninterested")
activity_labels <- c(
  Active       = sprintf("Active (seen in last %d days)", cfg$active_days),
  Dormant      = sprintf("Dormant (%d-%d days)", cfg$active_days + 1, cfg$dormant_days),
  Uninterested = sprintf("Uninterested (over %d days)", cfg$dormant_days)
)

engagement_levels <- c("High", "Moderate", "Barely")
engagement_labels <- c(
  High     = sprintf("Highly engaged (%d+ min/month)", cfg$engagement_high),
  Moderate = sprintf("Moderately engaged (%d-%d min/month)", cfg$engagement_moderate, cfg$engagement_high),
  Barely   = sprintf("Barely engaged (under %d min/month)", cfg$engagement_moderate)
)
