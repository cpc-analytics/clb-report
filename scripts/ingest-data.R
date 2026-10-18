# Ingest step: turns raw forum exports into the minimised files the report reads.
#
# Run after every manual export, before rendering anything:
#   Rscript scripts/ingest-data.R               # new files only
#   Rscript scripts/ingest-data.R --force       # rebuild every clean file
#   Rscript scripts/ingest-data.R --delete-raw  # also delete raw files once ingested
#
# Raw exports (data/raw/, or CLB_RAW_DIR) are read here and nowhere else. Only an explicit
# allowlist of columns is carried over. E-mail and username serve the member definition
# and the e-mail domain, then are dropped. Timestamps are reduced to calendar dates.

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
library(readr)
library(stringr)

# ---- Column allowlists -----------------------------------------------------

weekly_required <- c(
  "id", "username", "email", "group_names",
  "created_at", "last_seen_at", "last_posted_at",
  "time_read", "topics_entered", "posts_read_count",
  "likes_given", "topic_count", "post_count", "likes_received"
)

# ---- Transformations (pure, no file access) --------------------------------

# E-mail domain, lower case, without common mail-server prefixes
email_domain <- function(email) {
  d <- tolower(str_extract(as.character(email), "(?<=@)[^>@\\s]+"))
  d <- str_remove_all(d, "^mail\\.|^smtp\\.|^webmail\\.")
  ifelse(is.na(d) | d == "", NA_character_, d)
}

is_who_domain <- function(domain) {
  !is.na(domain) & (domain == cfg$who_domain | endsWith(domain, paste0(".", cfg$who_domain)))
}

# Member definition and column reduction for one weekly user export
minimize_members <- function(raw) {
  missing <- setdiff(weekly_required, names(raw))
  if (length(missing) > 0) {
    stop("Export is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  members <- raw %>%
    filter(!(username %in% cfg$system_usernames),
           !(email %in% cfg$placeholder_emails))

  domain <- email_domain(members$email)

  clean <- tibble(
    id               = as.integer(members$id),
    created_date     = parse_date_utc(members$created_at),
    last_seen_date   = parse_date_utc(members$last_seen_at),
    last_posted_date = parse_date_utc(members$last_posted_at),
    time_read        = as.numeric(members$time_read),
    topics_entered   = as.numeric(members$topics_entered),
    posts_read_count = as.numeric(members$posts_read_count),
    likes_given      = as.numeric(members$likes_given),
    topic_count      = as.numeric(members$topic_count),
    post_count       = as.numeric(members$post_count),
    likes_received   = as.numeric(members$likes_received),
    group_names      = as.character(members$group_names),
    email_domain     = domain,
    is_who           = is_who_domain(domain)
  )

  # A date column that is entirely missing means the timestamp format was not understood
  for (col in c("created_date", "last_seen_date")) {
    if (nrow(clean) > 0 && all(is.na(clean[[col]]))) {
      stop("Could not read dates in column '", col, "'. Check the export format.", call. = FALSE)
    }
  }
  stop_if_identifiers(clean, "minimised output")
  clean
}

# Survey export: keep only the configured answer columns (by position)
minimize_survey <- function(raw, rnd) {
  keep <- rnd$columns
  if (!is.na(rnd$language)) keep <- c(Language = rnd$language, keep)
  if (max(keep) > ncol(raw)) {
    stop("Survey '", rnd$id, "' has ", ncol(raw), " columns, expected at least ", max(keep),
         ". Update survey_rounds in config.R.", call. = FALSE)
  }
  out <- raw[-1, unname(keep)]          # row 1 is the header
  names(out) <- names(keep)
  check_survey_answers(out, rnd)
  out
}

# Guards against a shifted column layout: answer columns must hold recognised answers.
# Languages other than English may appear, so a few unrecognised values are tolerated.
survey_answer_sets <- list(
  Satisfaction = c("Very dissatisfied", "Somewhat dissatisfied", "Neutral",
                   "Somewhat satisfied", "Very satisfied"),
  Recommended  = c("Very likely", "Somewhat likely", "Neutral", "Somewhat unlikely",
                   "Somewhat Unlikely", "Very unlikely"),
  value        = c("Yes", "Somewhat", "No", "I didn't know about this")
)

check_survey_answers <- function(out, rnd) {
  for (col in setdiff(names(out), "Language")) {
    set <- if (col %in% names(survey_answer_sets)) survey_answer_sets[[col]] else survey_answer_sets$value
    vals <- trimws(out[[col]])
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals) > 0 && mean(vals %in% set) < 0.8) {
      stop("Survey '", rnd$id, "': column '", col, "' does not look like answers to that question. ",
           "The export layout may have changed. Check survey_rounds in config.R.", call. = FALSE)
    }
  }
  invisible(out)
}

# Country-per-member file: country only
minimize_geo <- function(raw) {
  if (!"country" %in% names(raw)) stop("Geo file has no 'country' column.", call. = FALSE)
  raw %>% select(country)
}

# ---- File handling ---------------------------------------------------------

write_clean <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, path)
}

# A raw file is (re)ingested when there is no clean file yet, or the raw file is newer
needs_ingest <- function(raw, clean, force = FALSE) {
  force || !file.exists(clean) || file.mtime(raw) > file.mtime(clean)
}

# Applies `fn` to each raw file, writing the result under dir_clean.
# Returns the raw files whose clean version is up to date.
ingest_files <- function(files, out_path, read_fn, fn, force, label) {
  done <- character(0)
  for (f in files) {
    out <- out_path(f)
    if (!needs_ingest(f, out, force)) {
      message(label, ": ", basename(f), " already ingested, skipped")
      done <- c(done, f)
      next
    }
    raw   <- read_fn(f)
    clean <- fn(raw, f)
    write_clean(clean, out)
    message(label, ": ", basename(f), " -> ", nrow(clean), " rows kept of ", nrow(raw),
            " (", ncol(clean), " columns)")
    done <- c(done, f)
  }
  done
}

ingest_all <- function(force = FALSE, delete_raw = FALSE) {
  if (!dir.exists(cfg$dir_raw)) stop("Raw folder not found: ", cfg$dir_raw, call. = FALSE)
  ingested <- character(0)

  weekly <- list.files(cfg$dir_raw, pattern = "^users-\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
  ingested <- c(ingested, ingest_files(
    weekly,
    out_path = function(f) file.path(cfg$dir_clean, basename(f)),
    read_fn  = function(f) read_csv(f, col_select = any_of(weekly_required), show_col_types = FALSE),
    fn       = function(raw, f) minimize_members(raw),
    force = force, label = "weekly"))

  geo <- list.files(cfg$dir_raw, pattern = "^output_with_country(_q[1-4])?\\.csv$", full.names = TRUE)
  ingested <- c(ingested, ingest_files(
    geo,
    out_path = function(f) file.path(cfg$dir_clean, sub("^output_with_country", "geo-country", basename(f))),
    read_fn  = function(f) read_csv(f, show_col_types = FALSE),
    fn       = function(raw, f) minimize_geo(raw),
    force = force, label = "geo"))

  for (rnd in cfg$survey_rounds) {
    f <- file.path(cfg$dir_raw, "survey", rnd$raw)
    if (!file.exists(f)) {
      message("survey: ", rnd$raw, " not found, skipped")
      next
    }
    ingested <- c(ingested, ingest_files(
      f,
      out_path = function(f) file.path(cfg$dir_clean, "survey", paste0(rnd$id, ".csv")),
      read_fn  = function(f) read_csv(f, col_names = FALSE, col_types = cols(.default = "c"),
                                      locale = locale(encoding = "UTF-8"), show_col_types = FALSE),
      fn       = function(raw, f) minimize_survey(raw, rnd),
      force = force, label = "survey"))
  }

  if (delete_raw && length(ingested) > 0) {
    file.remove(ingested)
    message("Deleted ", length(ingested), " raw file(s).")
  }
  invisible(ingested)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  ingest_all(force = "--force" %in% args, delete_raw = "--delete-raw" %in% args)
}
