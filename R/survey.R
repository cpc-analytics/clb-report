# Community Pulse Survey results. Requires config.R (rounds) and metrics.R.
# Reads the minimised survey files from data/clean/survey/ (answer columns only,
# see ingest-data.R). Defines survey_results, a list with one entry per round.

library(readr)
library(dplyr)
library(tidyr)

# Yes, Somewhat, No, I didn't know about this
custom_palette <- c(collab$brand_stronger, collab$brand, collab$cat1, collab$fg_weakest)

satisfaction_levels <- c("Very dissatisfied", "Somewhat dissatisfied",
                         "Neutral", "Somewhat satisfied", "Very satisfied")

question_labels <- c(
  "Network"      = "Networking, connecting with others",
  "Analysis"     = "Opportunities to Collaborate and/or conduct joint analysis",
  "Events"       = "Attending events (webinars, meetings, workshops etc)",
  "Access"       = "Access to tools, code, packages, and/or data",
  "Knowledge"    = "Knowledge sharing",
  "Interaction"  = "Interaction with decision makers"
)

# ---- Helper functions ------------------------------------------------------

# Promoter / passive / detractor shares. Neutral answers count as detractors.
compute_nps <- function(data, rec_col = "Recommended") {
  data %>%
    mutate(
      nps_group = case_when(
        .data[[rec_col]] == "Very likely"     ~ "Promoter",
        .data[[rec_col]] == "Somewhat likely" ~ "Passive",
        .data[[rec_col]] %in% c("Neutral", "Somewhat unlikely", "Very unlikely",
                                "Somewhat Unlikely") ~ "Detractor",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(nps_group)) %>%
    count(nps_group) %>%
    mutate(pct = n / sum(n) * 100)
}

nps_share <- function(nps_summary, group) {
  v <- nps_summary$pct[nps_summary$nps_group == group]
  if (length(v) == 0) 0 else v
}

compute_satisfaction <- function(data, sat_col = "Satisfaction") {
  data %>%
    filter(!is.na(.data[[sat_col]])) %>%
    count(Satisfaction = .data[[sat_col]]) %>%
    mutate(
      pct = n / sum(n) * 100,
      Satisfaction = factor(Satisfaction, levels = satisfaction_levels)
    )
}

compute_value_summary <- function(data, cols = names(question_labels)) {
  data %>%
    select(all_of(cols)) %>%
    pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
    filter(!is.na(Response)) %>%
    group_by(Question, Response) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Question) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(
      pct_signed = case_when(
        Response %in% c("Yes", "Somewhat") ~ pct,
        Response %in% c("No", "I didn't know about this") ~ -pct,
        TRUE ~ 0
      ),
      Response = factor(Response, levels = c("Yes", "Somewhat", "No", "I didn't know about this")),
      Question = factor(Question, levels = rev(names(question_labels)))
    )
}

# "English (41), French (5)" from a free-form language column
language_summary <- function(language) {
  short <- case_when(
    grepl("English", language) ~ "English",
    grepl("Fran", language)    ~ "French",
    grepl("Espa", language)    ~ "Spanish",
    TRUE                       ~ "Other"
  )
  counts <- sort(table(short), decreasing = TRUE)
  paste0(names(counts), " (", as.integer(counts), ")", collapse = ", ")
}

# Direction of a change: "flat" below the threshold
trend_direction <- function(new, old, threshold = cfg$trend_threshold_points) {
  diff <- new - old
  if (abs(diff) < threshold) "flat" else if (diff > 0) "up" else "down"
}

trend_arrow <- function(new, old, threshold = cfg$trend_threshold_points) {
  c(up = "\u2191", down = "\u2193", flat = "\u2192")[[trend_direction(new, old, threshold)]]
}

# Arrow in the WHO trend colors: up improving, down worsening, flat stagnating
trend_html <- function(new, old, threshold = cfg$trend_threshold_points) {
  color <- c(up = collab$improving_text, down = collab$worsening_text,
             flat = collab$stagnating_text)[[trend_direction(new, old, threshold)]]
  sprintf('<span style="color:%s;font-weight:700;">%s</span>', color, trend_arrow(new, old, threshold))
}

# ---- Results per round -----------------------------------------------------

read_survey <- function(rnd) {
  path <- file.path(cfg$dir_clean, "survey", paste0(rnd$id, ".csv"))
  read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(across(everything(), trimws))
}

survey_stats <- function(rnd) {
  data <- read_survey(rnd)
  nps  <- compute_nps(data)
  sat  <- compute_satisfaction(data)
  val  <- compute_value_summary(data)

  # Offering with the highest share of "Yes" or "Somewhat" answers
  top_offering <- val %>%
    filter(Response %in% c("Yes", "Somewhat")) %>%
    group_by(Question) %>%
    summarise(pct = sum(pct), .groups = "drop") %>%
    slice_max(pct, n = 1, with_ties = FALSE)

  list(
    id        = rnd$id,
    label     = rnd$label,
    n         = nrow(data),
    languages = if ("Language" %in% names(data)) language_summary(data$Language) else rnd$languages_note,
    nps_summary = nps,
    nps_score   = round(nps_share(nps, "Promoter") - nps_share(nps, "Detractor")),
    promoters   = round(nps_share(nps, "Promoter"), 1),
    passives    = round(nps_share(nps, "Passive"), 1),
    detractors  = round(nps_share(nps, "Detractor"), 1),
    satisfaction = sat,
    sat_positive = sum(sat$pct[sat$Satisfaction %in% c("Very satisfied", "Somewhat satisfied")]),
    value        = val,
    top_offering_label = unname(question_labels[as.character(top_offering$Question)]),
    top_offering_pct   = top_offering$pct
  )
}

survey_results <- setNames(
  lapply(cfg$survey_rounds, survey_stats),
  vapply(cfg$survey_rounds, function(r) r$id, character(1))
)
