library(readr)
library(dplyr)
library(tidyr)

# ─── Shared Setup ──────────────────────────────────────────────────────────────
custom_palette <- c("#00205C", "#009ADE", "#F4A81D", "#80BC00")  # Yes, Somewhat, No, I didn't know

satisfaction_levels <- c("Very dissatisfied", "Somewhat dissatisfied",
                         "Neutral", "Somewhat satisfied", "Very satisfied")

question_labels <- c(
  "Network"     = "Networking, connecting with others",
  "Analysis"    = "Opportunities to Collaborate and/or conduct joint analysis",
  "Events"      = "Attending events (webinars, meetings, workshops etc)",
  "Access"      = "Access to tools, code, packages, and/or data",
  "Knowledge"   = "Knowledge sharing",
  "Interaction"  = "Interaction with decision makers"
)

# ─── Helper Functions ──────────────────────────────────────────────────────────
compute_nps <- function(data, rec_col = "Recommended") {
  data %>%
    mutate(
      nps_group = case_when(
        .data[[rec_col]] == "Very likely"       ~ "Promoter",
        .data[[rec_col]] == "Somewhat likely"    ~ "Passive",
        .data[[rec_col]] %in% c("Neutral", "Somewhat unlikely", "Very unlikely",
                                "Somewhat Unlikely") ~ "Detractor",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(nps_group)) %>%
    count(nps_group) %>%
    mutate(pct = n / sum(n) * 100)
}

compute_nps_score <- function(nps_summary) {
  promoters  <- nps_summary$pct[nps_summary$nps_group == "Promoter"]
  detractors <- nps_summary$pct[nps_summary$nps_group == "Detractor"]
  if (length(promoters) == 0) promoters <- 0
  if (length(detractors) == 0) detractors <- 0
  round(promoters - detractors)
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

compute_value_summary <- function(data, cols = c("Network", "Analysis", "Events",
                                                 "Access", "Knowledge", "Interaction")) {
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

# ─── June 2025 ─────────────────────────────────────────────────────────────────
survey_jun <- read_csv(
  here::here("data", "survey", "com-pulse-survey-25-06.csv"),
  col_names = FALSE,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)
colnames(survey_jun) <- survey_jun[1, ] %>% unlist() %>% as.character()
survey_jun <- survey_jun[-1, ]
colnames(survey_jun)[8]  <- "Satisfaction"
colnames(survey_jun)[9]  <- "Recommended"
colnames(survey_jun)[10:15] <- c("Network", "Analysis", "Events", "Access", "Knowledge", "Interaction")

nps_summary_jun         <- compute_nps(survey_jun)
nps_score_jun           <- compute_nps_score(nps_summary_jun)
satisfaction_summary_jun <- compute_satisfaction(survey_jun)
survey_summary_jun      <- compute_value_summary(survey_jun)

jun_n         <- nrow(survey_jun)
jun_languages <- "English (41), French (5), and Spanish (3)"
jun_promoters  <- round(nps_summary_jun$pct[nps_summary_jun$nps_group == "Promoter"], 1)
jun_passives   <- round(nps_summary_jun$pct[nps_summary_jun$nps_group == "Passive"], 1)
jun_detractors <- round(nps_summary_jun$pct[nps_summary_jun$nps_group == "Detractor"], 1)

# ─── December 2025 ────────────────────────────────────────────────────────────
survey_dec <- read_csv(
  here::here("data", "survey", "com-pulse-survey-25-12.csv"),
  col_names = FALSE,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)
colnames(survey_dec) <- survey_dec[1, ] %>% unlist() %>% as.character()
survey_dec <- survey_dec[-1, ]

colnames(survey_dec)[6]  <- "Language"
colnames(survey_dec)[7]  <- "Satisfaction"
colnames(survey_dec)[8]  <- "Recommended"
colnames(survey_dec)[9:14] <- c("Network", "Analysis", "Events", "Access", "Knowledge", "Interaction")

survey_dec <- survey_dec %>%
  mutate(across(c(Satisfaction, Recommended, Network:Interaction),
                ~ trimws(as.character(.))))

nps_summary_dec         <- compute_nps(survey_dec)
nps_score_dec           <- compute_nps_score(nps_summary_dec)
satisfaction_summary_dec <- compute_satisfaction(survey_dec)
survey_summary_dec      <- compute_value_summary(survey_dec)

dec_n <- nrow(survey_dec)

dec_lang_counts <- survey_dec %>%
  mutate(lang_short = case_when(
    grepl("English", Language)  ~ "English",
    grepl("Fran", Language)     ~ "French",
    grepl("Espa", Language)     ~ "Spanish",
    TRUE                        ~ "Other"
  )) %>%
  count(lang_short) %>%
  arrange(desc(n))

dec_languages <- paste(
  paste0(dec_lang_counts$lang_short, " (", dec_lang_counts$n, ")"),
  collapse = ", ")

dec_promoters  <- round(nps_summary_dec$pct[nps_summary_dec$nps_group == "Promoter"], 1)
dec_passives   <- round(nps_summary_dec$pct[nps_summary_dec$nps_group == "Passive"], 1)
dec_detractors <- if (any(nps_summary_dec$nps_group == "Detractor")) {
  round(nps_summary_dec$pct[nps_summary_dec$nps_group == "Detractor"], 1)
} else { 0 }

# ─── Trend Helpers ────────────────────────────────────────────────────────────
sat_positive <- function(sat_df) {
  sat_df %>%
    filter(Satisfaction %in% c("Very satisfied", "Somewhat satisfied")) %>%
    summarise(total = sum(pct)) %>%
    pull(total)
}

jun_sat_pos <- sat_positive(satisfaction_summary_jun)
dec_sat_pos <- sat_positive(satisfaction_summary_dec)

events_yes <- function(val_df) {
  val_df %>%
    filter(Question == "Events", Response == "Yes") %>%
    pull(pct)
}

jun_events_yes <- events_yes(survey_summary_jun)
dec_events_yes <- events_yes(survey_summary_dec)

# Direction arrow — returns arrow only
trend_arrow <- function(new, old, threshold = 3) {
  diff <- new - old
  if (abs(diff) < threshold) return("→")
  if (diff > 0) return("↑")
  return("↓")
}