library(readr)
library(dplyr)

# Read CSV without using the first row as column names
survey_data <- read_csv(
  "./data/survey/com-pulse-survey.csv",
  col_names = FALSE,
  locale = locale(encoding = "UTF-8")
)


# Set proper column names manually using the first row
colnames(survey_data) <- survey_data[1, ] %>% unlist() %>% as.character()

# Remove the first row (original header row)
survey_data <- survey_data[-1, ]

colnames(survey_data)[9] <- "Recommended"


# Map responses to NPS categories
nps_data <- survey_data %>%
  mutate(
    nps_category = case_when(
      Recommended == "Very likely" ~ "Promoter",
      Recommended == "Somewhat likely" ~ "Passive",
      Recommended %in% c("Neutral", "Somewhat dissatisfied", "Very dissatisfied") ~ "Detractor",
      TRUE ~ NA_character_
    )
  )

# Count and percentages
nps_summary <- nps_data %>%
  count(nps_category) %>%
  mutate(percent = n / sum(n) * 100)
