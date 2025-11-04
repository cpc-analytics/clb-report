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

# Map text responses to NPS groups (Neutral treated as Detractor)
nps_summary <- survey_data %>%
  mutate(
    nps_group = case_when(
      Recommended == "Very likely" ~ "Promoter",
      Recommended == "Somewhat likely" ~ "Passive",
      Recommended %in% c("Neutral", "Somewhat Unlikely", "Very unlikely") ~ "Detractor",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(nps_group)) %>%
  count(nps_group) %>%
  mutate(pct = n / sum(n) * 100)

# Compute NPS (both integer and percentage forms)
nps_result <- nps_summary %>%
  summarise(
    promoters = pct[nps_group == "Promoter"],
    detractors = pct[nps_group == "Detractor"],
    NPS_integer = round(promoters - detractors),   # conventional reporting
    NPS_percentage = promoters - detractors        # exact % difference
  )

# Print results
print(nps_summary)
print(nps_result)


colnames(survey_data)[8] <- "Satisfaction"

# Rename column 8 to Satisfaction
colnames(survey_data)[8] <- "Satisfaction"

satisfaction_summary <- survey_data %>%
  filter(!is.na(Satisfaction)) %>%
  count(Satisfaction) %>%
  mutate(pct = n / sum(n) * 100)

# Order factors logically
satisfaction_summary$Satisfaction <- factor(
  satisfaction_summary$Satisfaction,
  levels = c("Very dissatisfied", "Somewhat dissatisfied", 
             "Neutral", "Somewhat satisfied", "Very satisfied")
)








# Rename columns 10–15
colnames(survey_data)[10:15] <- c("Network", "Analysis", "Events", "Access", "Knowledge", "Interaction")

custom_palette <- c("#00205C", "#009ADE", "#F4A81D", "#80BC00")  # Yes, Somewhat, No, I didn't know


# Reshape data from wide to long
survey_long <- survey_data %>%
  select(Network:Interaction) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
  filter(!is.na(Response))

# Summarize percentages
survey_summary <- survey_long %>%
  group_by(Question, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Question) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

# Assign positive/negative
survey_summary <- survey_summary %>%
  mutate(
    pct_signed = case_when(
      Response %in% c("Yes", "Somewhat") ~ pct,
      Response %in% c("No", "I didn't know about this") ~ -pct,
      TRUE ~ 0
    ),
    Response = factor(Response, levels = c("Yes", "Somewhat", "No", "I didn't know about this"))
  )

# Replace short names with full category labels
question_labels <- c(
  "Network" = "Networking, connecting with others",
  "Analysis" = "Opportunities to Collaborate and/or conduct joint analysis",
  "Events" = "Attending events (webinars, meetings, workshops etc)",
  "Access" = "Access to tools, code, packages, and/or data",
  "Knowledge" = "Knowledge sharing",
  "Interaction" = "Interaction with decision makers"
)

survey_summary$Question <- factor(survey_summary$Question, 
                                  levels = rev(names(question_labels)))  # top-to-bottom order

