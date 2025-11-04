


# Community of Practice Events

# Load the CSV
cops_events <- read.csv("data/communities/CoPs_events.csv")

# Select and tidy the data
cops_events_clean <- cops_events %>%
  select(CoP, Date, Event_Type, Attendees, Location) %>%
  mutate(
    Event_Type = str_to_title(Event_Type),
    Attendees = ifelse(Attendees == "????", "N/A", Attendees),
    Date_parsed = ymd(Date)  # Parse date (Year-Month-Day format)
  ) %>%
  arrange(desc(Date_parsed)) %>%  # Sort by parsed date
  select(-Date_parsed) %>%  # Remove helper column
  rename(
    `Community` = CoP,
    `Event Type` = Event_Type
  )

# Identify rows from the last month
one_month_ago <- Sys.Date() - 30
date_check <- ymd(cops_events_clean$Date)
recent_rows <- which(date_check >= one_month_ago)

# Display the table with scrollable box (approx 10 rows visible)
cops_events_clean %>%
  kbl(
    format = "html",
    align = c("l", "l", "l", "l", "r"),
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    font_size = 15,
    position = "center"
  ) %>%
  column_spec(1, width = "35em") %>%
  column_spec(2, width = "10em") %>%
  column_spec(3, width = "10em") %>%
  column_spec(4, width = "8em") %>%
  column_spec(5, width = "8em") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#019ADE") %>%
  row_spec(recent_rows, background = "#d4edda") %>%
  scroll_box(height = "400px")  # Adjust height to show ~10 rows at a time

