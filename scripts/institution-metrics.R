# Table 1

# Load the Excel file
file_path <- "data/collab-analysis-manual-info.xlsx"  # Adjust path if needed
manual_data <- read_excel(file_path)

# Ensure the Country column exists & handle missing values
manual_data <- manual_data %>%
  mutate(Country = ifelse(is.na(Country) | Country == "", "Unknown", Country))

# Count users per country
country_counts <- manual_data %>%
  count(Country, name = "Users")

# Identify countries with only 1 user and group them into "Other"
country_table <- country_counts %>%
  mutate(Country = ifelse(Users < 2, "Other", Country)) %>%
  group_by(Country) %>%
  summarise(Users = sum(Users)) %>%
  ungroup() %>%
  mutate(Percentage = round((Users / sum(Users)) * 100, 1),
         `Users (%)` = paste0(Users, " (", Percentage, "%)")) %>%
  select(Country, `Users (%)`)  

# Separate "Other" from the rest
other_row <- country_table %>% filter(Country == "Other")
country_table <- country_table %>% filter(Country != "Other")

# Sort main table by user count (descending)
country_table <- country_table %>%
  arrange(desc(as.numeric(gsub(" .*", "", `Users (%)`))))  

# Append "Other" at the bottom
country_table <- bind_rows(country_table, other_row)






# Table 2

# Fill Organization_m with Organization_n if empty, then mark as "Unknown" if still empty
manual_data <- manual_data %>%
  mutate(
    Organization_m = ifelse(is.na(Organization_m) | Organization_m == "", Organization_n, Organization_m),
    Organization_m = ifelse(is.na(Organization_m) | Organization_m == "", "Unknown", Organization_m)
  )

# Standardize WHO-related names into "WHO"
manual_data <- manual_data %>%
  mutate(
    Organization_m = case_when(
      Organization_m %in% c(
        "WHO HQ", "WHO Situations of Concern", "WHO WPRO", 
        "Pan American Health Organization", "WHO SEARO", "PAHO"
      ) ~ "WHO",
      TRUE ~ Organization_m
    )
  )

# Count users per organization
organization_counts <- manual_data %>%
  count(Organization_m, name = "Users")

# Identify organizations with fewer than 5 users and group them into "Other"
organization_table <- organization_counts %>%
  mutate(Organization_m = ifelse(Users < 5, "Other", Organization_m)) %>%
  group_by(Organization_m) %>%
  summarise(Users = sum(Users)) %>%
  ungroup() %>%
  mutate(Percentage = round((Users / sum(Users)) * 100, 1),
         `Users (%)` = paste0(Users, " (", Percentage, "%)")) %>%
  select(Organization_m, `Users (%)`)  

# Separate "Other" from the rest
other_row <- organization_table %>% filter(Organization_m == "Other")
organization_table <- organization_table %>% filter(Organization_m != "Other")

# Sort main table by user count (descending)
organization_table <- organization_table %>%
  arrange(desc(as.numeric(gsub(" .*", "", `Users (%)`))))  

# Append "Other" at the bottom
organization_table <- bind_rows(organization_table, other_row)