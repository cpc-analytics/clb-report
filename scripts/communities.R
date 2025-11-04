library(tidyverse)

# Load community tags
community_tags <- read.csv(
  "data/communities/community-tags.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

community_tags <- community_tags %>%
  filter(!if_all(everything(), is.na))

# Basic counts
na_count <- sum(is.na(community_tags$Tags))

type_counts <- community_tags %>%
  filter(!is.na(Tags)) %>%
  count(Type.1, sort = TRUE)

type_summary <- community_tags %>%
  filter(!is.na(Tags)) %>%
  group_by(Type.1) %>%
  summarise(
    count = n(),
    tags = list(unique(Tags)),
    .groups = "drop"
  )

tag_lists <- community_tags %>%
  filter(!is.na(Tags)) %>%
  group_by(Type.1) %>%
  summarise(tags = list(unique(Tags)), .groups = "drop") %>%
  deframe()

# Counts
count_cop <- type_counts %>% filter(Type.1 == "b-CoP") %>% pull(n)
count_group <- type_counts %>% filter(Type.1 == "c-Group") %>% pull(n)
count_network <- type_counts %>% filter(Type.1 == "a-Network") %>% pull(n)

# Create base dataframes
df_cop <- community_tags %>%
  filter(Type.1 == "b-CoP" & !is.na(Tags)) %>%
  select(Tags, Name, Parent_Tag)

df_group <- community_tags %>%
  filter(Type.1 == "c-Group" & !is.na(Tags)) %>%
  select(Tags, Name, Parent_Tag)

df_network <- community_tags %>%
  filter(Type.1 == "a-Network" & !is.na(Tags)) %>%
  select(Tags, Name, Parent_Tag)

# Process user tags
user_tags <- data1 %>%
  filter(!(email %in% c("discobot_email", "no_email"))) %>%
  mutate(UserID = row_number()) %>%
  separate_rows(group_names, sep = ";") %>%
  filter(group_names != "") %>%
  filter(group_names != " ")  # Also filter out spaces

# Create a lookup for parent-child relationships
parent_lookup <- community_tags %>%
  filter(!is.na(Parent_Tag) & Parent_Tag != "") %>%
  select(Tags, Parent_Tag)

# CRITICAL: Expand user tags to include parent tags, but avoid double counting
user_tags_expanded <- user_tags %>%
  # First, get all the tags users already have
  select(UserID, group_names) %>%
  # Then add parent tags for any child tags they have
  left_join(parent_lookup, by = c("group_names" = "Tags")) %>%
  # Reshape to have one row per user-tag combination
  pivot_longer(
    cols = c(group_names, Parent_Tag),
    values_to = "tag",
    names_to = "tag_type"
  ) %>%
  filter(!is.na(tag) & tag != "") %>%
  # CRITICAL: Remove duplicates - if user has both child and parent tag, count only once per tag
  distinct(UserID, tag) %>%
  rename(group_names = tag)

# Verify no double counting (optional, for debugging)
# user_tag_check <- user_tags_expanded %>%
#   group_by(UserID, group_names) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   filter(count > 1)
# 
# if(nrow(user_tag_check) > 0) {
#   warning("Double counting detected for some user-tag combinations!")
#   print(user_tag_check)
# }

# Count unique members per tag
tag_counts <- user_tags_expanded %>%
  group_by(group_names) %>%
  summarise(Members = n_distinct(UserID), .groups = "drop")  # Use n_distinct for extra safety

# Join member counts
df_cop <- df_cop %>%
  left_join(tag_counts, by = c("Tags" = "group_names"))

df_group <- df_group %>%
  left_join(tag_counts, by = c("Tags" = "group_names"))

df_network <- df_network %>%
  left_join(tag_counts, by = c("Tags" = "group_names"))

# Replace NA with 0
df_cop$Members[is.na(df_cop$Members)] <- 0
df_group$Members[is.na(df_group$Members)] <- 0
df_network$Members[is.na(df_network$Members)] <- 0

# Sort by members
df_cop <- df_cop %>%
  arrange(desc(Members))

df_group <- df_group %>%
  arrange(desc(Members))

df_network <- df_network %>%
  arrange(desc(Members))

# Add Privacy information
community_tags <- community_tags %>%
  rename(Privacy = Privacy..Join..Request..Private..Closed.)

df_cop <- df_cop %>%
  left_join(
    community_tags %>% select(Name, Privacy),
    by = "Name"
  ) %>%
  mutate(
    Privacy = case_when(
      Privacy %in% c("Request", "Join") ~ "Open",
      Privacy %in% c("Private", "Closed") ~ "Closed",
      TRUE ~ Privacy
    )
  )

df_group <- df_group %>%
  left_join(
    community_tags %>% select(Name, Privacy),
    by = "Name"
  ) %>%
  mutate(
    Privacy = case_when(
      Privacy %in% c("Request", "Join") ~ "Open",
      Privacy %in% c("Private", "Closed") ~ "Closed",
      TRUE ~ Privacy
    )
  )

df_network <- df_network %>%
  left_join(
    community_tags %>% select(Name, Privacy),
    by = "Name"
  ) %>%
  mutate(
    Privacy = case_when(
      Privacy %in% c("Request", "Join") ~ "Open",
      Privacy %in% c("Private", "Closed") ~ "Closed",
      TRUE ~ Privacy
    )
  )