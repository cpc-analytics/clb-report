# Community structure and membership. Requires load-data.R.
# Tag metadata is read from data/reference/community-tags.csv.
library(tidyverse)

community_tags <- read.csv(
  file.path(cfg$dir_reference, "community-tags.csv"),
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

community_tags <- community_tags %>%
  filter(!if_all(everything(), is.na))

type_counts <- community_tags %>%
  filter(!is.na(Tags)) %>%
  count(Type.1, sort = TRUE)

# Counts
count_of_type <- function(type) sum(type_counts$n[type_counts$Type.1 == type])
count_cop     <- count_of_type("b-CoP")
count_group   <- count_of_type("c-Group")
count_network <- count_of_type("a-Network")

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

# Member-tag pairs, one row per member and group tag
user_tags <- data1 %>%
  select(id, group_names) %>%
  separate_rows(group_names, sep = ";") %>%
  filter(!is.na(group_names), trimws(group_names) != "")

# Lookup for parent-child relationships
parent_lookup <- community_tags %>%
  filter(!is.na(Parent_Tag) & Parent_Tag != "") %>%
  select(Tags, Parent_Tag)

# Add the parent tag for every child tag a member holds. A member who has both
# the child and the parent tag is counted once per tag.
user_tags_expanded <- user_tags %>%
  left_join(parent_lookup, by = c("group_names" = "Tags")) %>%
  pivot_longer(
    cols = c(group_names, Parent_Tag),
    values_to = "tag",
    names_to = "tag_type"
  ) %>%
  filter(!is.na(tag) & tag != "") %>%
  distinct(id, tag) %>%
  rename(group_names = tag)

# Count unique members per tag
tag_counts <- user_tags_expanded %>%
  group_by(group_names) %>%
  summarise(Members = n_distinct(id), .groups = "drop")

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

# Add Privacy information: Request/Join count as Open, Private/Closed as Closed
community_tags <- community_tags %>%
  rename(Privacy = Privacy..Join..Request..Private..Closed.)

add_privacy <- function(df) {
  df %>%
    left_join(community_tags %>% select(Name, Privacy), by = "Name") %>%
    mutate(
      Privacy = case_when(
        Privacy %in% c("Request", "Join") ~ "Open",
        Privacy %in% c("Private", "Closed") ~ "Closed",
        TRUE ~ Privacy
      )
    )
}

df_cop     <- add_privacy(df_cop)
df_group   <- add_privacy(df_group)
df_network <- add_privacy(df_network)

# Cross-community connection: members in two or more CoPs (direct CoP tags only)
cop_counts <- cop_membership_counts(data1, df_cop$Tags)
cross_community_share <- mean(cop_counts$n_cops >= 2) * 100

cop_membership_summary <- cop_counts %>%
  mutate(Membership = case_when(
    n_cops == 0 ~ "None",
    n_cops == 1 ~ "1 CoP",
    n_cops == 2 ~ "2 CoPs",
    n_cops == 3 ~ "3 CoPs",
    TRUE        ~ "4+ CoPs"
  ),
  order = pmin(n_cops, 4)) %>%
  count(order, Membership, name = "Number of Users") %>%
  arrange(order) %>%
  mutate(`Percentage of Total Users` = fmt_pct(100 * `Number of Users` / sum(`Number of Users`))) %>%
  select(Membership, `Number of Users`, `Percentage of Total Users`)
