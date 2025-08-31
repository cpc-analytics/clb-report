library(dplyr)
library(stringr)

# Clean and count groups
data1_cleaned <- data1 %>%
  mutate(
    group_list = str_split(group_names, ";"),
    group_list = lapply(group_list, function(x) x[!str_detect(x, "^trust_level_\\d+$")]),
    group_count = sapply(group_list, length)
  )

# Make bin labels (e.g. "0", "1", ..., "6+")
max_groups <- max(data1_cleaned$group_count)
bins <- 0:max_groups
bins[bins > 6] <- 6  # group all 6+ together

data1_cleaned <- data1_cleaned %>%
  mutate(group_bin = ifelse(group_count >= 6, "6+", as.character(group_count)))

# Summary table
community_mem_table <- data1_cleaned %>%
  count(group_bin) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  mutate(percent = sprintf("%.1f", percent)) %>%  # Format to 1 decimal place
  rename(
    'Number of Communities' = group_bin,
    'Number of Members' = n,
    'Percentage of Members (%)' = percent
  )
