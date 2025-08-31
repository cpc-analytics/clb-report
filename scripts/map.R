# Load packages (optional in setup chunk)
pacman::p_load(tidyverse, whomapper, sf, lubridate, here, readxl, glue)

# Load country-region mapping
country_codes <- read_excel(here("data/country_codes.xlsx"), sheet = "Regions")

# Load IP-to-country data and merge with region codes
IP_address <- read_csv("data/output_with_country.csv") %>%
  rename(COUNTRY = country) %>%
  left_join(country_codes, by = "COUNTRY") %>%
  rename(iso_3_code = ISO3)

# Count users by ISO3 + COUNTRY to preserve names for tooltips
user_counts <- IP_address %>%
  group_by(iso_3_code, COUNTRY) %>%
  summarise(user_count = n(), .groups = "drop")

# Load WHO base map (adm level 0) and apply Robinson projection
sfs <- whomapper::pull_sfs(adm_level = 0, query_server = FALSE)
sfs <- map(sfs, ~ st_transform(.x, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# Calculate centroids for countries for bubble placement
country_centroids <- sfs$adm0 %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  select(iso_3_code, geometry)

# Join centroids and user counts
user_points <- left_join(user_counts, country_centroids, by = "iso_3_code") %>%
  filter(!is.na(user_count)) %>%
  st_as_sf() %>%
  st_transform(crs = st_crs(sfs$adm0))  # Ensure projection match

# Save map-ready objects to use in your RMarkdown
saveRDS(sfs, here("data/sfs.rds"))
saveRDS(user_points, here("data/user_points.rds"))


# Total number of distinct countries (with valid region info)
total_distinct_countries <- IP_address %>%
  filter(!is.na(COUNTRY)) %>%
  distinct(COUNTRY) %>%
  nrow()

# Check if any country appears in multiple regions
country_region_check <- IP_address %>%
  filter(!is.na(who_region), !is.na(COUNTRY)) %>%
  distinct(COUNTRY, who_region) %>%
  count(COUNTRY) %>%
  filter(n > 1)

# Recalculate per-region counts without adding total row
countries_per_region <- IP_address %>%
  filter(!is.na(who_region), !is.na(COUNTRY)) %>%
  distinct(COUNTRY, who_region) %>%
  count(who_region, name = "n_countries")

members_per_region <- IP_address %>%
  filter(!is.na(who_region)) %>%
  count(who_region, name = "n_members")

# Combine without total row
region_summary <- full_join(countries_per_region, members_per_region, by = "who_region") %>%
  arrange(desc(n_members))
