# Geographic reach for the web report. Requires load-data.R.
# Defines geo (country and region summaries), geo_map (map layers) and region_summary.

source(here::here("R", "geo.R"))

geo <- summarise_geo(read_geo(geo_path()), read_country_codes())
geo_map <- build_map_points(geo$user_counts)
region_summary <- geo$region_summary
