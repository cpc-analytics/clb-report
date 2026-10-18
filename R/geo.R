# Geography helpers: country counts and the map layers.
# The country-per-member file carries country only (see ingest-data.R).

library(readr)

# Latest country file, or the one for a given quarter label such as "Q2 2026".
# Quarter files are named geo-country_q1.csv, geo-country_q2.csv, ...
geo_path <- function(quarter = NULL) {
  latest <- file.path(cfg$dir_clean, "geo-country.csv")
  if (is.null(quarter)) return(latest)

  q_file <- file.path(cfg$dir_clean,
                      paste0("geo-country_", tolower(stringr::str_extract(quarter, "Q\\d")), ".csv"))
  if (file.exists(q_file)) return(q_file)

  warning("No country file for ", quarter, " (", basename(q_file),
          "). Using geo-country.csv, which reflects the current state.")
  latest
}

read_geo <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  stop_if_unexpected(df, "country", basename(path))
  df
}

read_country_codes <- function() {
  readxl::read_excel(file.path(cfg$dir_reference, "country-codes.xlsx"), sheet = "Regions")
}

# Bubble-map inputs: WHO base map and one point per country (Robinson projection).
# Base geometry comes from whomapper.
build_map_points <- function(user_counts) {
  library(sf)
  sfs <- whomapper::pull_sfs(adm_level = 0, query_server = FALSE)
  robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  # Split shapes at the antimeridian before projecting. Without this, shapes that cross
  # it are stretched across the map as vertical lines. Borders are not altered.
  sfs <- purrr::map(sfs, function(x) {
    x %>%
      st_make_valid() %>%
      st_transform(4326) %>%
      st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
      st_transform(robin)
  })

  centroids <- sfs$adm0 %>%
    st_centroid(of_largest_polygon = TRUE) %>%
    select(iso_3_code, geometry)

  points <- left_join(user_counts, centroids, by = "iso_3_code") %>%
    filter(!is.na(user_count)) %>%
    st_as_sf() %>%
    st_transform(crs = st_crs(sfs$adm0))

  list(base = sfs$adm0, points = points)
}
