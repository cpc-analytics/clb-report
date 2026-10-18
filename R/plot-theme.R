# Shared design for the web report, following the WHO Data Design Language:
# colors, font, text sizes and chart themes. Sourced by R/setup.R.
# Page CSS (assets/styles.css) uses the same colors. See guides/design.md for usage rules.

library(showtext)
showtext_auto()
font_add_google("Noto Sans", "Noto Sans")

# ---- Colors ----------------------------------------------------------------

collab <- list(
  # brand
  brand          = "#008dc9",
  brand_stronger = "#0f2d5b",
  brand_weaker   = "#90d3fb",
  # foreground and background
  fg             = "#000000",
  fg_weaker      = "#595959",   # axes, secondary text
  fg_weakest     = "#cccccc",   # ticks, gridlines
  bg             = "#ffffff",
  bg_weaker      = "#f7f7f7",   # backdrop behind cards, shaded rows
  line           = "#ececec",   # light row separators in tables (tint of fg_weakest)
  # categories 0 to 5 (nominal data) and "other"
  cat0 = "#f4a81d", cat1 = "#f26829", cat2 = "#bd53bd",
  cat3 = "#6363c0", cat4 = "#008dc9", cat5 = "#40bf73",
  cat_other = "#cccccc",
  # sequential brand scale, light to dark (ordered quantities)
  seq1 = "#d7e6f3", seq2 = "#75b6d6", seq3 = "#0582bb", seq4 = "#0f2d5b",
  # trend: improving, worsening, stagnating (text variants meet contrast rules)
  improving = "#27a3b4", improving_text = "#19818f",
  worsening = "#cf7e50", worsening_text = "#b15e2f",
  stagnating = "#746985", stagnating_text = "#746985"
)

# Text colors that go with the category colors 0 to 5
collab_cat_text <- c("#754d06", "#9a3709", "#8e2f8e", "#4b4baf", "#245993", "#1d6339")

# Multi-series charts: brand first, then categories in order
collab_palette <- unname(unlist(collab[c("brand", "cat0", "cat1", "cat2", "cat3", "cat5")]))

# Sequential scale for the four volume bins of the map
collab_sequential <- unname(unlist(collab[c("seq1", "seq2", "seq3", "seq4")]))

# ---- WHO regions: fixed color per region -----------------------------------

collab_region_colors <- c(AFR = "#6363c0", AMR = "#f26829", EMR = "#bd53bd",
                          EUR = "#008dc9", SEAR = "#40bf73", WPR = "#f4a81d")

# Region key (AFR, AMR, ...) from a code or name such as "AFRO" or "Eastern Mediterranean"
region_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  dplyr::case_when(
    grepl("^AFR|AFRICA", x)                  ~ "AFR",
    grepl("^AMR|AMERICA", x)                 ~ "AMR",
    grepl("^EMR|EASTERN MED", x)             ~ "EMR",
    grepl("^EUR", x)                         ~ "EUR",
    grepl("^SEAR|SOUTH.?EAST", x)            ~ "SEAR",
    grepl("^WPR|WESTERN PACIFIC", x)         ~ "WPR",
    TRUE                                     ~ NA_character_
  )
}

# Named fill values for a vector of region labels, for scale_fill_manual().
# Labels that are not a WHO region get the "other" color.
region_fill_values <- function(regions) {
  regions <- as.character(regions)
  cols <- unname(collab_region_colors[region_key(regions)])
  cols[is.na(cols)] <- collab$cat_other
  stats::setNames(cols, regions)
}

# ---- Text sizes (points; about 12 px and up once the figure is scaled) -----

collab_size <- list(text = 10.5, subtitle = 12, title = 14, label = 3.7)

# Label color on a filled segment: white when it has at least 3:1 contrast, otherwise black
label_color <- function(fill) {
  rgb <- grDevices::col2rgb(fill) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  ifelse(1.05 / (lum + 0.05) >= 3, "white", collab$fg)
}

# ---- Chart themes ----------------------------------------------------------

# Elements every chart shares
theme_collab_base <- function() {
  theme_minimal(base_size = 10, base_family = "Noto Sans") +
    theme(
      plot.title       = element_text(size = collab_size$title, face = "bold", color = collab$brand_stronger),
      plot.subtitle    = element_text(size = collab_size$subtitle, color = collab$fg_weaker),
      axis.text        = element_text(size = collab_size$text, color = collab$fg_weaker),
      legend.text      = element_text(size = collab_size$text, color = collab$fg_weaker),
      panel.grid.minor = element_blank()
    )
}

# Time-series line charts
theme_collab_line <- function(legend = "none") {
  theme_collab_base() +
    theme(
      axis.text.x        = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = collab$fg_weakest, linetype = "dotted"),
      legend.position    = legend,
      legend.title       = element_blank()
    )
}

# Horizontal bar charts with long category labels
theme_collab_hbar <- function(legend = "none") {
  theme_collab_base() +
    theme(
      panel.grid.major.x = element_line(color = collab$fg_weakest, linetype = "dotted"),
      panel.grid.major.y = element_blank(),
      legend.position    = legend
    )
}

# Single horizontal stacked bar (share of members by group): no axes, legend below
theme_collab_bar <- function() {
  theme_collab_base() +
    theme(
      axis.text.x        = element_blank(),
      axis.title.x       = element_blank(),
      axis.ticks.x       = element_blank(),
      axis.text.y        = element_blank(),
      axis.ticks.y       = element_blank(),
      panel.grid.major.x = element_line(color = collab$fg_weakest, linetype = "dotted"),
      panel.grid.major.y = element_blank(),
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.title       = element_text(size = collab_size$text, face = "bold", color = collab$fg_weaker),
      plot.margin        = margin(10, 20, 10, 20)
    )
}

# World map: no axes, legend below
theme_collab_map <- function() {
  theme_void(base_family = "Noto Sans") +
    theme(
      plot.title       = element_text(size = collab_size$title, face = "bold", color = collab$brand_stronger),
      plot.subtitle    = element_text(size = collab_size$subtitle, color = collab$fg_weaker),
      legend.position  = "bottom",
      legend.title     = element_text(size = collab_size$text, face = "bold", color = collab$fg_weaker),
      legend.text      = element_text(size = collab_size$text, color = collab$fg_weaker),
      plot.margin      = margin(10, 10, 10, 10),
      plot.background  = element_rect(fill = collab$bg, color = NA),
      panel.background = element_rect(fill = collab$bg, color = NA)
    )
}
