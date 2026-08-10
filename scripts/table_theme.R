# House style for gt tables across the report.
# Use collab_gt_theme() on any gt table to apply it.
# access_badge() turns "Open" / "Closed" into styled HTML pills.
# bold_md() wraps text in a bold span — handy for parent-row names that
# go through fmt_markdown (which strips row-level font weight).

library(gt)
library(htmltools)

collab_blue   <- "#009ADE"
collab_navy   <- "#00205C"
collab_grey   <- "#f8f9fa"
collab_hover  <- "#eef6fb"   # subtle blue tint on hover
collab_border <- "#e5e7eb"

# extra CSS — Lato everywhere, plus row hover.
# the descendant selector is needed because fmt_markdown wraps cells in
# spans/divs that otherwise inherit gt's default font stack.
collab_gt_css <- "
  .gt_table, .gt_table * {
    font-family: 'Lato', sans-serif !important;
  }
  .gt_table tbody tr:hover td {
    background-color: #eef6fb !important;
  }
"

collab_gt_theme <- function(tbl) {
  tbl %>%
    tab_options(
      table.font.size                = px(15),
      table.font.names               = "Lato, sans-serif",
      table.align                    = "left",
      table.border.top.style         = "none",
      table.border.bottom.style      = "none",
      heading.border.bottom.style    = "none",
      column_labels.background.color = collab_blue,
      column_labels.font.weight      = "bold",
      column_labels.padding          = px(10),
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.style = "none",
      table_body.border.top.style    = "none",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.color = collab_border,
      table_body.hlines.style        = "none",
      row.striping.background_color  = collab_grey,
      row.striping.include_table_body = TRUE,
      data_row.padding               = px(8),
      footnotes.border.bottom.style  = "none",
      footnotes.border.lr.style      = "none",
      footnotes.font.size            = px(13),
      footnotes.padding              = px(4),
      source_notes.border.bottom.style = "none",
      source_notes.font.size           = px(13)
    ) %>%
    tab_style(
      style = cell_text(color = "white"),
      locations = cells_column_labels()
    ) %>%
    opt_css(css = collab_gt_css)
}

# pill badge for Open / Private — fixed width so they line up
access_badge <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || v == "") return("")
    if (v == "Open") {
      return(as.character(htmltools::HTML(
        "<span style='display:inline-block;min-width:64px;padding:3px 10px;border-radius:10px;font-size:12px;text-align:center;font-weight:500;background-color:#eaf7ec;color:#2c7a2c;'>Open</span>"
      )))
    }
    if (v %in% c("Closed", "Private")) {
      return(as.character(htmltools::HTML(
        "<span style='display:inline-block;min-width:64px;padding:3px 10px;border-radius:10px;font-size:12px;text-align:center;font-weight:500;background-color:#faeaea;color:#a33a3a;'>Private</span>"
      )))
    }
    v
  }, character(1))
}

# render a child row's display name with the indent arrow
indent_child <- function(name) {
  paste0("&nbsp;&nbsp;&nbsp;&nbsp;<span style='color:#666;'>↳</span> ", name)
}

# wrap text in a bold span — used to force weight inside fmt_markdown cells
bold_md <- function(x) {
  paste0("<span style='font-weight:700;'>", x, "</span>")
}