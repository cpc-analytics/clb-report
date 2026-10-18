# House style for tables: a framed table with small uppercase column headers, light row
# separators, a shaded total row and quiet chips for changes. kable_collab() styles kable
# tables (assets/styles.css does the framing), collab_gt_theme() gives gt tables the same look.
# access_badge() turns "Open" / "Closed" into styled HTML pills.
# bold_md() wraps text in a bold span, for parent-row names that
# go through fmt_markdown (which strips row-level font weight).

library(gt)
library(htmltools)

# ---- kable tables ----------------------------------------------------------

# A last column named "... Change" is shown as chips. A row whose first cell is "Total" is shaded.
kable_collab <- function(tbl, title = NULL) {
  n <- ncol(tbl)
  if (grepl("change", names(tbl)[n], ignore.case = TRUE)) {
    tbl[[n]] <- paste0('<span class="chip">', tbl[[n]], "</span>")
  }
  out <- tbl %>%
    kbl(format = "html", align = c("l", rep("r", n - 1)), escape = FALSE) %>%
    kable_styling(bootstrap_options = "basic", full_width = TRUE)

  total_rows <- which(tbl[[1]] == "Total")
  if (length(total_rows) > 0) {
    out <- out %>% row_spec(total_rows, bold = TRUE, background = collab$bg_weaker)
  }
  htmltools::tagList(
    if (!is.null(title)) htmltools::tags$div(class = "collab-title", title),
    htmltools::tags$div(class = "collab-table", htmltools::HTML(as.character(out)))
  )
}

# The month-over-month tables of the Engagement section
kable_monthly <- function(tbl, title = NULL) kable_collab(tbl, title)

# ---- gt tables -------------------------------------------------------------

# extra CSS: Noto Sans everywhere, frame, header spacing, row hover.
# the descendant selector is needed because fmt_markdown wraps cells in
# spans/divs that otherwise inherit gt's default font stack.
collab_gt_css <- paste0("
  .gt_table, .gt_table * {
    font-family: 'Noto Sans', sans-serif !important;
  }
  .gt_table {
    border-collapse: separate !important;
    border-spacing: 0 !important;
    border: 1px solid ", collab$fg_weakest, " !important;
    border-top: 3px solid ", collab$brand_stronger, " !important;
    border-radius: 4px !important;
    overflow: hidden;
  }
  .gt_table {
    table-layout: fixed !important;
    width: 100% !important;
  }
  .gt_table th:not(:first-child),
  .gt_table td:not(:first-child) {
    width: 124px;
  }
  .gt_col_heading {
    letter-spacing: 0.04em;
    vertical-align: middle !important;
  }
  .gt_row {
    vertical-align: middle !important;
  }
  .gt_table.table-striped > tbody > tr:nth-of-type(odd) > *,
  .gt_table > :not(caption) > * > * {
    --bs-table-accent-bg: transparent !important;
    --bs-table-striped-bg: transparent !important;
    box-shadow: none !important;
  }
")

collab_gt_theme <- function(tbl) {
  tbl %>%
    tab_options(
      table.font.size                   = px(13),
      table.width                       = pct(100),
      table.font.names                  = "Noto Sans, sans-serif",
      table.align                       = "left",
      table.border.top.style            = "none",
      table.border.bottom.style         = "none",
      heading.border.bottom.style       = "none",
      column_labels.background.color    = collab$bg_weaker,
      column_labels.padding             = px(10),
      column_labels.padding.horizontal  = px(16),
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.color = collab$fg_weakest,
      column_labels.border.bottom.width = px(1),
      table_body.border.top.style       = "none",
      table_body.border.bottom.style    = "none",
      table_body.hlines.style           = "solid",
      table_body.hlines.color           = collab$line,
      row.striping.include_table_body   = FALSE,
      data_row.padding                  = px(10),
      data_row.padding.horizontal       = px(16),
      footnotes.border.bottom.style     = "none",
      footnotes.border.lr.style         = "none",
      footnotes.font.size               = px(13),
      footnotes.padding                 = px(4),
      source_notes.border.bottom.style  = "none",
      source_notes.font.size            = px(13)
    ) %>%
    tab_style(
      style = cell_text(color = collab$brand_stronger, size = px(11), weight = 600, transform = "uppercase"),
      locations = cells_column_labels()
    ) %>%
    opt_css(css = collab_gt_css)
}

# Open / Private as plain colored text (no box)
access_badge <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || v == "") return("")
    if (v == "Open") {
      return(sprintf("<span style='color:%s;font-weight:600;'>Open</span>", collab_cat_text[6]))
    }
    if (v %in% c("Closed", "Private")) {
      return(sprintf("<span style='color:%s;font-weight:600;'>Private</span>", collab_cat_text[2]))
    }
    v
  }, character(1))
}

# indent a child row's display name (no marker, just space)
indent_child <- function(name) {
  paste0("<span style='display:inline-block;width:1.6em;'></span>", name)
}

# wrap text in a bold span, used to force weight inside fmt_markdown cells
bold_md <- function(x) {
  paste0("<span style='font-weight:700;'>", x, "</span>")
}
