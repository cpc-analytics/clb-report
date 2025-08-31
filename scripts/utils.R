# Legend set up
addLegendCustom <- function(map, colors, labels, sizes = NULL, title = NULL, position = "bottomright") {
  legendHTML <- paste0(
    "<div style='padding:10px;font-size:13px;color:#2F3E46;font-family:sans;'>",
    if (!is.null(title)) paste0("<b>", title, "</b><br><br>"),
    paste0("<span style='background:", colors, ";border-radius:50%;display:inline-block;width:10px;height:10px;margin-right:5px;'></span>",
           labels, collapse = "<br>"),
    "</div>"
  )
  map %>% addControl(html = legendHTML, position = position)
}
