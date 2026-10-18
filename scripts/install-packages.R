# Installs the R packages the report needs. Run once per R installation:
#   Rscript scripts/install-packages.R
#
# whomapper (WHO base maps) is installed from GitHub (whocov/whomapper), not CRAN.

cran_packages <- c(
  "tidyverse", "lubridate", "glue", "scales", "readxl", "here",
  "kableExtra", "gt", "DT", "htmltools",
  "plotly", "viridis", "gridExtra", "showtext",
  "sf", "tidygraph", "ggraph", "igraph",
  "knitr", "rmarkdown"
)

if (is.null(getOption("repos")) || identical(unname(getOption("repos")["CRAN"]), "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing)
} else {
  message("All CRAN packages are installed.")
}

if (!requireNamespace("whomapper", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  message("Installing whomapper from GitHub (whocov/whomapper)")
  remotes::install_github("whocov/whomapper", dependencies = TRUE)
}
