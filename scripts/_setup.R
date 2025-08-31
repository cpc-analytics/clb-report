# scripts/_setup.R


# Data manipulation & wrangling
library(tidyverse)    # includes dplyr, tidyr, tibble, stringr, etc.
library(lubridate)    # date and time handling
library(glue)         # string interpolation
library(scales)       # scaling for plots

# Data import
library(readxl)       # reading Excel files
library(here)         # project-based file paths

# Tables & reporting
library(kableExtra)   # enhanced tables
library(DT)           # interactive tables
library(htmltools)    # HTML tools for tables/reports

# Visualization
library(plotly)       # interactive plots
library(viridis)      # color scales
library(gridExtra)    # arranging multiple plots

# Spatial data
library(sf)           # handling spatial/vector data

# Network analysis
library(tidygraph)    # tidy interface for networks
library(ggraph)       # graph visualization
library(igraph)       # network analysis backend

