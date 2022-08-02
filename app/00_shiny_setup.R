
# Load packages

library(shiny)
library(dplyr)
library(here)
library(readr)
library(readxl)
library(ggplot2)
library(leaflet)
library(DT)

library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggiraph)
library(ggrepel)

# library(shinycssloaders)
# library(mapview)
# library(openxlsx)
# library(shinyalert)
# library(shinythemes)
# library(htmlwidgets)
# library(shinyWidgets)
# library(datasets)

options(scipen = 999)

shiny_run_label <- "2022-July"

school_type <- "Primary"

source(here("functions", "sid_theme.R"))

theme_set(sid_theme())

update_geom_defaults("line", list(colour = "#3182bd", size = 1))

update_geom_defaults("col", list(colour = "#3182bd", fill = "#3182bd"))


### END OF SCRIPT ###