
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
library(purrr)
library(stringr)


# Turn off scientific notation format

options(scipen = 999)


# Set run_label to define which data to use

shiny_run_label <- "2022-July"


# Set school type for app

school_type <- "Primary"


# Load ggplot theme and modules

source(here("app", "sid_theme.R"))

walk(list.files(here("app", "modules"), full.names = TRUE), source)


# Set default ggplot themes and colours

theme_set(sid_theme())
update_geom_defaults("line", list(colour = "#3182bd", size = 1))
update_geom_defaults("col", list(colour = "#3182bd", fill = "#3182bd"))


### END OF SCRIPT ###