#########################################################################
# Name of file - 00_shiny_setup.R
# Data release - School Information Dashboard
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Sets up environment required for running SID shiny apps. 
# This is the only file which requires manual changes before the 
# shiny apps are run. This script does not need to be run separately.
#
# NOTE - The filepaths in this script are relative to the app/ folder. 
# This means they will work when this script is run within the shiny app, 
# but not if run directly from this script.
#########################################################################


### 1 - Run Label - TO UPDATE ----

## The run label for the data to be used in the shiny apps. This label should
## match a folder in all of the following; app/primary_data/, 
## app/secondary_data/, app/special_data/.

shiny_run_label <- "2022-September"


### 2 - Load packages ----

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
library(purrr)
library(stringr)
library(shinycssloaders)
library(forcats)
library(snakecase)
library(rsconnect)


### 3 - Turn off scientific notation format ----

options(scipen = 999)


### 4 - Load shiny modules ----

walk(list.files("modules", pattern = "\\.R$", full.names = TRUE), source)
walk(list.files("modules/text_content", pattern = "\\.R$", full.names = TRUE), source)
walk(list.files("modules/secondary_attainment", pattern = "\\.R$", full.names = TRUE), source)


### 5 - Set default ggplot themes and colours ----

theme_set(sid_theme())
update_geom_defaults("line", list(colour = "#3182bd", size = 1))
update_geom_defaults("col", list(colour = "#3182bd", fill = "#3182bd"))


### 6 - Read in FAQ data ----

faq <- read_excel("modules/text_content/FAQ.xlsx")
faq_sections <- unique(faq$Section)


### END OF SCRIPT ###