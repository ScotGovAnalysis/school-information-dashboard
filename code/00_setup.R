#########################################################################
# Name of file - 00_setup.R
# Data release - School Information Dashboard
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Sets up environment required for running SID RAP. 
# This is the only file which requires manual changes before the 
# RAP process is run.
#########################################################################


### 1 - Run Label - TO UPDATE ----

## The label used to name the folder of output data from this run. 
## This should be changed for every update to name the new output folder. 
## Follow the naming convention: "year-month of publish" e.g. 2021-May 

run_label <- "2025-April"


### 2 - Dates - TO UPDATE ----

## Update years of data files to be used for each dataset
## Note that where only a single year is required, this does not necessarily
## mean that only one year of data is included in that file. For some datasets
## (e.g. acel) each file contains a full trend and supercedes previous files.


## 2a - Datasets typically updated in April ----

# Datasets that require a single file, update with a single year

year_contacts <- 2025
year_bge      <- 2025

# School summary data updated every year
# Update the vector (list) with the new year; e.g. c(2021, 2022)

year_summary  <- c(2022, 2023, 2024)

# Attendance data requires multiple files but is typically only updated in 
# odd-numbered years; e.g. c(2021, 2023)

year_attendance <- c(2024)

# Insight data uses a rolling five year trend; ensure both start and
# end years are updated following the convention: year_start:year_end
# e.g. 2017:2021 includes years 2017, 2018, 2019, 2020, 2021. 

# Insights datasets include; attainment by deprivation, 
# attainment for all, breadth/depth, destinations and literacy/numeracy data

year_insight  <- 2020:2024


## 2b - Datasets typically updated in December ----

# Datasets that require a single file, update with a single year
year_estate         <- 2024
year_acel           <- 2024
year_PE_targets <- 2024


### 1 - Load packages ----

library(here)
library(dplyr)
library(readr)
library(readxl)
library(magrittr)
library(janitor)
library(stringr)
library(tidyr)
library(purrr)
library(rlang)
library(rstudioapi)
library(odbc)

# Load functions from functions folder of Rproject

walk(list.files(here("functions"), pattern = "\\.R$", full.names = TRUE), 
     source)


### 2 - Create folders ----

# If output folders for run_label specified above 
# don't already exist, create folders

folders <- paste0(
  here("app"), "/", 
  c("primary", "secondary", "special"), "_data"
)

# Create data folders
walk(
  folders,
  ~ if(!file.exists(.x)) dir.create(.x)
)

# Create subfolders for run_label
walk(
  paste0(folders, "/", run_label),
  ~ if(!file.exists(.x)) dir.create(.x)
)

# Create lookups folder for school_lookup
if(!file.exists(here("lookups", "school_lookup"))) {
  dir.create(here("lookups", "school_lookup"))
}


### END OF SCRIPT ###