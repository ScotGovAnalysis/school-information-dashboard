#########################################################################
# Name of file - 02_attendance.R
# Data release - School Information Dashboard
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Creates three data sets (primary, secondary, special) 
# containing attendance trend data.
#########################################################################


### 0 - Setup ----

## this sets a directory for the code to ensure if the folder is copied 
## and moved elsewhere it will contunie to work

source(here::here("code", "00_setup.R"))

## Read in school lookup containing definitive list of schools to be 
## included and their correct names.

school_lookup <- read_rds(here("output", run_label, "school_lookup.rds"))


### 1 - Attainment data ----

# This code creates a table of every combination of year and sheet name
# of data to be read in.


attendance_files <-
  expand_grid(
    year = c(year_summary, "timeseries"),
    sheet = c("Attendance", "Att by Stage")
  ) %>%
  
  filter(!(year != max(year_summary) & sheet == "Att by Stage"))

attendance <- 
  

  # This combines the attendance tabs in all of the school summary data sheets
  # Stage data is included for all years but "all stage" data is only included for the current year
  # .x refers to the first column of attendance files (year)
  # .y refers to the second column of attendance files (sheet name)
  
  pmap_dfr(
    attendance_files,
    ~ here("data", "school_summary_statistics", 
           paste0(.x, "_school_summary_statistics.xlsx")) %>%
      read_xlsx(sheet = .y, col_types = "text") %>%
      clean_names()
  ) %>%
  
  mutate(seed_code = as.character(seed_code)) %>%
  
  # Data from Attendance sheet is for All Stages - recode NAs to All Stages
  mutate(student_stage = replace_na(student_stage, "All Stages")) %>%
  
  # Recode seed_code for LA/Scotland summary rows
  mutate(seed_code = ifelse(seed_code == "NA", la_code, seed_code)) %>%
  select(-la_code, -la_name, -school) %>%
  
  # Filter school list and recode names
  inner_join(school_lookup, by = c("seed_code", "school_type"))



### 2 - Save summary and pupil profile data sets ----


# Save primary school file

primary_attendance <- 
  attendance %>% 
  filter(school_type == "Primary") 

write_rds(
  primary_attendance,
  here("output", run_label, "primary_attendance.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  primary_attendance,
  here("output", run_label, "primary_attendance.xlsx")
)


# Save secondary school file

secondary_attendance <- attendance %>% filter(school_type == "Secondary")

write_rds(
  secondary_attendance,
  here("output", run_label, "secondary_attendance.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  secondary_attendance,
  here("output", run_label, "secondary_attendance.xlsx")
)

# Save special school file

special_attendance <- attendance %>% filter(school_type == "special")

write_rds(
  special_attendance,
  here("output", run_label, "special_attendance.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  special_attendance,
  here("output", run_label, "special_attendance.xlsx")
)


### END OF SCRIPT ####