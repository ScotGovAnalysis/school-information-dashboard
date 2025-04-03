#########################################################################
# Name of file - 03_attendance.R
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

## Run setup script where years for each dataset are defined and
## packages/functions are loaded.

source(here::here("code", "00_setup.R"))

## Read in school lookup containing definitive list of schools to be 
## included and their correct names.

school_lookup <- read_rds(
  here("lookups", "school_lookup", paste0(run_label, "_school_lookup.rds"))
)


### 1 - Attendance data ----

# This code creates a table of every combination of year and sheet name
# of data to be read in.

attendance_files <-
  expand_grid(
    year = c(year_attendance, "timeseries"),
    sheet = c("Attendance", "Att by Stage")
  ) %>%
  
  # Attendance by stage is required for latest year only
  # Remove Att by Stage rows for any other years
  filter(!(year != max(year_attendance) & sheet == "Att by Stage"))


attendance <- 

  # This combines the attendance tabs in all of the school summary data sheets
  # Stage data is included for all years but "all stage" data is only included 
  # for the current year
  # .x refers to the first column of attendance files (year)
  # .y refers to the second column of attendance files (sheet name)
  
  pmap_dfr(
    attendance_files,
    ~ import_summary_data(.y, .x)
  ) %>%
  
  # Data from Attendance sheet is for All Stages - recode NAs to All Stages
  mutate(stage = replace_na(stage, "All Stages")) %>%
  
  # Recode seed_code for LA/Scotland summary rows
  mutate(seed_code = ifelse(seed_code == "NA", la_code, seed_code)) %>%
  select(-la_code, -la_name, -school) %>%
  
  # Restructure data to long format with column for measure name, value and
  # value label
  pivot_longer(cols = !c(year, seed_code, school_type, stage),
               names_to = "measure",
               values_to = "value") %>%
  mutate(measure = recode(measure,
    attendance = "Attendance",
    auth_absence = "Authorised Absence",
    unauth_absence = "Unauthorised Absence",
    temp_exclusions = "Temporary Exclusions"
  )) %>%
  
  mutate(
    
    # Recode missing/suppressed values
    value_label = recode_missing_values(value, label = TRUE, label_perc = TRUE),
    value = recode_missing_values(value),
    
    # Add chart_label to show suppressed values on bar chart
    chart_label = ifelse(value_label %in% c("z", "c", "x"), value_label, "")
    
  ) %>%
  
  # Filter school list and recode names
  inner_join(school_lookup, by = c("seed_code", "school_type")) %>%
  
  # Reorder columns
  select(year, seed_code, la_code, la_name, school_name, school_type,
         stage, measure, value, value_label, chart_label)


### 2 - Save attendance data sets ----

# Save primary school file

primary_attendance <- 
  attendance %>% 
  filter(school_type == "Primary") 

write_rds(
  primary_attendance,
  here("app", "primary_data", run_label, "primary_attendance.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  primary_attendance,
  here("app", "primary_data", run_label, "primary_attendance.xlsx")
)


# Save secondary school file

secondary_attendance <- attendance %>% filter(school_type == "Secondary")

write_rds(
  secondary_attendance,
  here("app", "secondary_data", run_label, "secondary_attendance.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  secondary_attendance,
  here("app", "secondary_data", run_label, "secondary_attendance.xlsx")
)

# Save special school file

special_attendance <- attendance %>% filter(school_type == "Special")

write_rds(
  special_attendance,
  here("app", "special_data", run_label, "special_attendance.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  special_attendance,
  here("app", "special_data", run_label, "special_attendance.xlsx")
)


### END OF SCRIPT ####