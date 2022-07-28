#########################################################################
# Name of file - 04_population.R
# Data release - School Information Dashboard
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Creates three data sets (primary, secondary, special) 
# containing population trend data.
#########################################################################


### 0 - Setup ----

## Run setup script where years for each dataset are defined and
## packages/functions are loaded.

source(here::here("code", "00_setup.R"))

## Read in school lookup containing definitive list of schools to be 
## included and their correct names.

school_lookup <- read_rds(here("output", run_label, "school_lookup.rds"))


### 1 - School Summary data ----

# This code creates a table of every combination of year and sheet name
# of data to be read in.

population_files <-    
    expand_grid(year = c(year_summary, "timeseries"),
                sheet = c("SCH", "LA and SCOT"))
 

population <- 
  
  # This combines the population tabs in all of the school summary data sheets
  # .x refers to the first column of population files (year)
  # .y refers to the second column of population files (sheet name)
  
  pmap_dfr(
    population_files,
    ~ import_summary_data(.y, .x)
  ) %>%

  # Recode seed_code for LA/Scotland summary rows
  mutate(seed_code = ifelse(seed_code == "NA", la_code, seed_code)) %>%
  select(-la_code, -la_name, -school, -sp) %>%
  
  # Restructure data to long format with column for measure name, value and
  # value label
  pivot_longer(cols = !c(year, seed_code, school_type),
               names_to = "measure",
               values_to = "value") %>%
  
  # Remove data for stage and demographic measures that is not for latest year
  filter(
    !(!measure %in% c("roll", "fte_teacher_numbers", "ptr", "average_class") &
        year != max(year_summary))
  ) %>%
  
  # Recode measure to readable format and add measure category
  mutate(
    measure_category = recode_population_measures(measure, category = TRUE),
    measure = recode_population_measures(measure),
    .before = measure
  ) %>%
  
  # Remove stage rows not applicable for school type
  filter(
    !(str_ends(measure_category, "_stage") &
        tolower(school_type) != word(measure_category, 1, sep = "_"))
  ) %>%
  
  # Recode missing / suppressed values
  mutate(
    value_label = recode_missing_values(value, label = TRUE),
    value = recode_missing_values(value)
  ) %>%
  
  # Add roll as value to every row to allow percentage calculations
  mutate(roll = ifelse(measure == "Pupil Numbers", value, NA)) %>%
  group_by(year, seed_code, school_type) %>%
  # Some roll values are missing or suppressed, code these as NA
  mutate(roll = ifelse(all(is.na(roll)), NA, max(roll, na.rm = TRUE))) %>%
  ungroup() %>%
  
  # Filter school list and recode names
  inner_join(school_lookup, by = c("seed_code", "school_type")) %>%

  # Reorder columns
  select(year, seed_code, la_code, la_name, school_name, school_type, 
         measure_category, measure, value, value_label)
 

### 2 - Save population data sets ----

# Save primary school file

primary_population <- 
  population %>% 
  filter(school_type == "Primary") 

write_rds(
  primary_population,
  here("output", run_label, "primary_population.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  primary_population,
  here("output", run_label, "primary_population.xlsx")
)


# Save secondary school file

secondary_population <- 
  population %>% filter(school_type == "Secondary") %>%
  select(-average_class)

write_rds(
  secondary_population,
  here("output", run_label, "secondary_population.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  secondary_population,
  here("output", run_label, "secondary_population.xlsx")
)

# Save special school file

special_population <- 
  population %>% filter(school_type == "Special")

write_rds(
  special_population,
  here("output", run_label, "special_population.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  special_population,
  here("output", run_label, "special_population.xlsx")
)


### END OF SCRIPT ###