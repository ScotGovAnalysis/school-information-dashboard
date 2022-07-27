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

population_files <-    

    expand_grid(year = c(year_summary,"timeseries"),
                sheet = c("SCH", "LA and SCOT")
    )
 

population <- 
  
  
  # This combines the population tabs in all of the school summary data sheets
  # Stage data is included for all years but "all stage" data is only included for the current year
  # .x refers to the first column of population files (year)
  # .y refers to the second column of population files (sheet name)
  
  pmap_dfr(
    population_files,
    ~ import_summary_data(.y, .x)
  ) %>%



  # Data from LA sheet is for All publicly funded schools - recode NAs to All publicly funded schools

  
  # Recode seed_code for LA/Scotland summary rows
  mutate(seed_code = ifelse(seed_code == "NA", la_code, seed_code)) %>%
  select(-la_code, -la_name, -school) %>%
  
  
  # Filter school list and recode names
  inner_join(school_lookup, by = c("seed_code", "school_type")) %>%


  # Recode missing / suppressed values
  mutate(across(c(fte_teacher_numbers, roll, average_class, ptr),
         ~ recode_missing_values(., label = TRUE))) %>%
  

  # Reorder columns
  select(year, seed_code, la_code, la_name, school_name, school_type, roll, fte_teacher_numbers, ptr, average_class)
 

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