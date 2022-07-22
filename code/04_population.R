#########################################################################
# Name of file - 03_population.R
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

## this sets a directory for the code to ensure if the folder is copied 
## and moved elsewhere it will contunie to work

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
    ~ here("data", "school_summary_statistics", 
           paste0(.x, "_school_summary_statistics.xlsx")) %>%
      read_xlsx(sheet = .y, col_types = "text") %>%
      clean_names()
  ) %>%
  
  mutate(seed_code = as.character(seed_code)) %>%

  ### 2 - Join data, filter schools, update school names and recode NAs  ----


  # Data from LA sheet is for All publicly funded schools - recode NAs to All publicly funded schools

  mutate(school = replace_na(school, "All publicly funded schools")) %>%
  
  # Recode seed_code for LA/Scotland summary rows
  mutate(seed_code = ifelse(seed_code == "NA", la_code, seed_code)) %>%
  select(-la_code, -la_name, -school) %>%
  
  
  # Filter school list and recode names
  inner_join(school_lookup, by = c("seed_code", "school_type")) %>%

  # Select collumns to keep
  select(year, seed_code, la_name, school_name, fte_teacher_numbers, roll, ptr, average_class,school_type) %>%

  #replace remaining NA with Z
  #mutate(fte_teacher_numbers = replace_na(fte_teacher_numbers,"z"))  %>%
  #mutate(roll = replace_na(roll,"z"))  %>%
  #mutate(ptr = replace_na(ptr,"z"))  %>%
  #mutate(average_class = replace_na(average_class,"z")) 

  # Recode missing / suppressed values
  mutate(fte_teacher_numbers = recode_missing_values(fte_teacher_numbers, label = TRUE),
         roll = recode_missing_values(roll, label = TRUE),
         average_class = recode_missing_values(average_class, label = TRUE),
         ptr = recode_missing_values(ptr, label = TRUE)) %>%
  

  # Reorder columns
  select(year, seed_code, la_name, school_name, school_type, roll, fte_teacher_numbers, ptr,average_class)
 

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
  population %>% filter(school_type == "Secondary")

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
  population %>% filter(school_type == "special")

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