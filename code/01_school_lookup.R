#########################################################################
# Name of file - 01_school_lookup.R
# Data release - School Information Dashboard
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Creates a lookup file containing open schools (as defined
# by school_summary_statistics) and their correct names and local 
# authorities (as defined by school_contact_details). This is then used 
# to filter and standardise school/LA names across all output data files.
#########################################################################


### 0 - Setup ----

## Run setup script where years for each dataset are defined and
## packages/functions are loaded.

source(here::here("code", "00_setup.R"))


### 1 - School Contact Data ----

## Code to pull in the school contact dataset. This file contains contact 
## details for all schools and is used as a reference for how schools should
## be named across all output data files.

contacts <- 
  
  here("data", "school_contact_details", 
       paste0(year_contacts, "_school_contact_details.xlsx")) %>%
  read_excel(sheet = "Open Schools") %>%
  
  # Clean names and remove columns not needed
  clean_names() %>%
  select(seed_code, la_name, school_name,
         matches("^(primary|secondary|special)_department$")) %>%
  mutate(seed_code = as.character(seed_code)) %>%
  
  # Create school_type column (Primary, Secondary, Special)
  pivot_longer(
    cols = matches("^(primary|secondary|special)_department$"),
    names_to = "school_type",
    values_to = "flag"
  ) %>%
  filter(flag == "Yes") %>%
  select(-flag) %>%
  mutate(school_type = str_to_title(str_remove(school_type, "_department"))) %>%
  
  # Match on Local Authority code
  left_join(
    here("lookups", "la_websites.xlsx") %>% 
      read_excel() %>%
      select(-la_website) %>%
      mutate(la_code = as.character(la_code)), 
    by = "la_name"
  )

## Check all rows have a match in LA lookup.
## This is matched by Local Authority name and so exact text must be the same
## in both contact data and LA websites lookup.

if(any(is.na(contacts$la_code))) {
  abort(paste(sum(is.na(contacts$la_code)), "rows do not have a match in",
              "`lookups/la_websites.xlsx`."))
}


### 2 - School Summary Statistics ----

## Code to pull in the school summary statistics dataset. This file contains
## various summary statistics for each school and is used as a reference for
## which schools should be included in the output data files. Only the latest
## year of data is required for the schools_lookup data file.

summary_schools <- 
  import_summary_data("SCH", max(year_summary)) %>%
  select(seed_code, school_type)


### 3 - Create lookup file ----

## This section creates a lookup file containing all schools to be included
## in output data files, with their correct names and local authorities.

school_lookup <-
  
  # Start with list of seed codes and school types for schools to be included
  summary_schools %>%
  
  # Match on school name and local authority from contact data
  left_join(
    contacts %>% select(seed_code, school_type, school_name, la_code, la_name),
    by = c("seed_code", "school_type")
  ) %>%
  
  # Remove any schools with no match in contact data
  # Due to different timings of data release, some schools in summary data 
  # may have since closed. 
  filter(!is.na(school_name))

school_lookup %<>%
  
  # Add rows for Local Authorities and Scotland
  bind_rows(
    school_lookup %>%
      group_by(school_type, la_code, la_name) %>%
      summarise(seed_code = first(la_code),
                school_name = "All publicly funded schools",
                .groups = "drop"),
    school_lookup %>%
      group_by(school_type) %>%
      summarise(seed_code = "0",
                school_name = "All publicly funded schools",
                la_code = "0",
                la_name = "Scotland",
                .groups = "drop")
  )

# Save file
write_rds(
  school_lookup,
  here("output", run_label, "school_lookup.rds"),
  compress = "gz"
)


### END OF SCRIPT ###