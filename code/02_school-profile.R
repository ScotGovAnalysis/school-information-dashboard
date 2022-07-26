#########################################################################
# Name of file - 02_school_profile.R
# Data release - School Information Dashboard
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Creates three data sets (primary, secondary, special) 
# containing school profile data for latest year.
#########################################################################


### 0 - Setup ----

## Run setup script where years for each dataset are defined and
## packages/functions are loaded.

source(here::here("code", "00_setup.R"))

## Read in school lookup containing definitive list of schools to be 
## included and their correct names.

school_lookup <- read_rds(here("output", run_label, "school_lookup.rds"))


### 1 - School Contact Data ----

## Code to pull in the school contact dataset. This file contains contact 
## details for all schools.

contacts <- 
  
  here("data", "school_contact_details", 
       paste0(year_contacts, "_school_contact_details.xlsx")) %>%
  read_excel(sheet = "Open Schools") %>%
  
  # Clean names and remove columns not needed
  clean_names() %>%
  select(seed_code, centre_type, la_name, school_name,
         matches("^address_line[12]$"), 
         post_code, email, phone_number, website_address, 
         matches("^(primary|secondary|special)_department$"),
         denomination) %>%
  mutate(seed_code = as.character(seed_code)) %>%
  
  # Combine address into one column
  mutate(address = paste(address_line1, address_line2, post_code, 
                         sep = ", "),
         address = str_remove_all(address, "NA, ")) %>%
  select(-address_line1, -address_line2, -post_code) %>%
  
  # Create school_type column (Primary, Secondary, Special)
  pivot_longer(
    cols = matches("^(primary|secondary|special)_department$"),
    names_to = "school_type",
    values_to = "flag"
  ) %>%
  filter(flag == "Yes") %>%
  select(-flag) %>%
  mutate(school_type = str_to_title(str_remove(school_type, "_department")))


### 2 - School Estate Statistics ----

## Code to pull in the School estate statistics. This data includes a 
## condition rating for each school.

estate <-
  here("data", "school_estate_statistics", 
       paste0(year_estate, "_school_estate_statistics.xlsx")) %>%
  read_excel(sheet = "Table 8", col_types = "text") %>%
  
  # Clean names and remove columns not needed
  clean_names() %>%
  rename(seed_code = seedcode) %>%
  select(seed_code, school_type, school_name, condition) %>%
  
  # Recode missing values as not recorded
  replace_na(list(condition = "not_recorded"))


## Remove duplicates of seed code and school type.
## There are some instances where a school has more than one campus.
## This section of code keeps the campus which corresponds to 
## the school address included in the contact dataset, and removes the other.

estate %<>%
  mutate(remove = case_when(
    seed_code == "1004760" & school_type == "Primary" & 
      str_detect(school_name, "Upper Campus") ~ 1,
    seed_code == "1004905" & school_type == "Primary" & 
      str_detect(school_name, "West Loan") ~ 1,
    seed_code == "5553024" & school_type == "Primary" & 
      str_detect(school_name, "Lochend Campus") ~ 1,
    TRUE ~ 0
  )) %>%
  filter(remove == 0) %>%
  select(-remove)


## Check if there are any other duplicates of seed code and school type
## If an error is produced, add details of row to remove to the code above
## and rerun.

dupes <- get_dupes(estate, seed_code, school_type)
n_dupes <- n_distinct(dupes$seed_code, dupes$school_type)

if(nrow(dupes) > 0) {
  abort(c(
    paste("There are", n_dupes, "rows in the estate data with the same",
          "seed code and school type."),
    i = paste("These must be accounted for in the code to determine which",
              "of the duplicate rows should be kept."),
    i = "To view these duplicates, run `print(dupes)` in the R console."
  ))
}

# Once there are no duplicates remaining, remove school name
estate %<>% select(-school_name)


### END OF SCRIPT ###