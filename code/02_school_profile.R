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
  select(seed_code, matches("^address_line[12]$"), 
         post_code, email, phone_number, website_address, 
         matches("^(primary|secondary|special)_department$"),
         denomination) %>%
  mutate(seed_code = as.character(seed_code)) %>%
  rename(postcode = post_code,
         website = website_address) %>%
  
  # Combine address into one column
  mutate(address = paste(address_line1, address_line2, postcode, 
                         sep = ", "),
         address = str_remove_all(address, "NA, ")) %>%
  select(-address_line1, -address_line2) %>%
  
  # Create school_type column (Primary, Secondary, Special)
  pivot_longer(
    cols = matches("^(primary|secondary|special)_department$"),
    names_to = "school_type",
    values_to = "flag"
  ) %>%
  filter(flag == "Yes") %>%
  select(-flag) %>%
  mutate(school_type = str_to_title(str_remove(school_type, "_department"))) %>%
  
  # Filter school list and recode names using school_lookup
  inner_join(school_lookup, by = c("seed_code", "school_type")) %>%
  
  # Add rows for Local Authorities and Scotland
  bind_rows(
    school_lookup %>% filter(seed_code == la_code)
  ) %>%
  
  # Add LA Website
  left_join(here("lookups", "la_websites.xlsx") %>% 
              read_xlsx(col_type = "text") %>% 
              select(la_code, la_website, la_postcode = postcode),
            by = "la_code") %>%
  
  # Merge school and LA postcodes
  mutate(postcode = ifelse(seed_code == la_code, la_postcode, postcode)) %>%
  select(-la_postcode) %>%

  # Reorder columns
  select(seed_code, la_code, la_name, school_name, school_type,
         denomination, address, phone_number, email, website, la_website, 
         postcode)


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


## Aggregate estate data to calculate percentage of school in condition A or B
## by Local Authority and Scotland

estate_la_scot <-
  
  estate %>%
  
  # Join school lookup to get Local Authority for each school
  inner_join(school_lookup %>% select(seed_code, la_code, school_type),
             by = c("seed_code", "school_type")) %>%
  
  # Aggregate data to Local Authority level
  group_by(school_type, la_code) %>%
  summarise(target_met = sum(condition %in% c("A", "B")),
            n = n(),
            .groups = "drop") %>%
  rename(seed_code = la_code) %>%
  
  # Add summary rows for Scotland
  bind_rows(
    group_by(., school_type) %>%
      summarise(target_met = sum(target_met),
                n = sum(n),
                .groups = "drop") %>%
      mutate(seed_code = "0")
  ) %>%
  
  # Calculate percentage of schools meeting PE target
  mutate(condition = paste0(round_half_up(target_met / n * 100, 1), "%")) %>%
  select(-target_met, -n)

# Add Local Authority and Scotland level data to schools level data
estate %<>% bind_rows(estate_la_scot)


### 3 - Healthy Living Data ----

## Code to pull in the Healthy Living Survey data. This data includes a flag to
## indicate whether a school is meeting the PE target. 

healthy_living <-
  
  # Read in school level data
  here("data", "healthy_living_survey", 
       paste0(year_healthy_living, "_healthy_living_survey.xlsx")) %>%
  read_excel(sheet = "Table 6", col_types = "text") %>%
  
  # Clean names and remove columns not needed
  clean_names() %>%
  select(seed_code, school_type, matches("pe_provision$")) %>%
  
  # Convert PE provision columns to numeric (recode # to NA)
  mutate(across(matches("pe_provision$"), ~ as.numeric(na_if(., "#")))) %>%

  # Derive Target Met / Not Met for each school
  mutate(pe_target = case_when(
    school_type == "Primary" & primary_pe_provision == 1 ~ "Target Met",
    school_type == "Secondary" & 
      reduce(select(., matches("^s[1-4]_pe_provision")), `+`) == 4 ~ 
      "Target Met",
    TRUE ~ "Target Not Met"
  )) %>%
  select(-matches("pe_provision$"))


## Aggregate data to get percentage of schools meeting target in each Local 
## Authority and Scotland

hl_la_scot <-
  
  healthy_living %>%
  
  # Join school lookup to get Local Authority for each school
  inner_join(school_lookup %>% select(seed_code, la_code, school_type),
             by = c("seed_code", "school_type")) %>%
  
  # Aggregate data to Local Authority level
  group_by(school_type, la_code) %>%
  summarise(target_met = sum(pe_target == "Target Met"),
            n = n(),
            .groups = "drop") %>%
  rename(seed_code = la_code) %>%
  
  # Add summary rows for Scotland
  bind_rows(
    group_by(., school_type) %>%
      summarise(target_met = sum(target_met),
                n = sum(n),
                .groups = "drop") %>%
      mutate(seed_code = "0")
  ) %>%
  
  # Calculate percentage of schools meeting PE target
  mutate(pe_target = paste0(round_half_up(target_met / n * 100, 1), "%")) %>%
  select(-target_met, -n)

# Add Local Authority and Scotland level data to schools level data
healthy_living %<>% bind_rows(hl_la_scot)
    

### END OF SCRIPT ###