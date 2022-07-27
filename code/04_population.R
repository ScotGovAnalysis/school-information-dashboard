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
  
  # Add measure category
  mutate(measure_category = case_when(
    measure %in% c("roll", "fte_teacher_numbers", "ptr", "average_class") ~ 
      "trend",
    str_detect(measure, "^p[1-7]$") ~ "primary_stage",
    str_detect(measure, "^s[1-6]$") ~ "secondary_stage",
    str_detect(measure, "^sp_(13_15|16plus|9_12|to_8)$") ~ "special_stage",
    str_detect(measure, "^(fe)?male$") ~ "sex",
    str_detect(measure, "^simd([1-5]|_unknown)") ~ "deprivation",
    str_detect(measure, "fsm") ~ "free_school_meals",
    str_detect(measure, "asn") ~ "additional_support_needs",
    str_detect(measure, "eal") ~ "english_additional_language",
    str_detect(measure, "gaelic") ~ "gaelic",
    str_detect(measure, "(urban|rural|small_town)") ~ "urban_rural",
    str_detect(measure, "(white|ethnic)") ~ "ethnicity"
  ), .before = measure) %>%
  
  # Remove stage rows not applicable for school type
  filter(
    !(str_ends(measure_category, "_stage") &
        tolower(school_type) != word(measure_category, 1, sep = "_"))
  ) %>%
  
  # Recode measure column
  mutate(measure = case_when(
    str_detect(measure_category, "^(primary|secondary)_stage") ~
      toupper(measure),
    str_detect(measure, "^simd[1-5]$") ~ 
      paste0("SIMD Q", str_sub(measure, -1)),
    str_detect(measure, "^(asn|eal|fsm)$") ~ toupper(measure),
    str_detect(measure, "^no_(asn|eal|fsm)$") ~ 
      paste("No", toupper(word(measure, 2, sep = "_"))),
    measure_category == "sex" ~ str_to_title(measure),
    measure == "urban_rural_missing" ~ "Urban Rural Not Known",
    measure_category == "urban_rural" ~ 
      str_replace_all(measure, "_", " ") %>% str_to_title(),
    TRUE ~ 
      recode(measure,
             roll = "Pupil Numbers",
             average_class = "Average Class",
             fte_teacher_numbers = "Teacher Numbers (FTE)",
             ptr = "Pupil Teacher Ratio",
             sp_to_8 = "0-8",
             sp_9_12 = "9-12",
             sp_13_15 = "13-15",
             sp_16plus = "16+",
             simd_unknown = "SIMD Unknown",
             gaelic = "Taught in Gaelic",
             no_gaelic = "Not Taught in Gaelic",
             white_british = "White UK",
             white_other = "White Other",
             min_ethnic = "Ethnic Minority",
             unknown_ethnicity = "Ethnicity Not Known"
      )
  )) %>%
  
  # Recode missing / suppressed values
  mutate(
    value_label = recode_missing_values(value, label = TRUE),
    value = recode_missing_values(value)
  ) %>%
  
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