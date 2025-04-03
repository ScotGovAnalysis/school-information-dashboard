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

school_lookup <- read_rds(
  here("lookups", "school_lookup", paste0(run_label, "_school_lookup.rds"))
)

## Local Authorities with less than 5 schools
## Extra disclosure control will be applied to LA level data

small_la <- 
  school_lookup %>%
  filter(seed_code != la_code) %>%
  group_by(school_type, la_code) %>%
  summarise(n_schools = n(), small_la = 1, .groups = "drop") %>%
  filter(n_schools < 5) %>%
  select(-n_schools)


### 1 - School Summary data ----

## Read in school level summary data

population_school <- 
  map_dfr(
    c(year_summary, "timeseries"),
    ~ import_summary_data("SCH", .x)
  )

## Calculate Local Authority and Scotland level free school meals data

fsm_la <- 
  population_school %>%
  filter(year == max(year_summary)) %>%
  group_by(la_code, school_type) %>%
  summarise(across(contains("fsm"), 
                   ~ sum(as.numeric(.), na.rm = TRUE)),
            .groups = "drop") %>%
  bind_rows(
    group_by(., school_type) %>%
      summarise(
        la_code = "0",
        across(contains("fsm"), sum),
        .groups = "drop"
      )
  ) %>%
  mutate(year = as.character(max(year_summary)))

## Combine school level data with local authority and Scotland level data

population <- 
  
  # Read in Local Authority and Scotland level data
  map_dfr(
    c(year_summary, "timeseries"),
    ~ import_summary_data("LA and SCOT", .x)
  ) %>%
  
  # Join free school meals data calculated from school level data
  left_join(fsm_la, by = c("year", "school_type", "la_code")) %>%

  # Add school level data
  bind_rows(population_school) %>%
  mutate(across(contains("fsm"), as.character)) %>%

  # Recode seed_code for LA/Scotland summary rows
  mutate(seed_code = ifelse(is.na(seed_code) | seed_code == "NA", 
                            la_code, 
                            seed_code)) %>%
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
    !(str_detect(measure_category, "stage") &
        tolower(school_type) != word(measure_category, 1, sep = "_"))
  ) %>%
  mutate(measure_category = ifelse(str_detect(measure_category, "stage"),
                                   "stage",
                                   measure_category)) %>%
  
  # Recode missing / suppressed values
  mutate(
    # All NAs in data represent zeros (except FSM)
    value = ifelse(measure_category != "free_school_meals",
                   replace_na(value, "0"),
                   value),
    value_label = recode_missing_values(value, label = TRUE),
    value = recode_missing_values(value)
  ) %>%
  
  # Add roll as seperate column to allow percentage calculations
  mutate(roll = ifelse(measure == "Pupil Numbers", value, NA)) %>%
  group_by(year, seed_code, school_type) %>%
  # Some roll values are missing or suppressed, code these as NA
  mutate(roll = ifelse(all(is.na(roll)), NA, max(roll, na.rm = TRUE))) %>%
  ungroup()
  

## Apply suppression rules (to both value and value_label)
## Stage Meaures - 
##    Suppress if less than 5, otherwise percentage of roll
## Measures other than Stage and Trend -
##    Suppress if roll <= 20, otherwise percentage of roll
##    If school level data or LA with less than 5 schools, 
##    convert to percentage banding

population %<>% 
  
  mutate(
    value_label = case_when(
      str_detect(measure_category, "stage") & value < 5 ~ "c",
      !str_detect(measure_category, "(stage|trend)") & roll <= 20 ~ "c",
      measure_category != "trend" ~ 
        recode_missing_values(value / roll * 100, label = TRUE, label_perc = TRUE),
      TRUE ~ value_label
    ),
    value = case_when(
      str_detect(measure_category, "stage") & value < 5 ~ 0,
      !str_detect(measure_category, "(stage|trend)") & roll <= 20 ~ 0,
      measure_category != "trend" ~ value / roll * 100,
      TRUE ~ value
    )
  ) %>%
  
  # Add flag for LA with less than five schools
  left_join(small_la, by = c("school_type", "seed_code" = "la_code")) %>%
  
  # Apply percentage bandings to non stage and trend measures at school level
  mutate(
    value_label = case_when(
      !str_detect(measure_category, "(stage|trend)") & 
        (nchar(seed_code) > 3 | small_la == 1) & 
        !value_label %in% c("z", "x", "c") ~
        percentage_band(value),
      TRUE ~ value_label
    ),
    value = case_when(
      !str_detect(measure_category, "(stage|trend)") & 
        (nchar(seed_code) > 3 | small_la == 1) & 
        !value_label %in% c("z", "x", "c") ~
        percentage_band(value, numeric = TRUE),
      TRUE ~ value
    )
  ) %>%
  
  # Add chart_label to show suppressed values on bar chart
  mutate(chart_label = ifelse(value_label %in% c("z", "c", "x"),
                              value_label,
                              "")) %>%
  
  # Filter school list and recode names
  inner_join(school_lookup, by = c("seed_code", "school_type")) %>%

  # Reorder columns
  select(year, seed_code, la_code, la_name, school_name, school_type, 
         measure_category, measure, value, value_label, chart_label)
 


### 2 - Save population data sets ----

# Save primary school file

primary_population <- 
  population %>% 
  filter(school_type == "Primary") %>%
  mutate(measure = droplevels(measure))

write_rds(
  primary_population,
  here("app", "primary_data", run_label, "primary_population.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  primary_population,
  here("app", "primary_data", run_label, "primary_population.xlsx")
)


# Save secondary school file

secondary_population <- 
  population %>% 
  filter(school_type == "Secondary") %>%
  mutate(measure = droplevels(measure))

write_rds(
  secondary_population,
  here("app", "secondary_data", run_label, "secondary_population.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  secondary_population,
  here("app", "secondary_data", run_label, "secondary_population.xlsx")
)

# Save special school file

special_population <- 
  population %>% 
  filter(school_type == "Special") %>%
  mutate(measure = droplevels(measure))

write_rds(
  special_population,
  here("app", "special_data", run_label, "special_population.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  special_population,
  here("app", "special_data", run_label, "special_population.xlsx")
)


### END OF SCRIPT ###