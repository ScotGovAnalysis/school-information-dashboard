#########################################################################
# Name of file - 05_attainment.R
# Data release - School Information Dashboard
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Creates two data sets (primary, secondary) containing 
# attainment trend data.
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


### 1 - Insight data ----

## This code reads in data for each year in `year_insight` and each dataset in 
## `insight_datasets` and combines into one dataset.

insight_datasets <- c("attainment_by_deprivation", "attainment_for_all", 
                      "breadth_depth", "destinations", "literacy_numeracy")

insight <- 
  
  pmap_dfr(expand.grid(insight_datasets, year_insight), 
                    ~ import_insight_data(.x, .y)) %>%
  
  # Recode missing values and add value label
  mutate(
    number_of_leavers = recode_missing_values(number_of_leavers),
    value_label = recode_missing_values(
      value, 
      label = TRUE,
      label_perc = ifelse(!str_detect(measure, "average"), TRUE, FALSE)
    ),
    value = recode_missing_values(value)
  )
  

### 2 - ACEL data ----

## Code to pull in the ACEL data. This dataset contains data on percentage
## of pupils meeting CfE level relevant to their stage for Listening & Talking, 
## Numeracy, Reading and Writing.

acel <- 
  here("data", "acel", paste0(year_acel, "_acel_data.xlsx")) %>%
  read_excel(col_types = "text") %>%
  
  # Clean names and ensure they follow a consistent naming convention
  # Remove LA and School names - these will be added later from school_lookup
  clean_names() %>%
  rename_with(~ "seed_code", matches("seedcode")) %>%
  rename(value = percentage) %>%
  select(-local_authority, -school_name) %>%
  
  # Remove duplicate rows
  distinct() %>%
  
  # Derive school type from stage and recode some other variables
  mutate(
    school_type = case_when(
      str_detect(stage, "^P") ~ "Primary",
      str_detect(stage, "^S") ~ "Secondary"
    ),
    measure = paste(measure, "- %", str_to_title(variable)),
    dataset = "acel",
    year = format_year(year, academic = TRUE)
  ) %>%
  
  # Recode missing values and add value label
  mutate(
    value_label = recode_missing_values(value, label = TRUE, label_perc = TRUE),
    value = recode_missing_values(value)
  ) %>%
 
  # Apply percentage bandings to school level data - meeting level
  # NOTE: School level "meeting level" data in the ACEL file has already been rounded up to the
  # nearest 10 (value to be plotted in charts) and so percentage bands are 
  # applied on this value minus 10.
  # e.g. Value in data file, 90; True value, 80; Percentage band, 80-90%
  mutate(
    value_label = case_when(
      nchar(seed_code) > 3 & !value_label %in% c("z", "x", "c") & (variable == "Meeting level"| variable == "Meeting Level") ~
        percentage_band(value - 10),
      TRUE ~ value_label 
    ) 
  ) %>%
  
  # Apply percentage bandings to school level data - not meeting level
  # NOTE: School level "not meeting level" data in the ACEL file has already been rounded down to the
  # nearest 10 (value to be plotted in charts) and so percentage bands are 
  # applied on this value directly.
  # e.g. Value in data file, 10; True value in Percentage band, 10-20%
  mutate(
    value_label = case_when(
      nchar(seed_code) > 3 & !value_label %in% c("z", "x", "c") & (variable == "Not meeting level"| variable == "Not Meeting Level") ~
        percentage_band(value),
      TRUE ~ value_label 
    ) 
  ) %>%
  select(-variable) 
  

### 3 - BGE data ----

## Code to pull in the BGE data. This dataset contains data on average CfE
## level achieved for Listening & Talking, Literacy, Numeracy, Reading and 
## Writing.

bge <- 
  here("data", "bge_tool", paste0(year_bge, "_bge_tool.xlsx")) %>%
  read_excel(col_types = "text") %>%
  
  # Clean names and ensure they follow a consistent naming convention
  # Remove LA and School names - these will be added later from school_lookup
  clean_names() %>%
  rename_with(~ "seed_code", matches("seedcode")) %>%
  select(-la_and_school, -matches("score|level")) %>%
  
  # Keep primary school stages only and recode some other variables
  filter(str_starts(stage, "P")) %>%
  mutate(
    dataset = "bge",
    school_type = "Primary",
    year = format_year(year, academic = TRUE)
  ) %>%
  
  # Suppress comparator values if pupil numbers 20 or less
  mutate(
    across(c(actual, comparator),
           ~ ifelse(as.numeric(pupil_numbers) <= 20, "c", .))
  ) %>%
  select(-pupil_numbers) %>%
  
  # Restructure data to long format to get column with comparator flag (0 for 
  # actual value and 1 for comparator) and column of corresponding values
  pivot_longer(cols = c("actual", "comparator"),
               names_to = "comparator",
               values_to = "value") %>%
  mutate(
    comparator = ifelse(comparator == "actual", "0", "1"),
    value_label = recode_missing_values(value, label = TRUE),
    value = recode_missing_values(value)
  ) %>%
  
  # Subtract 1 from values
  mutate(
    value = ifelse(!value_label %in% c("z", "x", "c"),
                   value - 1,
                   value),
    value_label = ifelse(!value_label %in% c("z", "x", "c"),
                         recode_missing_values(value, label = TRUE),
                         value_label)
  )


### 4 - Join data, filter schools and update school names ----

attainment <- 
  
  # Combine Insight, ACEL and BGE data into one dataset
  bind_rows(insight, acel, bge) %>%
  
  # Add chart_label to show suppressed values on bar chart
  mutate(chart_label = ifelse(value_label %in% c("z", "c", "x"),
                              value_label,
                              "")) %>%
  
  # Filter school list and recode names using school_lookup
  inner_join(school_lookup, by = c("seed_code", "school_type")) %>%
  
  # Reorder columns
  select(year, seed_code, la_code, la_name, school_name, school_type,
         stage, comparator, minimum_scqf_level, minimum_number_of_awards,
         number_of_leavers, dataset, measure, value, value_label, chart_label)


### 5 - Save attainment data sets ----

## Primary

primary_attainment <- 
  attainment %>% 
  filter(school_type == "Primary") %>%
  # Remove these columns as not relevant to primary data
  select(-c(minimum_scqf_level, minimum_number_of_awards, number_of_leavers))

write_rds(
  primary_attainment,
  here("app", "primary_data", run_label, "primary_attainment.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  primary_attainment,
  here("app", "primary_data", run_label, "primary_attainment.xlsx")
)


## Secondary 

secondary_attainment <- attainment %>% filter(school_type == "Secondary")

write_rds(
  secondary_attainment,
  here("app", "secondary_data", run_label, "secondary_attainment.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  secondary_attainment,
  here("app", "secondary_data", run_label, "secondary_attainment.xlsx")
)


### END OF SCRIPT ###