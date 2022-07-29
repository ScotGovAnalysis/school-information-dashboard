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

school_lookup <- read_rds(here("output", run_label, "school_lookup.rds"))


### 1 - Insight data ----

## This code reads in data for each year in `year_insight` and each dataset in 
## `insight_datasets` and combines into one dataset.

insight_datasets <- c("attainment_by_deprivation", "attainment_for_all", 
                      "breadth_depth", "destinations", "literacy_numeracy")

insight <- pmap_dfr(expand.grid(insight_datasets, year_insight), 
                    ~ import_insight_data(.x, .y))


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
  select(-la_and_school, -matches("(score|level)")) %>%
  
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
    comparator = ifelse(comparator == "actual", "0", "1")
  )


### 4 - Join data, filter schools and update school names ----

attainment <- 
  
  # Combine Insight, ACEL and BGE data into one dataset
  bind_rows(insight, acel, bge) %>%
  
  # Recode missing / suppressed values; this will create both a numeric value 
  # column with all missing/suppressed values coded as NA and a character value 
  # column with all missing/suppressed values coded as z/x/c
  mutate(number_of_leavers = recode_missing_values(number_of_leavers),
         value_label = recode_missing_values(value, label = TRUE),
         value = recode_missing_values(value)) %>%
  
  # Filter school list and recode names using school_lookup
  inner_join(school_lookup, by = c("seed_code", "school_type")) %>%
  
  # Reorder columns
  select(year, seed_code, la_code, la_name, school_name, school_type,
         stage, comparator, minimum_scqf_level, minimum_number_of_awards,
         number_of_leavers, dataset, measure, value, value_label)


### 5 - Save attainment data sets ----

## Primary

primary_attainment <- 
  attainment %>% 
  filter(school_type == "Primary") %>%
  # Remove these columns as not relevant to primary data
  select(-c(minimum_scqf_level, minimum_number_of_awards, number_of_leavers))

write_rds(
  primary_attainment,
  here("output", run_label, "primary_attainment.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  primary_attainment,
  here("output", run_label, "primary_attainment.xlsx")
)


## Secondary 

secondary_attainment <- attainment %>% filter(school_type == "Secondary")

write_rds(
  secondary_attainment,
  here("output", run_label, "secondary_attainment.rds"),
  compress = "gz"
)

# Temp - save as xlsx for checking
writexl::write_xlsx(
  secondary_attainment,
  here("output", run_label, "secondary_attainment.xlsx")
)


### END OF SCRIPT ###