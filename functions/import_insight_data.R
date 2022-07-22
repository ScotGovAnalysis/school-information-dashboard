#' @title Import and clean Insight data
#' 
#' @description This function reads in the desired Insight dataset, cleans 
#' variable names and restructures the data into long format, with a column
#' containing measure names and a column containing values.
#'
#' @param dataset Name of dataset to import
#' @param calendar_year Year of dataset to import in calendar year format; 
#' e.g. 2020.
#'
#' @return Tibble of cleaned data for given dataset and year.
#' 
#' @export
#'
#' @examples
#' import_insight_data("attainment_for_all", 2020)

import_insight_data <- function(dataset, calendar_year) {
  
  # Source format_year function
  source(here::here("functions", "format_year.R"))
  
  # Define pipe - this ensures that the pipe operator is available for use 
  # within the function even if magrittr package is not loaded.
  `%>%` <- magrittr::`%>%`
  
  # Check that dataset argument matches one of available datasets
  valid_datasets <- c("attainment_by_deprivation", "attainment_for_all", 
                      "breadth_depth", "destinations", "literacy_numeracy")
  if(!dataset %in% valid_datasets) {
    stop("`", dataset, "` is not a valid Insight dataset. Valid datasets ",
         "are:\n", paste0("\U0009\U2022 ", valid_datasets, collapse = "\n"))
  }
  
  # Construct filepath for data required
  filepath <- here::here("data", 
                          dataset, 
                          paste0(calendar_year, "_", dataset, ".xlsx"))
  
  # Check the file exists for given dataset and year and if not stop running 
  # the function and print an error message  
  if(!file.exists(filepath)) {
    stop("File does not exist:\n", filepath, ".")
  }
  
  dat <- 
    
    # Read in data and clean column names
    here::here("data", dataset, 
               paste0(calendar_year, "_", dataset, ".xlsx")) %>%
    readxl::read_excel(col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::rename_with(~ "seed_code", 
                       tidyselect::matches("^see(d)?code$")) %>%
    
    # Remove LA and School names - these will be added later from school_lookup
    dplyr::select(-any_of(c("la_name", "school")))
  
  # Vector of expected non-measure variable names
  names <- c("year", "seed_code", "comparator", "number_of_leavers")
  
  # For breadth and depth data, add two extra columns that should be present
  if(dataset == "breadth_depth") {names <- c(names, 
                                             "minimum_scqf_level",
                                             "minimum_number_of_awards")}
  
  # Check that all non-measure variables as defined above exist in data and 
  # if not, stop running the function and print an error
  if(any(!names %in% names(dat))) {
    stop("Dataset must contain all of the following columns: ",
         paste(names, collapse = ", "), ".")
  }
  
  dat %>%
    
    # Some NA values are not picked up when the datafile is read and are coded
    # as a character value of "NA". Recode these to true NA value.
    dplyr::mutate(dplyr::across(tidyselect::all_of(names), 
                                ~ ifelse(. == "NA", NA_character_, .))) %>%
    
    # Restructure data to long format to get column of various measure names 
    # and column of corresponding values
    tidyr::pivot_longer(cols = !tidyselect::all_of(names), 
                        names_to = "measure", 
                        values_to = "value") %>%
    
    # Add extra variables required
    dplyr::mutate(
      year = format_year(calendar_year, academic = TRUE),
      dataset = as.character(dataset),
      school_type = "Secondary"
    )
  
}


### END OF SCRIPT ###