#' @title Import and clean School Summary Statistics data
#' 
#' @description This function reads in the desired sheet of the Summary
#' Statistics dataset and cleans variable names.
#'
#' @param sheet_name Name of data sheet to import
#' @param calendar_year Year of dataset to import in calendar year format; 
#' e.g. 2020.
#'
#' @return Tibble of cleaned data for given dataset and year.
#' 
#' @export
#'
#' @examples
#' import_summary_data("LA and SCOT", 2021)
#' import_summary_data("LA and SCOT", "timeseries")

import_summary_data <- function(sheet_name, calendar_year) {
  
  # Define pipe - this ensures that the pipe operator is available for use 
  # within the function even if magrittr package is not loaded.
  `%>%` <- magrittr::`%>%`
  
  filepath <- here::here("data", 
                          "school_summary_statistics", 
                          paste0(calendar_year, 
                                 "_school_summary_statistics.xlsx"))
  
  # Check the file exists for given year and if not stop running the function
  # and print an error message
  if(!file.exists(filepath)) {
    stop("File does not exist:\n", filepath, ".")
  }
  
  # Read in data
  here::here("data", "school_summary_statistics", 
             paste0(calendar_year, "_school_summary_statistics.xlsx")) %>%
    readxl::read_excel(sheet = sheet_name, col_types = "text") %>%
    
    # Clean column names
    janitor::clean_names() %>%
    dplyr::rename_with(~ "female", tidyselect::matches("^f$")) %>%
    dplyr::rename_with(~ "male", tidyselect::matches("^m$")) %>%
    dplyr::rename_with(~ "fte_teacher_numbers", 
                       tidyselect::matches("^fte$")) %>%
    dplyr::rename_with(~ "fsm", tidyselect::matches("^universal_fsm$")) %>%
    dplyr::rename_with(~ "no_fsm", tidyselect::matches("^other_fsm$")) %>%
    
    # Ensure school_type is capitalised
    dplyr::mutate(across(any_of("school_type"), ~ stringr::str_to_title(.))) %>%
    
    # For LA and Scotland rows, use Local Authority code in place of Seed Code
    mutate(seed_code = ifelse(is.na(seed_code) | seed_code == "NA" &
                                "la_code" %in% names(.), 
                              la_code, 
                              seed_code))
  
}


### END OF SCRIPT ###