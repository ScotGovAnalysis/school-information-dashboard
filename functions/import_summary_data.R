#' @title Import and clean School Summary Statistics data
#' 
#' @description This function reads in the desired sheet of the Summary
#' Statistics dataset (including attendance data) and cleans variable names.
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
  
  # Check sheet_name is valid
  if(!sheet_name %in% c("SCH", "LA and SCOT", "Attendance", "Att by Stage")) {
    stop("Invalid sheet_name argument. Must be one of 'SCH', 'LA and SCOT', ",
         "'Attendance' or 'Att by Stage'.")
  }
  
  filepath <- here::here("data", 
                          "school_summary_statistics", 
                          paste0(calendar_year, 
                                 "_school_summary_statistics.xlsx"))
  
  # Check the file exists for given year and if not stop running the function
  # and print an error message
  if(!file.exists(filepath)) {
    stop("File does not exist:\n", filepath, ".")
  }
  
  dat <-
    
    # Read in data
    here::here("data", "school_summary_statistics", 
             paste0(calendar_year, "_school_summary_statistics.xlsx")) %>%
    readxl::read_excel(sheet = sheet_name, col_types = "text") %>%
    
    # Clean column names
    janitor::clean_names() %>%
    dplyr::rename(female = tidyselect::matches("^f$")) %>%
    dplyr::rename(male = tidyselect::matches("^m$")) %>%
    dplyr::rename(fte_teacher_numbers =
                       tidyselect::matches("^fte$")) %>%
    dplyr::rename(stage = tidyselect::matches("^student_stage$")) %>%
    
    # Ensure school_type is capitalised
    dplyr::mutate(dplyr::across(tidyselect::any_of("school_type"), 
                                ~ stringr::str_to_title(.)))
  
  # For school level data, calculate total # receiving free school meals;
  # this is sum of those receiving universal FSM and other FSM
  # The number not receiving FSM is calculated by subtracting this figure from
  # total school roll
  if(sheet_name == "SCH") {
    
    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(c(roll, universal_fsm, other_fsm), as.numeric),
        dplyr::across(c(universal_fsm, other_fsm), ~ tidyr::replace_na(., 0)),
        fsm = universal_fsm + other_fsm,
        no_fsm = roll - fsm
      ) %>%
      dplyr::select(-universal_fsm, -other_fsm) %>%
      dplyr::mutate(roll = as.character(roll))
    
  }
  
  # For LA and SCOT level data, remove free school meal variables
  # These figures will be calculated from school level data elsewhere
  if(sheet_name == "LA and SCOT") {
    
    dat <- dat %>% dplyr::select(-tidyselect::any_of(c("fsm", "no_fsm")))
    
  }
  
  dat
  
}


### END OF SCRIPT ###
