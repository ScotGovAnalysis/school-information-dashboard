#' @title Format Year
#'
#' @param x Character string or vector of character strings. Input values should
#' be in the format YYYY, YYYY/YY, or YY/YY. Any other formats will be coded 
#' as NA and generate a warning message. 
#' @param academic The default behaviour is to return year in academic year 
#' format. Use `academic = FALSE` to return in calendar year format.
#'
#' @return A character string or vector of character strings, where each element
#' is the academic year for the corresponding value in `x`. If 
#' `academic = FALSE`, then the calendar year is returned. 
#' 
#' Any input values that are invalid; e.g. 2020/22, or do not match the expected
#' input format; e.g. 202021, will generate an NA output value and a warning
#' message. 
#' 
#' @export
#'
#' @examples
#' format_year("2022")
#' format_year("2021/22", academic = FALSE)

format_year <- function(x, academic = TRUE) {

  # Define pipe - this ensures that the pipe operator is available for use 
  # within the function even if magrittr package is not loaded.
  `%>%` <- magrittr::`%>%`
  
  x_fmt <- 
    
    tibble::tibble(x) %>%
    
    # Separate year into two pieces; e.g. 2020/21 into 2020 and 21
    tidyr::separate(x, into = c("yr1", "yr2"), 
                    remove = FALSE, fill = "right", sep = "/") %>%
    
    dplyr::mutate(
      
      # Label input year format - calendar, academic or academic abbreviated
      format = dplyr::case_when(
        stringr::str_detect(x, "^\\d{4}$") ~ "cal",
        stringr::str_detect(x, "^\\d{4}[^a-zA-Z0-9]\\d{2}$") ~ "acad",
        stringr::str_detect(x, "^\\d{2}[^a-zA-Z0-9]\\d{2}$") ~ "acad_abbr"
      ),
      
      # Change year columns into two-digit numeric format;
      # e.g. change 2021 to 21
      dplyr::across(
        tidyselect::matches("^yr"), 
        ~ suppressWarnings(as.numeric(
          stringr::str_sub(., start = -2, end = -1))
        )),
      
      # Check that second part of year is one greater than first part
      # If this isn't true, then input year invalid and will return NA
      valid = yr2 == yr1 + 1,
      
      # Format as academic year
      acad_formatted = dplyr::case_when(
        format == "cal" ~ paste0("20", yr1 - 1, "/", yr1),
        format == "acad_abbr" & valid ~ paste0("20", yr1, "/", yr2),
        format == "acad" & valid ~ paste0("20", yr1, "/", yr2)
      ),
      
      # Format as calendar year
      cal_formatted = dplyr::case_when(
        format == "cal" ~ paste0("20", yr1),
        format %in% c("acad", "acad_abbr") & valid ~ paste0("20", yr2)
      ),
      
      # Select which formatted output to use depending on `academic` argument
      # When TRUE, return academic year, otherwise return calendar year
      out = dplyr::case_when(
        academic ~ acad_formatted,
        !academic ~ cal_formatted
      )
    )
  
  # If any of the output values are NA (due to invalid format (e.g. 202021), 
  # invalid year (e.g. 2021/25), or NA supplied), 
  # print a warning message to explain to user
  
  if(any(is.na(x_fmt$out))) {
    warning(sum(is.na(x_fmt$out)), " input value(s) are either missing or do ",
            "not adhere to the accepted year formats and will be coded as NA. ",
            "Accepted formats are:\n",
            "\U2022 Calendar year: YYYY \n",
            "\U2022 Academic year: YYYY/YY or YY/YY")
  }
  
  # Return the out column from x_fmt
  x_fmt$out
    
}


### END OF SCRIPT ###