#' @title Recode Missing Values
#' 
#' @description This function recodes missing or suppressed values. Values are 
#' either recoded as NA (if `label = FALSE`) or as the relevant character based 
#' on the following guidance: 
#' https://gss.civilservice.gov.uk/policy-store/symbols-in-tables-definitions-and-help/
#' In summary, values are recoded as follows:
#' Not Applicable = 'z'
#' Not Available = 'x'
#' Suppressed = 'c'
#' 
#' @param x Vector of values to recode.
#' @param label The default behaviour is to recode both missing and suppressed 
#' values coded as NA. If `label = TRUE`, values are recoded to a character
#' as detailed above.
#' @param label_digits A numeric value to define the number of digits to round 
#' `value_label` to. This argument is ignored if `label = FALSE`.
#'
#' @return When `label = FALSE`, returns numeric vector. When `label = TRUE`, 
#' returns character vector.
#' 
#' @export
#'
#' @examples
#' recode_missing_values(c("29", NA, "z", "*", 2003))
#' recode_missing_values(c("29", NA, "z", "*", 2003), label = TRUE)

recode_missing_values <- function(x, label = FALSE, label_digits = 1) {
  
  # Define how values are categorised
  not_applicable <- c("NA", "#", "Z", "z")
  not_available <- c("X", "x")
  suppressed <- c("*", "C", "c")
  
  if(label) {
    
    # Recode values as character vector with missing/suppressed values recoded
    # as categorised above 
    tibble::as_tibble(x = x) %>%
      
      # Add label for missing/suppressed values
      dplyr::mutate(
        recode = dplyr::case_when(
          x %in% not_applicable | is.na(x) ~ "z",
          x %in% not_available             ~ "x",
          x %in% suppressed                ~ "c",
          TRUE                             ~ NA_character_
        ),
        
        # Add separate column with non-missing/suppressed values
        value = ifelse(is.na(recode), x, NA_character_),
        
        # Round value column
        round_value = ifelse(
          is.na(recode),
          janitor::round_half_up(as.numeric(value), label_digits),
          NA_character_
        ),
        
        # Merge missing/suppressed codes with values
        value_label = ifelse(is.na(recode), as.character(round_value), recode)
      ) %>%
      
      # Extract value_label column as vector
      pull(value_label)
    
  } else {
    
    # Recode values as numeric vector with missing/suppressed values 
    # recoded as NA  
    as.numeric(
      replace(x, 
              x %in% c(not_applicable, not_available, suppressed) | is.na(x), 
              NA)
    )
    
  }
  
}


### END OF SCRIPT ###