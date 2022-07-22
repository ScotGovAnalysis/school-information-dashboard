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
#'
#' @return When `label = FALSE`, returns numeric vector. When `label = TRUE`, 
#' returns character vector.
#' 
#' @export
#'
#' @examples
#' recode_missing_values(c("29", NA, "z", "*", 2003))
#' recode_missing_values(c("29", NA, "z", "*", 2003), label = TRUE)

recode_missing_values <- function(x, label = FALSE) {
  
  # Define how values are categorised
  not_applicable <- c("NA", "#", "Z", "z")
  not_available <- c("X", "x")
  suppressed <- c("*", "C", "c")
  
  # Recode values as character vector with missing/suppressed values recoded
  # as categorised above 
  value_label <- dplyr::case_when(
    x %in% not_applicable | is.na(x) ~ "z",
    x %in% not_available             ~ "x",
    x %in% suppressed                ~ "c",
    TRUE                             ~ as.character(x)
  )
  
  # Recode values as numeric vector with missing/suppressed values 
  # recoded as NA  
  value <- as.numeric(
    replace(x, 
            x %in% c(not_applicable, not_available, suppressed) | is.na(x), 
            NA)
  )
  
  # Return value_label or value depending on whether label argument is T/F.
  if(label) {value_label} else {value}
  
}


### END OF SCRIPT ###