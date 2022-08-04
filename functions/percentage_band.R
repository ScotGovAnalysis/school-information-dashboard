#' @title Apply percentage bandings
#'
#' @param x Numeric value or vector of numeric values
#' @param numeric Logical; indicates whether the return value is a character 
#' value or a numeric value. 
#'
#' @return The default behaviour is to return a character value/vector of
#' percentage bandings in the format; e.g. 10-20%. Note that percentage bandings
#' are closed on the left and open on the right; i.e. 10-20% includes values 
#' greater than or equal to 10 and less than 20.
#' 
#' If `numeric = TRUE`, a numeric value is returned that represents the 
#' corresponding percentage banding. For example, "10-20%" is represented by 2.
#' This return format is used to plot the percentage bandings on a chart.
#' 
#' @export
#'
#' @examples
#' percentage_band(52.4)
#' percentage_band(c(11, 58, 32), numeric = TRUE)


percentage_band <- function(x, numeric = FALSE) {
  
  # Define breaks for bandings; from 0 to 100 in increments of 10
  breaks <- seq(0, 100, 10)
  
  # Define labels for each break to be in format XX-XX%
  # Recode label for last value; change "90-100%" to "90+%"
  labels <- paste0(head(breaks, -1), "-", tail(breaks, -1), "%")
  labels <- stringr::str_replace(labels, "-100", "+")
  
  # Convert values into percentage bandings
  # `right = FALSE` indicates that bands should be closed on the left and open
  # on the right. i.e. bandings labelled as 10-20% truly mean 10-19%
  perc_band <-
    cut(x,                   
        breaks = breaks,
        labels = labels,
        right = FALSE,
        include.lowest = TRUE)
  
  # If numeric = TRUE, then recode the percentage bandings to the first integer
  # of the upper limit; e.g. recode 10-20% as 2
  # Otherwise, return perc_band as a character vector
  if(numeric) {
    as.numeric(stringr::str_extract(perc_band, "^\\d{1,2}")) + 10
  } else {
    as.character(perc_band)
  }
  
}


### END OF SCRIPT ###