html_tags <- function() {
  
  tags$head( 
    
    # Set universal font
    # This is used as a workaround for ggiraph issue where font is different
    # when app is deployed:
    # https://github.com/davidgohel/ggiraph/issues/91
    tags$style(type = "text/css", "text {font-family: sans-serif}"),
    
    # Recode error messages resulting from data validation
    tags$style(
      type = "text/css",
      ".shiny-output-error-no-data { visibility: hidden; }",
      ".shiny-output-error-no-data:before { visibility: visible; ",
      "content: 'There is no data available for this chart.'; }"
    )
    
  )
  
}