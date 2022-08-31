html_tags <- function() {
  
  tagList(
    
    # Recode any error messages that are visible to user
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      paste0(".shiny-output-error:before { visibility: visible; ",
             "content: 'There is no data for this chart'; }")
    ),
    
    # Set universal font
    # This is used as a workaround for ggiraph issue where font is different
    # when app is deployed:
    # https://github.com/davidgohel/ggiraph/issues/91
    tags$head( 
      tags$style(type = "text/css", "text {font-family: sans-serif}")
    )
  )
  
}