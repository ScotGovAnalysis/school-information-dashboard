html_tags <- function() {
  
  tags$head( 
    
    # Set universal font
    # This is used as a workaround for ggiraph issue where font is different
    # when app is deployed:
    # https://github.com/davidgohel/ggiraph/issues/91
    tags$style(type = "text/css", "text {font-family: sans-serif}"),
    
    # Edit background colour of value boxes to meet required colour contrast
    # Edit cursor type for clickable value boxes
    
    ## Main dashboard header
    tags$style(".small-box.bg-orange { ",
               "background-color: #DC730B !important; ",
               "color: #FFFFFF !important; }"),
    
    ## Section headers
    tags$style(".small-box.bg-yellow { ",
               "background-color: #DC730B !important; ",
               "color: #FFFFFF !important; ",
               "cursor: pointer !important; }"),
    tags$style(".small-box.bg-navy { ",
               "cursor: pointer !important; }"),

    ## School profile value boxes
    tags$style(".small-box.bg-teal { ",
               "background-color: #298477 !important; ",
               "color: #FFFFFF !important; ",
               "cursor: pointer !important; }"),

    # Recode error messages resulting from data validation
    tags$style(
      type = "text/css",
      ".shiny-output-error-no-data { visibility: hidden; }",
      ".shiny-output-error-no-data:before { visibility: visible; ",
      "content: 'There is no data available for this chart.'; }"
    )
    
  )
  
}