sidebar_ui <- function(id, la_names) {
  
  ns <- NS(id)
  
  tagList(
    
    # Set the colour of the side bar          
    tags$style(
      HTML(".main-sidebar.main-sidebar-solid.main-sidebar-primary>",
           ".main-sidebar-header {color:white; background:#100f3a}",
           ".skin-blue .main-sidebar {background-color: #100f3a;}")
    ),
    
    # Smarter Scotland Logo
    tags$a(
      href = "https://statistics.gov.scot/home", 
      tags$img(
        src = "smarter-scotland.jpg", 
        width = 220, 
        alt = "Smarter Scotland logo with a link to Scotland Statistics page"
      )
    ),
    
    br(),
    
    # Text instruction to use dropdown filers
    h2("Select options from the drop downs below", align = "left"),
    br(),
    
    # Local Authority and School Dropdown Filters
    school_filter_input(ns("la_school_filter"), la_names),

    # Text instruction to click on boxes for further info
    h3("Click on any title box for more information", align = "left"),
    
  )
    
}

sidebar_server <- function(input, output, session, data) {
  
  # Return school_filter_server to allow LA/School selection to be used
  # in other dashboard elements
  return(callModule(school_filter_server, "la_school_filter", data))
  
}