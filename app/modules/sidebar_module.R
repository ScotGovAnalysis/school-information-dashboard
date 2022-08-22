sidebar_ui <- function(id, la_names) {
  
  ns <- NS(id)
  
  tagList(
    
    tags$script(
      JS("document.getElementsByClassName('sidebar-toggle')[0]",
         ".style.visibility = 'hidden';")
    ),
    
    useShinyjs(),
    
    # Set the colour of the side bar          
    tags$style(
      HTML(".main-sidebar.main-sidebar-solid.main-sidebar-primary>",
           ".main-sidebar-header {color:white; background:#100f3a}",
           ".skin-blue .main-sidebar {background-color: #100f3a;}")
    ),
    
    # Smarter Scotland Logo
    tags$a(
      href="https://statistics.gov.scot/home", 
      tags$img(src = "smarter-scotland.jpg", 
                    width = 220, 
                    alt = "Smarter Scotland logo with a link to scotland statistics page", 
                    ))
          ,
    br(),
    
    # Text instruction to use dropdown filers
    h2("Select options from the drop downs below", align = "left"),
    br(),
    
    # Dropdown Filter - Local Authority
    school_filter_input(ns("la_school_filter"), la_names),
    

    # Text instruction to click on boxes for further info
    h3("Click on any title box for more information", align = "left"),
    

    
    
   
    
  )
    
}

sidebar_server <- function(input, output, session, data) {
  
  la_school <- callModule(school_filter_server, "la_school_filter", data)
  
  return(reactive(c(la_school(), measure = input$measure_filter)))
  
}