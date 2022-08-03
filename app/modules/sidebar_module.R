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
    
    # Text instruction to use dropdown filers
    h2("Select options from the drop downs below", align = "center"),
    
    # Dropdown Filter - Local Authority
    school_filter_input(ns("la_school_filter"), la_names),
    
    # Dropdown Filter - Measure
    measure_filter_input(ns("measure_filter")),
    
    # Text instruction to click on boxes for further info
    h3("Click on any box for more information", align = "center"),
    
    br(),
    br(),
    br(),
    
    # Smarter Scotland Logo
    HTML(paste0("<center>", 
                img(src = "smarter-scotland.jpg", width = 200),
                "</center>"))
    
  )
    
}

sidebar_server <- function(input, output, session, data) {
  
  la_school <- callModule(school_filter_server, "la_school_filter", data)
  
  return(reactive(c(la_school(), measure = input$measure_filter)))
  
}