
dashboard_title_output <- function(id, school_type) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  # Value Box Output
  valueBoxOutput(ns("title"), width = 12)
  
}

dashboard_title_server <- function(input, output, session, school_type, selected_la_school) {
  
  # Render Value Box
  output$title <- renderValueBox({
    valueBox(paste(school_type, "School Information Dashboard"),
             h4(paste(selected_la_school()$la, "-", selected_la_school()$school)),
             color = "yellow")
  })
  
}