
dashboard_title_output <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  # Value Box Output
  valueBoxOutput(ns("title_box"), width = 12)
  
}

dashboard_title_server <- function(input, output, session, 
                                   school_type, selected_la_school) {
  
  output$title_box <- renderValueBox({
    valueBox(
      value = paste(school_type, "School Information Dashboard"),
      subtitle = 
        h4(paste(selected_la_school()$la, "-", selected_la_school()$school)),
      color = "orange"
    )
  })
  
}