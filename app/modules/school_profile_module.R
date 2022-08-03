
school_profile_output <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  box(
    width = 12,
    collapsible = FALSE,
  
    column(
      width = 4,
      
      # Insert map
      map_output(ns("map")),
      br(),
      
      # Insert buttons for Covid-19 and FAQs
      fluidRow(
        covid19_ui(ns("covid19")),
        faq_ui(ns("faq"))
      )
    ),
    
    # Create column for school profile text
    column(
      width = 8,
      school_profile_text_output(ns("text"))
    )
    
  )

}

school_profile_server <- function(input, output, session, map_data, data, faq_data) {
  
  callModule(map_server, "map", map_data)
 
  callModule(faq_server, "faq", faq_data)
   
  callModule(school_profile_text_server, "text", data)
  
}