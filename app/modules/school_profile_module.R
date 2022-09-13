
school_profile_output <- function(id, school_type, faq_sections) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  box(
    
    width = 12,
    collapsible = FALSE,
  
    column(
      
      # Insert map
      map_output(ns("map")),
      br(),
      
      # Insert buttons for Covid-19, FAQs and info
      fluidRow(
        covid19_ui(ns("covid19"), school_type),
        faq_ui(ns("faq"), faq_sections)
      ),
      br(),
      
      fluidRow(
        important_info_ui(ns("important_info"))
      ),
      
      width = 4,
      
    ),
    
    # Create column for school profile text
    column(
      school_profile_text_output(ns("text")),
      width = 8
    )
    
  )
  

}

school_profile_server <- function(input, output, session, data, faq_data, school_type) {
  
  callModule(map_server, "map", data)
 
  callModule(faq_server, "faq", faq_data)
   
  callModule(school_profile_text_server, "text", data, school_type)
  
}