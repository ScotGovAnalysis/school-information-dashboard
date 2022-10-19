
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
      withSpinner(uiOutput(ns("school_profile"))),
      width = 8
    )
    
  )
  

}

school_profile_server <- function(input, output, session, data, faq_data, school_type) {
  
  callModule(map_server, "map", data)
 
  callModule(faq_server, "faq", faq_data)
   
  output$school_profile <- renderUI({
    
    # Display error message if no data returned
    validate(need(nrow(data()) > 0, label = "data"), errorClass = "no-data")    
    
    list(
      
      h3(strong("School Name:  "), data()$school_name),
      
      h3(strong("Seed Code:  "), data()$seed_code),
      
      h3(strong("Local Authority/Area:  "), data()$la_name),
      
      if(!is.na(data()$denomination) & school_type != "Special") {
        h3(strong("Denomination:  "), data()$denomination)
      },
      
      if(!is.na(data()$condition) & school_type == "Primary") {
        h3(strong("School condition:  "),
           data()$condition,
           ifelse(str_starts(data()$school_name, "All "),
                  " in A or B",
                  ""))
      },
      
      if(!is.na(data()$address)) {
        h3(strong("Address:  "), data()$address)
      },
      
      if(!is.na(data()$phone_number)) {
        h3(strong("Telephone:  "), data()$phone_number)
      },
      
      if(!is.na(data()$email)) {
        h3(strong("Email:  "), 
           a(href = paste0("mailto:", data()$email), data()$email)
        )
      },
      
      if(!is.na(data()$website)) {
        h3(strong("Website:  "), a(href = data()$website, data()$website))
      },
      
      if(!is.na(data()$la_website)) {
        h3(strong("Local Authority Website:  "),
           a(href = data()$la_website, data()$la_website)
        )
      }
      
    )
    
  })
  
}