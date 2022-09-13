
school_profile_text_output <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  withSpinner(uiOutput(ns("school_profile")))

}

school_profile_text_server <- function(input, output, session, data, school_type) {
  
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
