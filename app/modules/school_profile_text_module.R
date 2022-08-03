
school_profile_text_output <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  uiOutput(ns("school_profile"))

}

school_profile_text_server <- function(input, output, session, data) {
  
  output$school_profile <- renderUI({
    
    list(
      
      h3("School Name: ", data()$school_name),
      
      h3("Seed Code: ", data()$seed_code),
      
      h3("Local Authority/Area: ", data()$la_name),
      
      if(!is.na(data()$denomination)) {
        h3("Denomination: ", data()$denomination)
      },
      
      if(!is.na(data()$condition)) {
        h3("School condition: ",
           data()$condition,
           ifelse(str_starts(data()$school_name, "All "),
                  " in A or B",
                  ""))
      },
      
      if(!is.na(data()$address)) {
        h3("Address:", data()$address)
      },
      
      if(!is.na(data()$phone_number)) {
        h3("Telephone:", data()$phone_number)
      },
      
      if(!is.na(data()$email)) {
        h3("Email:", a(href = paste0("mailto:", data()$email),
                       data()$email))
      },
      
      if(!is.na(data()$website)) {
        h3("Website:", a(href = data()$website, data()$website))
      },
      
      if(!is.na(data()$la_website)) {
        h3("Local Authority Website:",
           a(href = data()$la_website,
             data()$la_website))
      }
      
    )
    
  })
  
}