
school_profile_output <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("school_name")),
    uiOutput(ns("la_name")),
    uiOutput(ns("seed_code")),
    uiOutput(ns("denomination")),
    uiOutput(ns("condition")),
    uiOutput(ns("address")),
    uiOutput(ns("telephone")),
    uiOutput(ns("email")),
    uiOutput(ns("website")),
    uiOutput(ns("la_website"))
  )

}

school_profile_server <- function(input, output, session, data) {
  
  output$school_name <- renderUI({
    h3("School Name: ", data()$school_name)
  })
  
  output$seed_code <- renderUI({
    h3("Seed Code: ", data()$seed_code)
  })

  output$la_name <- renderUI({
    h3("Local Authority/Area: ", data()$la_name)
  })

  output$denomination <- renderUI({
    if(!is.na(data()$denomination)) {
      h3("Denomination: ", data()$denomination)
    }
  })

  output$condition <- renderUI({
    h3("School condition: ",
       data()$condition,
       ifelse(str_starts(data()$school_name, "All "),
              " in A or B",
              ""))
  })

  output$address <- renderUI({
    if(!is.na(data()$address)) {
      h3("Address: ", data()$address)
    }
  })

  output$telephone <- renderUI({
    if(!is.na(data()$phone_number)) {
      h3("Telephone: ", data()$phone_number)
    }
  })

  output$email <- renderUI({
    if(!is.na(data()$email)) {
      h3("Email: ", a(href = paste0("mailto:", data()$email),
                      data()$email))
    }
  })

  output$website <- renderUI({
    if(!is.na(data()$website)) {
      h3("Website: ", a(href = data()$website,
                        data()$website))
    }
  })

  output$la_website <- renderUI({
    if(!is.na(data()$la_website)) {
      h3("Local Authority Website: ",
         a(href = data()$la_website,
           data()$la_website))
    }
  })
  
}