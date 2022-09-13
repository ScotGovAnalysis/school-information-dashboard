
school_filter_input <- function(id, la_names) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  tagList(
    
    # Local Authority dropdown filter
    selectInput(
      inputId = ns("la"),
      label = "Local Authority",
      choices = la_names,
      selected = "Scotland"
    ),
    
    # School dropdown filter
    # Choices are NULL as these are determined in server side by what
    # Local Authority is selected
    selectInput(
      inputId = ns("school"),
      label = "School",
      choices = NULL,
      selected = "All publicly funded schools"
    )
    
  )
  
}

school_filter_server <- function(input, output, session, data) {
  
  # When the Local Authority selection changes, update the school filter
  # to give choice of schools within that local authority
  observeEvent(
    input$la,
    {updateSelectInput(session,
                       input = "school",
                       choices = data %>%
                         filter(la_name == input$la) %>%
                         pull(school_name))
    }
  )
  
  # Return a list of the selected Local Authority and School to be used
  # to filter other elements of dashboard
  return(reactive(list(la = input$la, school = input$school)))
  
}


### END OF SCRIPT ###