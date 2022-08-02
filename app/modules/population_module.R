
population_ui <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  fluidRow(
    
    section_header_output(ns("population_header")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = FALSE,
      
      # Dropdown to select population measure
      selectInput(ns("measure_filter"), 
                  label = "Select population measure",
                  choices = c("Pupil Numbers", 
                              "Teacher Numbers (FTE)",
                              "Pupil Teacher Ratio", 
                              "Average Class"),
                  selected = "Pupil Numbers"),
      
      # Population Trend Line Chart
      plotlyOutput(ns("trend")),
      
    )
    
  )
  
}

population_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "population_header", "Population")
  
  output$trend <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(measure == input$measure_filter) %>%
        ggplot(aes(year, 
                   value, 
                   group = 1,
                   text = paste0("Year: ", year, "<br>",
                                 input$measure_filter, ": ", value_label))) + 
        geom_line() +
        scale_y_continuous(limits = c(0,NA)) +
        labs(x = "Year", y = input$pop_var),
      tooltip = "text"
    )
    
  })
  
}