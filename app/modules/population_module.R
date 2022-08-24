
population_ui <- function(id,  school_type) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  # Measure options dependent on school_type
  pop_measures <- c(
    "Pupil Numbers", "Teacher Numbers (FTE)", "Pupil Teacher Ratio"
  )
  
  if(school_type == "Primary") {
    pop_measures <- c(pop_measures, "Average Class")
  }
  
  fluidRow(
    
    section_header_output(ns("header")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = TRUE,
      
      # Dropdown to select population measure
      column(
        selectInput(ns("measure_filter"), 
                     label = "Select population measure",
                     choices = pop_measures,
                     selected = "Pupil Numbers"),
        width = 10
      ),
      
      # Download button
      column(
        br(),
        download_data_ui(ns("download")), 
        width = 2
      ),
      
      # Population Trend Line Chart
      column(
        withSpinner(uiOutput(ns("chart_title"))),
        withSpinner(plotlyOutput(ns("chart"))),
        br(),
        width = 12
      )
      
    )
    
  )
  
}

population_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "header", "Population")
  callModule(download_data_server, "download", "Population Profile", data)
  
  output$chart_title <- renderUI({
      h3(input$measure_filter, " by Year", align = "center")
  })
  
  output$chart <- renderPlotly({
    
    plot <- 
      data() %>%
      filter(measure == input$measure_filter) %>%
      ggplot(aes(
        x = year, y = value, 
        group = 1,
        text = paste0("Year: ", year, "<br>",
                      input$measure_filter, ": ", value_label)
      )) + 
      geom_line() +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "Year", y = input$measure_filter)
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, 
             responsive = FALSE) %>% 
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
    
  })
  
}