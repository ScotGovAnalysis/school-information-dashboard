
population_ui <- function(id,  school_type) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  fluidRow(
    
    section_header_output(ns("population_profile")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = TRUE,
      
      
      
      
      column(br(),width = 10,
      
      # Dropdown to select population measure
      if(school_type != "Primary")
        
        {selectInput(ns("measure_filter"), 
                  label = "Select population measure",
                  choices = c("Pupil Numbers", 
                              "Teacher Numbers (FTE)",
                              "Pupil Teacher Ratio"),
                  selected = "Pupil Numbers")}
      else{selectInput(ns("measure_filter"), 
                       label = "Select population measure",
                       choices = c("Pupil Numbers", 
                                   "Teacher Numbers (FTE)",
                                   "Pupil Teacher Ratio", 
                                   "Average Class"),
                       selected = "Pupil Numbers")}
      ),
      
      #add download button
      column(br(),br(),
             download_data_ui(ns("download")), width = 2),
      
      # Population Trend Line Chart
      fluidRow(column(br(),
                      withSpinner(uiOutput(ns("population_title"))),
                      
                      withSpinner(
                      plotlyOutput(ns("trend"))),width = 12)
                              ),
    
    column(br(), width = 12),
    br()
    
    )
  )
  
}

population_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "population_profile", "Population")
  callModule(download_data_server, "download", "Population Profile", data)
  
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
        labs(x = "Year", y = input$measure_filter),
      tooltip = "text"
    )%>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  
  output$population_title <- renderUI({
    
   list(
      h3(input$measure_filter, " by Year", align = "center"))
    
  })
  
}