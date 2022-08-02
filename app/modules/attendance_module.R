
attendance_ui <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  fluidRow(
    
    section_header_output(ns("attendance_header")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = FALSE,
      
      # Dropdown Filter - Attendance Measure
      selectInput(ns("measure_filter"), 
                  label = "Select attadance measure",
                  choices = c("Attendance", 
                              "Authorised Absence",
                              "Unauthorised Absence"),
                  selected = "Attendance"),
      
      # Attendance Trend Line Chart
      column(plotlyOutput(ns("trend")), width = 7),
      
      # Attendance Stage Bar Chart
      column(plotlyOutput(ns("stage")), width = 5)
      
    )
    
  )
  
}

attendance_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "attendance_header", "Attendance")
  
  output$trend <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(measure == input$measure_filter & stage == "All Stages") %>%
        ggplot(aes(year, 
                   value, 
                   group = 1,
                   text = paste0("Year: ", year, "<br>",
                                input$measure_filter, ": ", value_label))) + 
        geom_line() +
        geom_text_repel(aes(label = paste(value_label,"%")),
                        na.rm = TRUE,
                        nudge_x = 0,
                        check_overlap = TRUE) +
        scale_y_continuous(limits = c(0,NA)) +
        labs(x = "Academic Year", y = paste("%",input$measure_filter)),
      tooltip = "text"
    ) 

  })
  
  output$stage <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(measure == input$measure_filter & stage != "All Stages") %>%
        ggplot(aes(value, 
                   stage,
                   text = paste0("Stage: ", stage, "<br>",
                                input$measure_filter, ": ", value_label))) + 
        geom_col() +
        labs(x ="Percentage Attendance" , y = "Pupil Stage"),
      tooltip = "text"
    )
    
  })
  
}