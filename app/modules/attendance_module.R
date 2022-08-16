
attendance_ui <- function(id, school_type) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  fluidRow(
    
    section_header_output(ns("attend_profile")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = TRUE,
      
      # Dropdown Filter - Attendance Measure
      column(br(),
        width = 10,
        selectInput(ns("measure_filter"), 
                    label = "Select attadance measure",
                    choices = c("Attendance", 
                                "Authorised Absence",
                                "Unauthorised Absence",
                                "Temporary Exclusions"),
                    selected = "Attendance")
      ),
        
      column(width = 2, 
             br(),br(),
             download_data_ui(ns("download"))
      ),
      
      # Attendance Trend Line Chart
      column(plotlyOutput(ns("trend")), 
             width = ifelse(school_type == "Special", 12, 7)),
      
      # Attendance Stage Bar Chart
      column(plotlyOutput(ns("stage")), width = 5)
      
    )
    
  )
  
}

attendance_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "attend_profile", "Attendance")
  
  callModule(download_data_server, "download", "Attendance", data)
  
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
        theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1)) +
        labs(x = "Academic Year", y = paste("%",input$measure_filter)),
      tooltip = "text"
    ) %>%
      config(displayModeBar = F)

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
        labs(x = paste("%",input$measure_filter) , y = "Pupil Stage"),
      tooltip = "text"
    ) %>%
      config(displayModeBar = F)
    
  })
  
}