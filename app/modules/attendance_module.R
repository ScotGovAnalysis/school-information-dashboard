
attendance_ui <- function(id, school_type) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  tagList(
    
    section_header_output(ns("title_box")),
    
    box(
      
      title = NULL,
      width = 12,
      
      # Dropdown Filter - Attendance Measure
      column(
        selectInput(ns("measure"), 
                    label = "Select attendance measure",
                    choices = c("Attendance", 
                                "Authorised Absence",
                                "Unauthorised Absence"),
                    selected = "Attendance"),
        width = 10
      ),
        
      # Download data button
      column( 
        br(),
        download_data_ui(ns("download")),
        width = 2,
      ),
      
      # Attendance Trend Line Chart
      column(
        withSpinner(tagList(
          uiOutput(ns("trend_title")),
          plotlyOutput(ns("trend"))
        )), 
        width = ifelse(school_type == "Special", 12, 7)
      ),
      
      # Attendance Stage Bar Chart
      column(
        if(school_type != "Special") {
          withSpinner(tagList(
            uiOutput(ns("stage_title")),
            plotlyOutput(ns("stage"))
          ))
        },
        width = 5
      ),
      
      column(br(), width = 12)
      
    )
    
  )
  
}

attendance_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "title_box", "Attendance")
  
  callModule(download_data_server, "download", "Attendance", data)
  
  output$trend_title <- renderUI({
    h3(input$measure, " by Year", align = "center")
  })
  
  output$trend <- renderPlotly({
    
    dat <-
      data() %>%
      filter(measure == input$measure & stage == "All Stages") %>%
      mutate(value = ifelse(value_label %in% c("z", "c", "x"), NA, value))
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")
    
    plot <-
      ggplot(
        dat,
        aes(x = year, y = value, group = 1,
            text = paste0("Year: ", year, "<br>",
                          input$measure, ": ", value_label))
      ) + 
      geom_line() +
      scale_y_continuous(limits = c(0, NA)) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
      labs(x = "Academic Year", y = paste("%",input$measure))
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))

  })
  
  output$stage_title <- renderUI({
    h3(input$measure, " by Stage", align = "center")
  })
  
  output$stage <- renderPlotly({
    
    dat <-
      data() %>%
      filter(measure == input$measure & stage != "All Stages")
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data") 
    
    text_nudge <- max(dat$value) * 0.1
    
    plot <-
      ggplot(
        dat, 
        aes(x = value, y = stage,
            text = paste0("Stage: ", stage, "<br>",
                          input$measure, ": ", value_label))
      ) + 
      geom_col() +
      geom_text(aes(x = value + text_nudge, label = chart_label), 
                vjust = 0.5) +
      scale_x_continuous(limits = c(0, NA)) +
      labs(x = paste("%",input$measure) , y = "Pupil Stage")
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
    
  })
  
}