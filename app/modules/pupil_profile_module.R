
pupil_profile_ui <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  tagList(
    
    section_header_output(ns("title_box")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = FALSE,
      
      column(width = 10),
      
      column(
        br(),
        download_data_ui(ns("download")), 
        width = 2
      ),
      
      column(
        withSpinner(tagList(
          plotlyOutput(ns("chart1"), height = 400),
          plotlyOutput(ns("chart2"), height = 440)
        )),
        width = 12
      )
      
    )
    
  )
  
}

pupil_profile_server <- function(input, output, session, data, school_type) {
  
  callModule(section_header_server, "title_box", "Pupil", box_colour = "navy")
  
  callModule(download_data_server, "download", "Pupil Profile", data)
  
  pp_chart <- function(pp_data) {
    
    plot <-
      ggplot(pp_data, aes(text = paste0("Measure: ", measure, "<br>",
                                    "% of Pupils: ", value_label))) + 
      geom_col(aes(x = measure, y = value)) +
      geom_text(aes(x = measure, y = value, label = trimws(value_label)),
                hjust = 0.5, nudge_y = 5) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
      scale_y_continuous(limits = c(0, 105)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(x = NULL , y = NULL) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank())
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
    
  }
  
  output$chart1 <- renderPlotly({
    
    dat <- 
      data() %>%
        filter(measure_category %in% c("sex", "stage", "deprivation"))
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")
    
    pp_chart(dat)

  })
  
  c2_measures <- c("english_additional_language", "ethnicity", "urban_rural")
  
  if(school_type != "Special") {
    c2_measures <- c(c2_measures, 
                     "free_school_meals", "additional_support_needs")
  }
  
  output$chart2 <- renderPlotly({
    
    dat <-
      data() %>%
      filter(measure_category %in% c2_measures) 
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")
    
    pp_chart(dat)
    
  })
  
}