
pupil_profile_ui <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  fluidRow(
    
    section_header_output(ns("pupil_header")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = FALSE,
      
      column(width = 10),
      column(download_data_ui(ns("download")), width = 2),
      
      column(plotlyOutput(ns("chart1")), width = 12),
      column(plotlyOutput(ns("chart2")), width = 12)
      
    )
    
  )
  
}

pupil_profile_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "pupil_header", "Pupil", box_colour = "navy")
  
  callModule(download_data_server, "download", "Pupil Profile", data)
  
  colours <- c("#3182bd", "#9ecae1", "#deebf7")
  
  output$chart1 <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(measure_category %in% 
                 c("sex", "stage", "deprivation")) %>%
        ggplot() + 
        geom_col(aes(measure, 
                     value, 
                     fill = measure_category,
                     colour = measure_category,
                     text = paste0("Measure: ", measure, "<br>",
                                   "% of Pupils: ", value_label))) +
        geom_text(aes(x = measure, y = value, label = value_label, text = ""),
                  hjust = 0.5, nudge_y = 5) +
        theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        scale_fill_manual(values = colours) +
        scale_colour_manual(values = colours) +
        labs(x = NULL , y = NULL) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              legend.position = "none"),
      tooltip = "text"
    )
    
  })
  
  output$chart2 <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(measure_category %in% 
                 c("free_school_meals", "additional_support_needs",
                   "english_additional_language", "ethnicity", 
                   "urban_rural")) %>%
        ggplot() + 
        geom_col(aes(measure, 
                     value, 
                     fill = measure_category,
                     colour = measure_category,
                     text = paste0("Measure: ", measure, "<br>",
                                   "% of Pupils: ", value_label))) +
        geom_text(aes(x = measure, y = value, label = value_label, text = ""),
                  hjust = 0.5, nudge_y = 5) +
        theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        labs(x = NULL , y = NULL) +
        scale_fill_manual(values = c(colours, colours[1:2])) +
        scale_colour_manual(values = c(colours, colours[1:2])) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              legend.position = "none"),
      tooltip = "text"
    )
    
  })
  
}