
attainment_ui <- function(id, year_options) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  max_year <- which.max(as.numeric(substr(year_options, 1, 4)))
  
  fluidRow(
    
    section_header_output(ns("attainment_header")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = TRUE,
      
      # Dropdown Filter - Attainment Year
      column(selectInput(ns("year"), 
                         label = "Select year",
                         choices = year_options,
                         selected = year_options[max_year]), 
             width = 4),
      
      # Dropdown Filter - Attainment BGE Measure
      column(selectInput(ns("bge"), 
                         label = "Select attainment measure",
                         choices = c("Reading", 
                                     "Listening & Talking",
                                     "Numeracy", 
                                     "Writing"),
                         selected = "Reading"), 
             width = 4),
      
      # Dropdown Filter - Attainment Stage
      column(selectInput(ns("stage"), 
                         label = "Select stage",
                         choices = c("P1", 
                                     "P4",
                                     "P7",
                                     "P1, P4 & P7 combined"),
                         selected = "P4"), 
             width = 4),
      
      # Attainment BGE Bar Chart
      column(
        # h3("Average curriculum for excellence level achieved", 
        #         align = "center"),
             plotlyOutput(ns("bar_chart")), 
             width = 7),
      
      # Attainment BGE Doughnut Chart
      column(
        # h3("Percentage of students meeting curriculum for excellence level",
        #         align = "center"),
             girafeOutput(ns("donut_plot")), 
             width = 4)
      
    )
    
  )
    
}

attainment_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "attainment_header", "Attainment")
  
  output$bar_chart <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(dataset == "bge" & year == input$year &
                 measure == input$bge & stage == input$stage) %>%
        ggplot(aes(
          comparator, 
          value,
          text = paste0("School: ", 
                        ifelse(comparator == 0, 
                               school_name, 
                               "Virtual Comparator"),
                        "<br>",
                        "Value: ", value_label)
        )) + 
        geom_col() +
        labs(x = NULL , y = NULL) +
        ggtitle("Average curriculum for excellence level achieved"),
      tooltip = "text"
    )
    
  })  

  output$donut_plot <- renderGirafe({

    acel_data <-
      data() %>%
      filter(dataset == "acel" & year == input$year &
               str_starts(measure, input$bge) & stage == input$stage)
    
    girafe(
      ggobj =
        ggplot(acel_data, aes(y = value, fill = rev(measure))) +
        geom_bar_interactive(
          aes(x = 1,
              tooltip = paste0(measure, ": ", value_label)),
          width = 0.5,
          stat = "identity",
          show.legend = FALSE
        ) +
        annotate(
          geom = "text",
          x = 0,
          y = 0,
          label = paste0(
            filter(acel_data, str_ends(measure, "% Meeting Level")) %>%
              pull(value_label),
            "%"),
          size = 12,
          color = "#3182bd"
        ) +
        scale_fill_manual(values = c("white","#3182bd")) +
        coord_polar(theta = "y") +
        theme_void() +
        theme(plot.title = element_text(size = 16, hjust = 0.5)) +
        ggtitle(str_wrap(
          "Percentage of students meeting curriculum for excellence level",
          width = 30)),
      width_svg = 5,
      height_svg = 5
    )

  })

}