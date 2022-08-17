
primary_attainment_ui <- function(id, year_options) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  max_year <- which.max(as.numeric(substr(year_options, 1, 4)))
  
  fluidRow(
    
    section_header_output(ns("attain_profile")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = TRUE,
     
      # Dropdown Filter - Attainment Year
      fluidRow(column(selectInput(ns("year"), 
                         label = "Select year",
                         choices = year_options,
                         selected = year_options[max_year]), 
             width = 3),
      
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
             width = 3),
      
      column(br(),
             download_data_ui(ns("download")),width = 2),
             
      
      # Attainment BGE Bar Chart
      fluidRow(column(br(),withSpinner(plotlyOutput(ns("bar_chart"))), 
             width = 7),
      
      column(width = 1),
      
      # Attainment BGE Doughnut Chart
      column(br(),withSpinner(girafeOutput(ns("donut_plot"))), 
             width = 4))
      
    )
    
  )
  )
    
}

primary_attainment_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "attain_profile", "Primary Attainment")
  
  callModule(download_data_server, "download", "Attainment Profile", data)
  
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
        scale_x_discrete(labels = c("0" = unique(data()$school_name), 
                                    "1" = "Virtual Comparator")) +
        scale_y_continuous(limits = c(0, 4),
                           labels = c("Not yet early level",
                                      "Early level",
                                      "1st level",
                                      "2nd level",
                                      "3rd level or better")) + 
        ggtitle("Average curriculum for excellence level achieved"),
      tooltip = "text"
    )%>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })  

  output$donut_plot <- renderGirafe({

    acel_data <-
      data() %>%
      filter(dataset == "acel" & year == input$year &
               str_starts(measure, input$bge) & stage == input$stage) %>%
      mutate(text = paste0(measure, ": ", value_label))
    
    plot <- 
      ggplot(acel_data, aes(y = rev(value), 
                            fill = measure, 
                            tooltip = rev(text))) +
      geom_bar_interactive(
        aes(x = 1),
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
      scale_fill_manual(values = c("white", "#3182bd")) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(plot.title = element_text(size = 16, hjust = 0.5)) +
      ggtitle(str_wrap(
        "Percentage of students meeting curriculum for excellence level",
        width = 30))
    
    girafe(
      ggobj = plot,
      width_svg = 5,
      height_svg = 5,
      options = list(opts_toolbar(saveaspng = FALSE))
    )
    
  })

}