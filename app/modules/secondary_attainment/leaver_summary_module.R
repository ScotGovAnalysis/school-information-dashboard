leaver_summary_ui <- function(id) {

  ns <- NS(id)
  
  column(

    # Attainment leavers destination chart
    h3("Percentage of leavers in a positive destination",
       align = "center"),
    br(),
    withSpinner(plotlyOutput(ns("leavers_dest_chart"))),

    # Attainment leavers total tariff chart
    h3("Percentage of leavers in a positive destination",
       align = "center"),
    br(),
    withSpinner(plotlyOutput(ns("leavers_tariff_chart"))),

    width = 12
    
  )
  
}

leaver_summary_server <- function(input, output, session, data) {
  
  # Leavers destinations bar chart
  output$leavers_dest_chart <- renderPlotly({

    dat <-
      data() %>%
      filter(dataset == "destinations") %>%
      mutate(comparator = ifelse(comparator == "0",
                                 str_wrap(school_name, width = 20),
                                 "Virtual Comparator"))
    
    req(nrow(dat) > 0)
    
    plot <-
      ggplot(dat,
             aes(x = comparator,
                 y = value,
                 fill = comparator,
                 colour = comparator,
                 text = paste0("School: ", comparator, "<br>",
                               "Value: ", value_label)
      )) +
      geom_col() +
      scale_fill_manual(values = c("#3182bd", "#9ecae1")) +
      scale_colour_manual(values = c("#3182bd", "#9ecae1")) +
      labs(x = NULL , y = "% of Leavers", fill = NULL, colour = NULL)
    
    ggplotly(plot, tooltip = "text") %>%
    config(displayModeBar = F, responsive = FALSE) %>%
    layout(xaxis = list(fixedrange = TRUE),
           yaxis = list(fixedrange = TRUE))

  })


  # Leavers tariff chart
  output$leavers_tariff_chart <- renderPlotly({

    dat <-
      data() %>%
      filter(dataset == "attainment_for_all") %>%
      mutate(comparator = ifelse(comparator == "0",
                                 str_wrap(school_name, width = 20),
                                 "Virtual Comparator"))
    
    req(nrow(dat) > 0)
    
    plot <-
      ggplot(dat,
             aes(x = measure,
                 y = value,
                 fill = comparator,
                 colour = comparator,
                 text = paste0("School: ", comparator, "<br>",
                               "Value: ", value_label)
      )) +
      geom_col(width = 0.5, position = "dodge") +
      scale_fill_manual(values = c("#3182bd", "#9ecae1")) +
      scale_colour_manual(values = c("#3182bd", "#9ecae1")) +
      labs(x = NULL , y = "Total Tariff Score", fill = NULL, colour = NULL) +
      scale_x_discrete(labels = c(
        "average_total_tariff_lowest_20_percent" = "Lowest 20%",
        "average_total_tariff_middle_60_percent" = "Middle 60%",
        "average_total_tariff_highest_20_percent" = "Highest 20%"
      )) +
      theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))

  })
  
}