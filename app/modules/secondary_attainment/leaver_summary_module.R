leaver_summary_ui <- function(id) {

  ns <- NS(id)
  
  column(

    # Attainment leavers destination chart
    h3("Percentage of leavers in a positive destination",
       align = "center"),
    withSpinner(plotlyOutput(ns("leavers_dest_chart"))),

    # Attainment leavers total tariff chart
    h3("Average total tarrif score",
       align = "center"),
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
      mutate(
        comparator = ifelse(comparator == "0",
                                 str_wrap(school_name, width = 20),
                                 "Virtual Comparator")
      )
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")    
    
    plot <-
      ggplot(dat, aes(x = comparator, 
                      y = value,
                      text = paste0("School: ", comparator, "<br>",
                                    "Value: ", value_label))) +
      geom_col(aes(fill = comparator, colour = comparator)) +
      geom_text(aes(label = chart_label),
                hjust = 0.5, nudge_y = 5) +
      scale_fill_manual(values = c("#3182bd", "#9ecae1")) +
      scale_colour_manual(values = c("#3182bd", "#9ecae1")) +
      scale_y_continuous(limits = c(0, 100)) +
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
      mutate(
        comparator = ifelse(comparator == "0",
                                 str_wrap(school_name, width = 20),
                                 "Virtual Comparator"),
        measure = paste0(
          to_any_case(word(measure, 4, 5, sep = "_"), case = "title"), "%"
        ),
        measure = factor(measure, 
                         levels = c("Lowest 20%", "Middle 60%", "Highest 20%"))
      )
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")    
    
    plot <-
      ggplot(dat,
             aes(x = measure,
                 y = value,
                 text = paste0("School: ", comparator, "<br>",
                               "Value: ", value_label)
      )) +
      geom_col(aes(fill = comparator, colour = comparator),
               width = 0.8, position = "dodge2") +
      geom_text(aes(y = value + 100, label = chart_label, group = comparator),
                hjust = 0.5, position = position_dodge2(width = 0.8)) +
      scale_fill_manual(values = c("#3182bd", "#9ecae1")) +
      scale_colour_manual(values = c("#3182bd", "#9ecae1")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = NULL , y = "Total Tariff Score", fill = NULL, colour = NULL) +
      theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))

  })
  
}