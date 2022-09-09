leaver_simd_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
  
    # Attainment leavers deprivation chart
    column(
      h3("School leavers attainment by SIMD", align = "center"),
      withSpinner(plotlyOutput(ns("tariff"))),
      width = 6
    ),

    # Attainment leavers SIMD chart
    column(
      h3("School leavers by SIMD", align = "center"),
      withSpinner(plotlyOutput(ns("leavers"))),
      width = 6
    )
    
  )
  
}

leaver_simd_server <- function(input, output, session, data) {
  
  # Attainment by deprivation chart
  output$tariff <- renderPlotly({

    dat <-
      data() %>%
      filter(dataset == "attainment_by_deprivation" & 
               str_ends(measure, "total_tariff")) %>%
      mutate(
        comparator = ifelse(comparator == "0",
                            str_wrap(school_name, width = 20),
                            "Virtual Comparator"),
        measure = paste0("Q", parse_number(measure))
      )
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")
    
    plot <-
      ggplot(dat, aes(x = measure,
                      y = value,
                      text = paste0("School: ", comparator, "<br>",
                                    "Value: ", value_label)
      )) +
      geom_col(aes(fill = comparator, colour = comparator),
               width = 0.8, position = "dodge") +
      geom_text(aes(y = value + 100, label = chart_label, group = comparator),
                hjust = 0.5, position = position_dodge2(width = 0.8)) +
      scale_fill_manual(values = c("#3182bd", "#9ecae1")) +
      scale_colour_manual(values = c("#3182bd", "#9ecae1")) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "SIMD Grouping", y = "Total Tariff Score", 
           fill = NULL, colour = NULL)
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
    
  })
  
  # Number of leavers by SIMD chart
  output$leavers <- renderPlotly({
    
    dat <-
      data() %>%
      filter(dataset == "attainment_by_deprivation"
             & str_ends(measure, "of_leavers")) %>%
      mutate(measure = paste0("Q", parse_number(measure)))
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")    
    
    plot <-
      ggplot(dat, aes(x = measure,
                      y = value,
                      text = paste0("School: ", school_name, "<br>",
                                    "Value: ", value_label)
      )) +
      geom_col(width = 0.8) +
      geom_text(aes(y = value + 5, label = chart_label), hjust = 0.5) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "SIMD Grouping", y = "% of Leavers")
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
    
  })
  
  
}