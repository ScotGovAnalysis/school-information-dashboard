breadth_depth_ui <- function(id) {
  
  ns <- NS(id)
  
  column(
    withSpinner(tagList(
      uiOutput(ns("title")),
      plotlyOutput(ns("chart"))
    )),
    width = 12
  )
  
}

breadth_depth_server <- function(input, output, session, data, scqf_level) {
  
  output$title <- renderUI({
    
    h3("School Leavers achieving ", scqf_level(), " by Year", 
       align = "center")
    
  })
  
  output$chart <- renderPlotly({
    
    dat <-
      data() %>%
      mutate(
        comparator = ifelse(comparator == "0",
                            str_wrap(school_name, width = 20),
                            "Virtual Comparator"),
        comparator = fct_relevel(
          as_factor(comparator), 
          function(x) {sort(x[x != "Virtual Comparator"])}
        ),
        minimum_scqf_level = paste("SCQF level", 
                                   minimum_scqf_level, 
                                   "or better"),
        value = ifelse(value_label %in% c("z", "x", "c"), NA, value)
      )
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")
    
    plot <- 
      ggplot(dat, aes(x = year, 
                      y = value, 
                      group = 1,
                      colour = comparator,
                      text = paste0("Year: ", year, "<br>",
                                    value_label, "%"))) + 
      geom_line() +
      scale_color_manual(values = c("#3182bd", "#9ecae1")) +
      scale_y_continuous(limits = c(0, NA)) + 
      theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
      labs(x = "Academic Year", y = "% of Leavers", colour = "")
   
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
    
  })
  
}