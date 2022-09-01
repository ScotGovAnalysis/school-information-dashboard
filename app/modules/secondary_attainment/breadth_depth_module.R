breadth_depth_ui <- function(id) {
  
  ns <- NS(id)
  
  column(
    withSpinner(
      tagList(
        uiOutput(ns("title")),
        plotlyOutput(ns("chart"))
      )
    ),
    width = 12
  )
  
}

breadth_depth_server <- function(input, output, session, data) {
  
  output$title <- renderUI({
    
    req(nrow(data()) > 0)
    
    h3("Pupils achieving SCQF Level ", 
       unique(data()$minimum_scqf_level), 
       " or better by Year", 
       align = "center")
    
  })
  
  output$chart <- renderPlotly({
    
    req(nrow(data()) > 0)
    
    plot <-
      data() %>%
      mutate(comparator = ifelse(comparator == "0",
                                 str_wrap(school_name, width = 20),
                                 "Virtual Comparator"),
             comparator = fct_relevel(
               as_factor(comparator),
               "Virtual Comparator", after = Inf
             ),
             minimum_scqf_level = str_c("SCQF level ",
                                        minimum_scqf_level, 
                                        " or better")) %>%
      ggplot(aes(year, 
                 value, 
                 group = 1,
                 colour = comparator,
                 text = paste0("Year: ", year, "<br>",
                               value_label, "%"))) + 
      geom_line() +
      scale_color_manual(values=c("#3182bd", "#9ecae1")) +
      scale_y_continuous(limits = c(0,NA)) + 
      theme(axis.text.x = element_text(angle = 40, hjust = 1),
            legend.position = "bottom") +
      labs(x = "Academic Year", y = "% of Leavers", colour = "")
   
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      )
    
  })
  
}