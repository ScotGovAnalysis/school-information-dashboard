cfe_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
      column(
        h3(paste("Percentage of students meeting",
                 "curriculum for excellence level"),
           align = "center"), 
        width = 12
      ),
      
      column(
        br(),
        withSpinner(girafeOutput(ns("reading"))),
        withSpinner(girafeOutput(ns("listening"))),
        width = 6
      ),
      
      column(
        br(),
        withSpinner(girafeOutput(ns("writing"))),
        withSpinner(girafeOutput(ns("numeracy"))),
        width = 6
      )
    
  )
    
}

cfe_server <- function(input, output, session, data) {
  
  donut <- function(data, skill) {
    
    acel <- 
      data() %>%
      filter(dataset == "acel" 
             & str_starts(measure, skill) 
             & stage == "S3") %>%
      mutate(text = paste0(measure, ": ", value_label))
    
    plot <- 
      ggplot(acel, 
             aes(y = rev(value), fill = measure, tooltip = rev(text))) +
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
        label = 
          filter(acel, str_ends(measure, "% Meeting Level")) %>%
          pull(value_label),
        size = 12,
        color = "#3182bd"
      ) +
      scale_fill_manual(values = c("white", "#3182bd")) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(plot.title = element_text(size = 16, hjust = 0.5)) +
      ggtitle(skill)
    
    girafe(
      ggobj = plot,
      width_svg = 5,
      height_svg = 5,
      options = list(opts_toolbar(saveaspng = FALSE))
    )
    
  }
  
  output$reading <- renderGirafe({
    donut(data, "Reading")
  })
  
  output$writing <- renderGirafe({
    donut(data, "Writing")
  })
  
  output$listening <- renderGirafe({
    donut(data, "Listening")          
  })
  
  output$numeracy <- renderGirafe({
    donut(data, "Numeracy")        
  })
  
}