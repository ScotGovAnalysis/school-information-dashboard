
primary_attainment_ui <- function(id, year_options) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  max_year <- which.max(as.numeric(substr(year_options, 1, 4)))
  
  tagList(
    
    section_header_output(ns("title_box")),
    
    box(
      
      title = NULL,
      width = 12,
      collapsible = TRUE,
     
      # Dropdown Filter - Attainment Year
      column(
        selectInput(ns("year"), 
                    label = "Select year",
                    choices = year_options,
                    selected = year_options[max_year]), 
        width = 3
      ),
      
      # Dropdown Filter - Attainment Measure
      column(
        selectInput(ns("measure"), 
                    label = "Select attainment measure",
                    choices = c("Reading", 
                                "Listening & Talking",
                                "Numeracy", 
                                "Writing"),
                    selected = "Reading"), 
        width = 4
      ),
      
      # Dropdown Filter - Attainment Stage
      column(
        selectInput(ns("stage"), 
                    label = "Select stage",
                    choices = c("P1", "P4", "P7", "P1, P4 & P7 combined"),
                    selected = "P4"), 
        width = 3
      ),
      
      column(
        br(),
        download_data_ui(ns("download")),
        width = 2
      ),

      # Attainment BGE Bar Chart
      column(
        br(),
        h3("Average Curriculum for Excellence Level Achieved",
           align = "center"),
        withSpinner(plotlyOutput(ns("bar_chart"))), 
        width = 7
      ), 
      
      # Attainment BGE Doughnut Chart
      column(
        br(),
        h3("Percentage of Students Meeting Curriculum for Excellence Level",
           align = "center"),
        withSpinner(girafeOutput(ns("donut_plot"))), 
        width = 4
      )
      
    )
    
  )

}

primary_attainment_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "title_box", "Primary Attainment")
  
  callModule(download_data_server, "download", "Attainment Profile", data)
  
  output$bar_chart <- renderPlotly({
    
    # Display error message when P1, P4 & P7 combined is selected
    validate(
      need(
        input$stage != "P1, P4 & P7 combined",
        message = paste(
          "Average curriculum for excellence level achieved",
          "is not recorded for P1, P4 & P7 combined."
        )
      )
    )
    
    dat <-
      data() %>%
      filter(dataset == "bge" & 
               year == input$year &
               measure == input$measure & 
               stage == input$stage) %>%
      mutate(comparator = 
               ifelse(comparator == 0, school_name, "Virtual Comparator")
      )
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")
    
    plot <-
      ggplot(dat, aes(x = comparator, 
                      y = value,
                      text = paste("School:", comparator)
      )) + 
      geom_col() +
      geom_text(aes(y = value + 0.5, label = chart_label), hjust = 0.5) +
      labs(x = NULL , y = NULL) +
      scale_y_continuous(limits = c(0, 4),
                         labels = c("Not yet early level",
                                    "Early level",
                                    "1st level",
                                    "2nd level",
                                    "3rd level or better"))
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE))
      
    
  })  

  output$donut_plot <- renderGirafe({
    
    req(nrow(data()) > 0)

    dat <-
      data() %>%
      filter(dataset == "acel" & 
               year == input$year &
               str_starts(measure, input$measure) & 
               stage == input$stage) %>%
      mutate(text = paste0(measure, ": ", value_label))
    
    # Display error message if no data returned
    validate(need(nrow(dat) > 0, label = "data"), errorClass = "no-data")
    
    plot <- 
      ggplot(dat, aes(y = rev(value), 
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
        label = 
          filter(dat, str_ends(measure, "% Meeting Level")) %>%
            pull(value_label),
        size = 12,
        color = "#3182bd"
      ) +
      scale_fill_manual(values = c("white", "#3182bd")) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
    
    girafe(
      ggobj = plot,
      width_svg = 5,
      height_svg = 5,
      options = list(opts_toolbar(saveaspng = FALSE))
    )
    
  })

}