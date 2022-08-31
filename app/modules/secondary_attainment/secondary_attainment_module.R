
secondary_attainment_ui <- function(id, year_options) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  max_year <- which.max(as.numeric(substr(year_options, 1, 4)))
  
  tagList(
    
    section_header_output(ns("sec_attain_profile")),
           
    box(
      
      title = NULL,
      width = 12,
      collapsible = TRUE,
      
      column(
        selectInput(
          inputId = ns("measure"),
          label = "Select attainment measure",
          choices = c(
            "Curriculum for Excellence",
            "Leavers Breadth and Depth Profile",
            "Percentage of School Leavers Gaining SCQF Credited Awards",
            "School Leavers Summary",
            "School Leavers by SIMD",
            "School Leavers Literacy and Numeracy"
          )
        ),
        width = 5
      ),
      
      column(
        
        conditionalPanel(
          condition = "input.measure != 'Leavers Breadth and Depth Profile'",
          selectInput(ns("year"), 
                      label = "Select year",
                      choices = year_options,
                      selected = year_options[max_year]),
          ns = ns
        ),
        
        conditionalPanel(
          condition = "input.measure == 'Leavers Breadth and Depth Profile'",
          selectInput(
            ns("minimum_scqf_level"),
            label = "Select minimum SCQF level",
            choices = paste("SCQF Level", 1:10, "or better")
          ),
          ns = ns
        ),
        
        width = 5
      ),

      column(
        br(),
        download_data_ui(ns("download")),
        width = 2
      ),
      
      # Curriculum for Excellence
      
      conditionalPanel(
        condition = "input.measure == 'Curriculum for Excellence'",
        
        conditionalPanel(
          condition = "['2019/20', '2020/21'].includes(input.year)",
          column(
            h4("Curriculum for Excellence performance data was not collected ",
               "in 2019/20 or 2020/21 due to the impact of the ",
               "COVID-19 pandemic."),
            br(),
            br(),
            br(),
            width = 12
          ),
          ns = ns
        ),

        conditionalPanel(
          condition = "!['2019/20', '2020/21'].includes(input.year)",
          cfe_ui(ns("cfe")),
          ns = ns
        ),
        
        ns = ns
      
      ),
      
      # Leavers' Breadth and Depth Profile
      
      conditionalPanel(
        condition = "input.measure == 'Leavers Breadth and Depth Profile'",
        
        # Attainment BGE Bar Chart
        column(
          withSpinner(uiOutput(ns("scqf_level"))),
          withSpinner(plotlyOutput(ns("breadth_depth"))),
          width = 12
        ),
        
        ns = ns
      ),
      
      # Percentage of School Leavers Gaining SCQF Credited Awards
      
      conditionalPanel(
        condition = "input.measure == 'Percentage of School Leavers Gaining SCQF Credited Awards'",
        
        column(
          
          h3("Percentage of school leavers gaining SCQF credited awards"), 
          withSpinner(dataTableOutput(ns("breadth_depth_table"))),
          
          br(),
          
          h3("Percentage of school leavers gaining SCQF ",
             "credited awards (virtual comparator)"),
          withSpinner(dataTableOutput(ns("breadth_depth_vc_table"))),
          
          width = 12
        ),
        
        ns = ns
      ),
      
      # School Leavers Summary
      
      conditionalPanel(
        condition = "input.measure == 'School Leavers Summary'",
        
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
          
          width = 12),

        ns = ns
      ),
      
      # School Leavers by SIMD
      
      conditionalPanel(
        condition = "input.measure == 'School Leavers by SIMD'",
        
        # Attainment leavers deprivation chart
        column(
          h3("School leavers attainment by SIMD", align = "center"),
          br(),
          withSpinner(plotlyOutput(ns("leavers_deprivation_chart"))),
          width = 6
        ),
        
        # Attainment leavers SIMD chart
        column(
          h3("School leavers by SIMD", align = "center"),
          br(),
          withSpinner(plotlyOutput(ns("leavers_simd_chart"))), 
          width = 6
        ),
        
        ns = ns
      ),
      
      # School Leavers Literacy and Numeracy
      
      conditionalPanel(
        condition = "input.measure == 'School Leavers Literacy and Numeracy'",
        
        # Attainment literacy and numeracy chart
        column(
          h3("Percentage of school leavers' achieving Literacy and Numeracy",
             align = "center"),
          withSpinner(plotlyOutput(ns("lit_num"))),
          width = 12
        ),
        
        # Attainment literacy chart
        column(
          h3("Percentage of school leavers' achieving Literacy",
             align = "center"),
          withSpinner(plotlyOutput(ns("literacy"))),
          width = 6
        ),
        
        # Attainment numeracy chart
        column(
          h3("Percentage of school leavers' achieving Numeracy",
             align = "center"),
          withSpinner(plotlyOutput(ns("numeracy"))),
          width = 6
        ),
        
        ns = ns
      )
      
    )
    
  )
  
}

secondary_attainment_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "sec_attain_profile", "Secondary Attainment")
  
  callModule(download_data_server, "download", "Attainment Profile", data)
  
  data_filtered <- reactive({
    data() %>% filter(year == input$year)
  })
  
  # Curriculum for Excellence ----
  callModule(cfe_server, "cfe", data_filtered)
  
  # Leavers Breadth and Depth Profile ----
  
  output$scqf_level <- renderUI({
      h3("Pupils achieving ", input$minimum_scqf_level, " by Year", 
         align = "center")
  })
  
  output$breadth_depth <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(dataset == "breadth_depth"
               & minimum_scqf_level == str_extract(input$minimum_scqf_level, "\\d")
               & minimum_number_of_awards == "1") %>% 
        mutate(comparator = ifelse(comparator=="0","School/Area","VC"),
               minimum_scqf_level = str_c("SCQF level ",minimum_scqf_level, " or better")) %>%
        ggplot(aes(year, 
                   value, 
                   group = 1,
                   colour = comparator,
                   text = paste0("Year: ", year, "<br>",
                                 value_label, "%"))) + 
        geom_line() +
        scale_color_manual(values=c("#3182bd", "#9ecae1")) +
        scale_y_continuous(limits = c(0,NA)) + 
        theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1)) +
        labs(x = "Academic Year", y = "% of Leavers", fill = NULL),
      tooltip = "text"
    ) %>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  
  # Percentage of School Leavers Gaining SCQF Credited Awards ----
  
  #school
  output$breadth_depth_table <- renderDataTable({
    
    table_data_1 <-
      data() %>%
      filter(dataset == "breadth_depth" & comparator == "0" & year == input$year) %>% 
      select(year, seed_code, la_code, school_name, minimum_scqf_level, 
             `Minimum number of awards` = minimum_number_of_awards, 
             value_label) %>% 
      mutate(
        `Minimum number of awards` = str_c(`Minimum number of awards`, " or more awards"), 
        minimum_scqf_level = str_c("SCQF level ", minimum_scqf_level, " or better")
      ) %>%
      pivot_wider(names_from = "minimum_scqf_level", values_from = "value_label")}, 
    
    rownames = FALSE,
    options = list(dom = 't', 
                   columnDefs = list(list(targets = '_all', className = 'dt-center', orderable = FALSE),
                                     list(targets = c(0,1,2,3), visible = FALSE)))
    
  )
  
  #comparator
  output$breadth_depth_vc_table <- renderDataTable({
    
    table_data_2 <-
      data() %>%
      filter(dataset == "breadth_depth" & comparator == "1" & year == input$year) %>% 
      select(year, seed_code, la_code, school_name, minimum_scqf_level, `Minimum number of awards` = minimum_number_of_awards, value_label) %>% 
      mutate(`Minimum number of awards` = str_c(`Minimum number of awards`, " or more awards"), 
             minimum_scqf_level = str_c("SCQF level ", minimum_scqf_level, " or better")) %>%
      pivot_wider(names_from = "minimum_scqf_level", values_from = "value_label")}, 
    
    rownames = FALSE,
    options = list(dom = 't', 
                   columnDefs = list(list(targets = '_all', className = 'dt-center', orderable = FALSE),
                                     list(targets = c(0,1,2,3), visible = FALSE)))
    
  )
  
  
  # School Leavers Summary ----
  
  # Leavers destinations bar chart
  output$leavers_dest_chart <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(dataset == "destinations" & year == input$year
        ) %>%
        mutate(comparator = ifelse(comparator=="0","School/Area","VC")) %>%
        ggplot(aes(
          comparator,
          value,
          fill = comparator,
          text = paste0("School: ",
                        ifelse(comparator == "School/Area",
                               school_name,
                               "Virtual Comparator"),
                        "<br>",
                        "Value: ", value_label)
        )) +
        geom_col(colour = NA) +
        scale_fill_manual(values=c("#3182bd", "#9ecae1")) +
        labs(x = NULL , y = "% of Leavers", fill = NULL) +
        scale_x_discrete(labels = c("0" = "School/Area",
                                    "1" = "Virtual Comparator")), 
      tooltip = "text"
    )%>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  
  # Leavers tariff chart
  output$leavers_tariff_chart <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(dataset == "attainment_for_all" & year == input$year) 
      %>%
        mutate(comparator = ifelse(comparator=="0","School/Area","VC")) %>%
        ggplot(aes(
          measure,
          value,
          fill = comparator,
          text = paste0("School: ",
                        ifelse(comparator == "School/Area",
                               school_name,
                               "Virtual Comparator"),
                        "<br>",
                        "Value: ", value_label)
        )) +
        geom_col(width = 0.8, position = "dodge", stat="identity", colour = NA) +
        scale_fill_manual(values=c("#3182bd", "#9ecae1")) +
        labs(x = NULL , y = "Total Tariff Score", fill = NULL) +
        scale_x_discrete(labels = c("average_total_tariff_lowest_20_percent" = "Lowest 20%",
                                    "average_total_tariff_middle_60_percent" = "Middle 60%",
                                    "average_total_tariff_highest_20_percent" = "Highest 20%")) +
        theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1)) ,
      tooltip = "text"
    )%>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  
  # School Leavers by SIMD ----
  
  # Leavers deprivation chart
  output$leavers_deprivation_chart <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(dataset == "attainment_by_deprivation" & year == input$year
               & str_ends(measure, "total_tariff"))
      %>%
        mutate(comparator = ifelse(comparator=="0","School/Area","VC")) %>%
        ggplot(aes(
          measure,
          value,
          fill = comparator,
          text = paste0("School: ",
                        ifelse(comparator == "School/Area",
                               school_name,
                               "Virtual Comparator"),
                        "<br>",
                        "Value: ", value_label)
        )) +
        geom_col(width = 0.8, position = "dodge", stat="identity", colour = NA) +
        scale_fill_manual(values=c("#3182bd", "#9ecae1")) +
        labs(x = "SIMD Grouping" , y = "Total Tariff Score" , fill = NULL) +
        scale_x_discrete(labels = c("quintile_1_average_total_tariff" = "Q1",
                                    "quintile_2_average_total_tariff" = "Q2",
                                    "quintile_3_average_total_tariff" = "Q3",
                                    "quintile_4_average_total_tariff" = "Q4",
                                    "quintile_5_average_total_tariff" = "Q5")),
      tooltip = "text"
    )%>%
      config(displayModeBar =F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  # Leavers by SIMD chart
  output$leavers_simd_chart <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(dataset == "attainment_by_deprivation" & year == input$year
               & str_ends(measure, "of_leavers"))
      %>%
        
        mutate(comparator = ifelse(comparator=="0","School/Area","VC")) %>%
        ggplot(aes(
          measure,
          value,
          fill = comparator,
          text = paste0("School: ",
                        ifelse(comparator == "School/Area",
                               school_name,
                               "Virtual Comparator"),
                        "<br>",
                        "Value: ", value_label)
        )) +
        geom_col(width = 0.8, position = "dodge", stat="identity", colour = NA) +
        scale_fill_manual(values=c("#3182bd", "#9ecae1")) +
        labs(x = "SIMD Grouping", y = "Number of Leavers", fill = NULL) +
        scale_x_discrete(labels = c("quintile_1_percent_of_leavers" = "Q1",
                                    "quintile_2_percent_of_leavers" = "Q2",
                                    "quintile_3_percent_of_leavers" = "Q3",
                                    "quintile_4_percent_of_leavers" = "Q4",
                                    "quintile_5_percent_of_leavers" = "Q5"
        )) ,
      tooltip = "text"
    )%>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  
  # School Leavers Literacy and Numeracy ----
  
  # Leavers literacy and numeracy chart
  output$lit_num <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(dataset == "literacy_numeracy" & year == input$year
               & str_starts(measure, "percentage_achieving_literacy_and_numeracy"))
      %>%
        mutate(comparator = ifelse(comparator=="0","School/Area","VC")) %>%
        ggplot(aes(
          measure,
          value,
          fill = comparator,
          text = paste0("School: ",
                        ifelse(comparator == "School/Area",
                               school_name,
                               "Virtual Comparator"),
                        "<br>",
                        "Value: ", value_label)
        )) +
        geom_col(width = 0.8, position = "dodge", stat="identity", colour = NA) +
        scale_fill_manual(values=c("#3182bd", "#9ecae1")) +
        labs(x = NULL , y = "% Achieving", fill = NULL) +
        scale_x_discrete(labels = c("percentage_achieving_literacy_and_numeracy_at_level_4_or_better" = 
                                      "Level 4 or Better",
                                    "percentage_achieving_literacy_and_numeracy_at_level_5_or_better" = 
                                      "Level 5 or Better")) ,
      tooltip = "text"
    )%>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  # Leavers literacy chart
  output$literacy <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(dataset == "literacy_numeracy" & year == input$year
               & str_starts(measure, "percentage_achieving_literacy_at"))
      %>%
        mutate(comparator = ifelse(comparator=="0","School/Area","VC")) %>%
        ggplot(aes(
          measure,
          value,
          fill = comparator,
          text = paste0("School: ",
                        ifelse(comparator == "School/Area",
                               school_name,
                               "Virtual Comparator"),
                        "<br>",
                        "Value: ", value_label)
        )) +
        geom_col(width = 0.8, position = "dodge", stat="identity", colour = NA) +
        scale_fill_manual(values=c("#3182bd", "#9ecae1")) +
        labs(x = NULL , y = "% Achieving", fill = NULL) +
        scale_x_discrete(labels = c("percentage_achieving_literacy_at_level_4_or_better" = 
                                      "Level 4 or Better",
                                    "percentage_achieving_literacy_at_level_5_or_better" = 
                                      "Level 5 or Better")) ,
      tooltip = "text"
    )%>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  }) 
  
  # Leavers numeracy chart
  output$numeracy <- renderPlotly({
    
    ggplotly(
      data() %>%
        filter(dataset == "literacy_numeracy" & year == input$year
               & str_starts(measure, "percentage_achieving_numeracy"))
      %>%
        mutate(comparator = ifelse(comparator=="0","School/Area","VC")) %>%
        ggplot(aes(
          measure,
          value,
          fill = comparator,
          text = paste0("School: ",
                        ifelse(comparator == "School/Area",
                               school_name,
                               "Virtual Comparator"),
                        "<br>",
                        "Value: ", value_label)
        )) +
        geom_col(width = 0.8, position = "dodge", stat="identity", colour = NA) +
        scale_fill_manual(values=c("#3182bd", "#9ecae1")) +
        labs(x = NULL , y = "% Achieving", fill = NULL) +
        scale_x_discrete(labels = c("percentage_achieving_numeracy_at_level_4_or_better" = 
                                      "Level 4 or Better",
                                    "percentage_achieving_numeracy_at_level_5_or_better" = 
                                      "level 5 or Better")) ,
      tooltip = "text"
    )%>%
      config(displayModeBar = F, responsive = FALSE) %>% 
      
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
  })

  
}