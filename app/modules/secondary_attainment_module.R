
secondary_attainment_ui <- function(id, year_options) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  max_year <- which.max(as.numeric(substr(year_options, 1, 4)))
  
  fluidRow(
    
    section_header_output("sec_attain_profile"),
    
           
    box(
      
            title = NULL,
      width = 12,
      collapsible = TRUE,
      
      
      
      column(h3("Use the menu below to select the attainment measure you wish to see"), width = 10),
      
      column(br(),
             download_data_ui(ns("download")),width = 2),
      
      
     
      
    fluidRow(
    column(h3(
      menuItem("Attainment measure selection", tabName = "dashboard",startExpanded = TRUE, collapsible = FALSE,
               menuSubItem("Ciriculum for Excellence", tabName = "CfE"),
               menuSubItem("Leavers' Breadth and Depth Profile", tabName = "level_awards"),
               menuSubItem("Percentage of School Leavers Gaining SCQF Credited Awards", tabName = "SCQF_awards"),
               menuSubItem("School Leavers Summary", tabName = "leavers_summary"),
               menuSubItem("School Leavers by SIMD", tabName = "leavers_SIMD"),
               menuSubItem("School Leavers Litteracy and Numeracy", tabName = "litteracy_and_numeracy"),colour = "navy")),width = 7)),


      #Dropdown Filte - for years
      
      column(selectInput(ns("year"), 
                         label = "Select year",
                         choices = year_options,
                         selected = year_options[max_year]), 
             width = 6, br(), br(),
      ),
      
      
      #Dropdown Filter - Attainment type
      tabItems(
        
        #BGE data by stage
        tabItem("CfE",title= "Ciriculum for Excellence",
                
                
                
                
                
                # Attainment BGE Doughnut Chart
                fluidRow(column(h3("Percentage of students meeting
              curriculum for excellence
              level"), width = 12)),
                fluidRow(column(br(),withSpinner(
                  girafeOutput(ns("donut_reading"))),width = 6),
                  column(br(),withSpinner(girafeOutput(ns("donut_writing"))),width = 6)),
                
                fluidRow(column(withSpinner(girafeOutput(ns("donut_listening"))),width = 6),
                         column(withSpinner(girafeOutput(ns("donut_numeracy"))),width = 6))), 
        
        
        
        
        
        
        #SCQF across levels and years
        tabItem("level_awards",title= "Percentage of Leavers Attainming 1 or more awards",
                
                #Dropdown Filte - Number of awards
                column(selectInput(ns("minimum_scqf_level"), 
                                   label = "Select the SCQF level you are interested in",
                                   choices = paste("SCQF Level", 
                                                   c("1", "2", "3", "4", "5",
                                                     "6", "7", "8", "9", "10"),
                                                   "or better"),
                                   selected = "1"), 
                       width = 12),
                
                
                br(),
                
                # Attainment BGE Bar Chart
                column(withSpinner(uiOutput(ns("scqf_level"))),
                       withSpinner(plotlyOutput(ns("breadth_depth"))), 
                       width = 12)       
                
        ),
        
        
        
        tabItem("SCQF_awards", title= "Percentage of School Leavers Gaining SCQF Credited Awards",
                
                #Title
                column(h3("Percentage of school leavers gaining SCQF credited awards"), width = 12),
                # Attainment breadth and depth tables         
                withSpinner(dataTableOutput(ns("breadth_depth_table"))),
                br(),
                h3("Percentage of school leavers gaining SCQF credited awards (virtual comparator)"),
                withSpinner(dataTableOutput(ns("breadth_depth_vc_table"))),
                
        ),
        
        
        
        
        tabItem("leavers_summary", title= "School Leavers Summary",
                
                
                column(h3("Percentage of leavers in a positive destination",
                          align = "center"),
                
                # Attainment leavers destination chart
                br(),withSpinner(plotlyOutput(ns("leavers_dest_chart"))), 
                       width = 12),
                
                # Attainment leavers total tariff chart
                column(h3("Percentage of leavers in a positive destination",
                          align = "center"),
                br(),withSpinner(plotlyOutput(ns("leavers_tariff_chart"))), 
                       width = 12)),
        
        
       
        
        
        
        tabItem("leavers_SIMD", title= "School Leavers by SIMD",
                column(width = 12),
                
                column(h3("School leavers attainment by SIMD", align = "center"),    
                
                # Attainment leavers deprivation chart
                br(),withSpinner(plotlyOutput(ns("leavers_deprivation_chart"))),width = 6,),
                
                # Attainment leavers SIMD chart
                
                column(h3("School leavers by SIMD", align = "center"), 
                br(),withSpinner(plotlyOutput(ns("leavers_simd_chart"))), width = 6)
        ),
        
        
        
        tabItem("litteracy_and_numeracy", title= "School Leavers Litteracy and Numeracy",
                
                br(),
                
                
                
                # Attainment literacy and numeracy chart
                column(h3("Percentage of school leavers' achieving Litteracy and Numeracy",
                          align = "center"),
                       withSpinner(plotlyOutput(ns("lit_num"))), 
                       width = 12),
                
                # Attainment literacy chart
                column(h3("Percentage of school leavers' achieving Litteracy",
                          align = "center"), 
                       withSpinner(plotlyOutput(ns("literacy"))),
                       width = 6),
                # Attainment numeracy chart          
                column(h3("Percentage of school leavers' achieving Numeracy",
                          align = "center"), 
                       withSpinner(plotlyOutput(ns("numeracy"))),
                       width = 6)
                
        )
        
        
      ))
  )
  
  

  
  
  
  
}

secondary_attainment_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "sec_attain_profile", "Secondary Attainment")
  
  callModule(download_data_server, "download", "Attainment Profile", data)
  
  
  
  #Breadth and depth output
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
        geom_text_repel(aes(label = paste(value_label,"%")),
                        na.rm = TRUE,
                        nudge_x = 0,
                        check_overlap = TRUE) +
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
  
  
  output$scqf_level <- renderUI({
    
    list(
      h3("Pupils achivening ", input$minimum_scqf_level, " by Year", align = "center"))
    
  })
  
  
  
  #breadth and depth tables 
  
  #school
  output$breadth_depth_table <- renderDataTable({
    
    table_data_1 <-
      data() %>%
      filter(dataset == "breadth_depth" & comparator == "0" & year == input$year) %>% 
      select(year, seed_code, la_code, school_name, minimum_scqf_level, `Minimum number of awards` = minimum_number_of_awards, value_label)%>% 
      mutate(`Minimum number of awards` = str_c(`Minimum number of awards`, " or more awards"), 
             minimum_scqf_level = str_c("SCQF level ", minimum_scqf_level, " or better")) %>%
      pivot_wider(names_from = "minimum_scqf_level", values_from = "value_label")}, 
    rownames = FALSE,
    options = list(dom = 't', columnDefs = list(list(targets = '_all', className = 'dt-center', orderable = FALSE),
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
    options = list(dom = 't', columnDefs = list(list(targets = '_all', className = 'dt-center', orderable = FALSE),
                                                list(targets = c(0,1,2,3), visible = FALSE)))
    
    
  )
  
  
  
  #Doughnut charts
  
  output$donut_reading <- renderGirafe({
    validate(
      need(input$year != "2019/20",
           "            Meeting curriculum for excellence level in reading was not collected in this year due to the impact of the COVID-19 pandemic"),
           need(input$year != "2020/21",
           "            Meeting curriculum for excellence level in reading was not collected in this year due to the impact of the COVID-19 pandemic" ))
    acel_data <-
      data() %>%
      filter(dataset == "acel" 
             & year == input$year 
             & str_starts (measure,"Reading") 
             & stage == "S3") %>%
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
        label = 
          filter(acel_data, str_ends(measure, "% Meeting Level")) %>%
          pull(value_label),
        size = 12,
        color = "#3182bd"
      ) +
      scale_fill_manual(values = c("white", "#3182bd")) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(plot.title = element_text(size = 16, hjust = 0.5)) +
      ggtitle(str_wrap(
        "Reading",
        width = 30))
    
    girafe(
      ggobj = plot,
      width_svg = 5,
      height_svg = 5,
      options = list(opts_toolbar(saveaspng = FALSE))
    )
    
  })
  
  
  
  output$donut_writing <- renderGirafe({
    validate(
      need(input$year != "2019/20",
           "            Meeting curriculum for excellence level in writing was not collected in this year due to the impact of the COVID-19 pandemic"),
      need(input$year != "2020/21",
           "            Meeting curriculum for excellence level in writing was not collected in this year due to the impact of the COVID-19 pandemic" ))
    acel_data <-
      data() %>%
      filter(dataset == "acel" 
             & year == input$year 
             & str_starts (measure,"Writing") 
             & stage == "S3") %>%
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
        label = 
          filter(acel_data, str_ends(measure, "% Meeting Level")) %>%
          pull(value_label),
        size = 12,
        color = "#3182bd"
      ) +
      scale_fill_manual(values = c("white","#3182bd")) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(plot.title = element_text(size = 16, hjust = 0.5)) +
      ggtitle(str_wrap(
        "Writing",
        width = 30))
    
    girafe(
      ggobj = plot,
      width_svg = 5,
      height_svg = 5,
      options = list(opts_toolbar(saveaspng = FALSE))
    )
    
  })   
  
  
  
  
  output$donut_listening <- renderGirafe({
    validate(
      need(input$year != "2019/20",
           "            Meeting curriculum for excellence level in listening was not collected in this year due to the impact of the COVID-19 pandemic"),
      need(input$year != "2020/21",
           "            Meeting curriculum for excellence level in listening was not collected in this year due to the impact of the COVID-19 pandemic" ))
    acel_data <-
      data() %>%
      filter(dataset == "acel" 
             & year == input$year 
             & str_starts (measure,"Listening") 
             & stage == "S3") %>%
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
        label = 
          filter(acel_data, str_ends(measure, "% Meeting Level")) %>%
          pull(value_label),
        size = 12,
        color = "#3182bd"
      ) +
      scale_fill_manual(values = c("white", "#3182bd")) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(plot.title = element_text(size = 16, hjust = 0.5)) +
      ggtitle(str_wrap(
        "Listening & Talking",
        width = 30))
    
    girafe(
      ggobj = plot,
      width_svg = 5,
      height_svg = 5,
      options = list(opts_toolbar(saveaspng = FALSE))
    )
    
  })               
  
  
  output$donut_numeracy <- renderGirafe({
    validate(
      need(input$year != "2019/20",
           "            Meeting curriculum for excellence level in numeracy was not collected in this year due to the impact of the COVID-19 pandemic"),
      need(input$year != "2020/21",
           "            Meeting curriculum for excellence level in numeracy was not collected in this year due to the impact of the COVID-19 pandemic" ))
    acel_data <-
      data() %>%
      filter(dataset == "acel" 
             & year == input$year 
             & str_starts (measure,"Numeracy") 
             & stage == "S3") %>%
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
        label = 
          filter(acel_data, str_ends(measure, "% Meeting Level")) %>%
          pull(value_label),
        size = 12,
        color = "#3182bd"
      ) +
      scale_fill_manual(values = c("white", "#3182bd")) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(plot.title = element_text(size = 16, hjust = 0.5)) +
      ggtitle(str_wrap(
        "Numeracy",
        width = 30))
    
    girafe(
      ggobj = plot,
      width_svg = 5,
      height_svg = 5,
      options = list(opts_toolbar(saveaspng = FALSE))
    )
    
  })               
  
  
  #Leavers destinations bar chart
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
  
  
  # Leavers tariff chart <- renderPlotly({
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
  
  
  
  # Leavers deprivation chart <- renderPlotly({
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
  
  
  
  # Leavers by SIMD chart <- renderPlotly({
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
  
  # Leavers literacy and numeracy chart<- renderPlotly({
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
  
  # Leavers literacy chart<- renderPlotly({
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
                        ifelse(comparator == 0,
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
  
  # Leavers numeracy chart <- renderPlotly({
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