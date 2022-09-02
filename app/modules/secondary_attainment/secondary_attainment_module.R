
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
            "Leavers Breadth and Depth Profile",
            "Curriculum for Excellence",
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
      
      # Leavers' Breadth and Depth Profile
      
      conditionalPanel(
        condition = "input.measure == 'Leavers Breadth and Depth Profile'",
        breadth_depth_ui(ns("breadth_depth")),
        ns = ns
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
      
      # Percentage of School Leavers Gaining SCQF Credited Awards
      conditionalPanel(
        condition = "input.measure == 'Percentage of School Leavers Gaining SCQF Credited Awards'",
        leaver_awards_ui(ns("leaver_awards")),
        ns = ns
      ),

      # School Leavers Summary
       
      conditionalPanel(
        condition = "input.measure == 'School Leavers Summary'",
        leaver_summary_ui(ns("leaver_summary")),
        ns = ns
      ),
      
      # School Leavers by SIMD
      conditionalPanel(
        condition = "input.measure == 'School Leavers by SIMD'",
        leaver_simd_ui(ns("leaver_simd")),
        ns = ns
      ),
      
      # School Leavers Literacy and Numeracy
      conditionalPanel(
        condition = "input.measure == 'School Leavers Literacy and Numeracy'",
        leaver_lit_num_ui(ns("leaver_lit_num")),
        ns = ns
      ),
      
      column(br(), width = 12)
      
    )
    
  )
  
}

secondary_attainment_server <- function(input, output, session, data) {
  
  callModule(section_header_server, "sec_attain_profile", "Secondary Attainment")
  
  callModule(download_data_server, "download", "Attainment Profile", data)
  
  data_year <- reactive({
    data() %>% filter(year == input$year)
  })
  
  data_scqf <- reactive({
    data() %>%         
      filter(dataset == "breadth_depth"
             & minimum_scqf_level == str_extract(input$minimum_scqf_level, "\\d")
             & minimum_number_of_awards == "1")
  })
  
  # Leavers Breadth and Depth Profile ----
  callModule(breadth_depth_server, "breadth_depth", data_scqf)
  
  # Curriculum for Excellence ----
  callModule(cfe_server, "cfe", data_year)
  
  # Percentage of School Leavers Gaining SCQF Credited Awards ----
  callModule(leaver_awards_server, "leaver_awards", data_year)
   
  # School Leavers Summary ----
  callModule(leaver_summary_server, "leaver_summary", data_year)

  # School Leavers by SIMD ----
  callModule(leaver_simd_server, "leaver_simd", data_year)
  
  # School Leavers Literacy and Numeracy ----
  callModule(leaver_lit_num_server, "leaver_lit_num", data_year)
  
}