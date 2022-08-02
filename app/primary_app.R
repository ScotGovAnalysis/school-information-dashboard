
### 0 - Set up ----

source(here::here("app", "00_shiny_setup.R"))


### 1 - Load data files ----

school_profile <- read_rds(
  here("output", shiny_run_label, 
       paste0(school_type, "_school_profile.rds"))
) %>%
  mutate(across(c(roll, fte_teacher_numbers), ~ prettyNum(., big.mark = ",")))

attendance <- read_rds(
  here("output", shiny_run_label,
       paste0(school_type, "_attendance.rds"))
) %>%
  mutate(value_label = prettyNum(value_label, big.mark = ","))

population <- read_rds(
  here("output", shiny_run_label,
       paste0(school_type, "_population.rds"))
) %>%
  mutate(value_label = prettyNum(value_label, big.mark = ",")) %>%
  mutate(measure = factor(
    measure,
    c("Pupil Numbers", "Teacher Numbers (FTE)", "Pupil Teacher Ratio",
      "Average Class", "Female", "Male", paste0("P", 1:7), 
      paste0("SIMD Q", 1:5), "SIMD Unknown", 
      "ASN", "No ASN", "FSM", "No FSM", "EAL", "No EAL", 
      "White UK", "White Other", "Ethnic Minority", "Ethnicity Not Known",
      "Taught in Gaelic", "Not Taught in Gaelic",
      "Urban", "Small Town", "Rural", "Urban Rural Not Known")
  ))
      
attainment <- read_rds(
  here("output", shiny_run_label,
       paste0(school_type, "_attainment.rds"))
) %>%
  mutate(value_label = prettyNum(value_label, big.mark = ","))

FAQ <- 
  read_excel(here("lookups", "FAQ.xlsx")) %>%
  select(Section, Question, Notes)


# Read in school lookup data - REMOVE THIS?
school_lookup <-
  read_rds(here("output", shiny_run_label, "school_lookup.rds")) %>%
  filter(school_type == "Primary") %>%
  arrange(as.numeric(la_code), nchar(seed_code), school_name)


# TEMP - Read in data with lat/long 
# (this will eventually be added to school_profile)
dat <- 
  read_rds(here("map", "lat_long_data_schools.rds")) %>%
  right_join(school_lookup, by = "seed_code")


# Set app language 
# tags$script(HTML("<script html lang= en ></script>"))


### 2 - UI ----

## The UI is the main body of app - it controls mostly what the app looks and 
## functions like when interacting with it.

ui <- 

  # 2 - UI - Dashboard title and header ----
    
  dashboardPage(
    
    title = "Primary School Information Dashboard",
    
    skin = "blue",
                    
    dashboardHeader(
      
      title = h1("Primary School Information Dashboard"), 
      titleWidth = NULL, 
      disable = TRUE
      
    ),
          
    # 2 - UI - Sidebar ----
    
    dashboardSidebar(
      
      tags$script(
        JS("document.getElementsByClassName('sidebar-toggle')[0]",
           ".style.visibility = 'hidden';")
      ),

      useShinyjs(),
      
      # Set the colour of the side bar          
      tags$style(
        HTML(".main-sidebar.main-sidebar-solid.main-sidebar-primary>",
             ".main-sidebar-header {color:white; background:#100f3a}",
             ".skin-blue .main-sidebar {background-color: #100f3a;}")
      ),
      
      # Text instruction to use dropdown filers
      h2("Select options from the drop downs below", align = "center"),
      
      # Dropdown Filter - Local Authority
      
      selectInput(
        inputId = "la",
        label = "Local Authority",
        choices = unique(school_lookup$la_name),
        selected = "Scotland",
        multiple = FALSE
      ),
      
      # Dropdown Filter - School
      
      selectInput(
        inputId = "school",
        label = "School",
        choices = NULL,
        selected = "All publicly funded schools",
        multiple = FALSE
      ),
      
      # Dropdown Filter - Measure
      
      selectInput(
        inputId = "measure",
        label = "Measure",
        choices = NULL,
        selected = "Select",
        multiple = FALSE
      ),

      # Text instruction to click on boxes for further info
      h3("Click on any box for more information", align = "center"),
      
      br(),
      br(),
      br(),
      
      HTML(paste0("<center>", 
                  img(src = "smarter-scotland.jpg", width = 200),
                  "</center>")),
      
      disable = FALSE, 
      width = NULL, 
      collapsed = FALSE
      
    ),
    
    
    # 2 - UI - Main dashboard body ----

    dashboardBody(
      
      # 2 - UI - School profile ----
      
      fluidRow(
        
        # School Profile Title Box 
        valueBox(paste(school_type, "School Information Dashboard"), 
                 tags$p(h4(textOutput("la_school_title"))),
                 color = "yellow",
                 width = 12),
        
        # School Profile Content Box
        
        box(
          
          column(
            
            width = 4,
            
            map_output("map"),
            br(),
          
            fluidRow(
              
              # Button to click for further COVID-19 information
              actionButton(
                "COVID", 
                "COVID-19", 
                style = "color: white; background-color: purple; border-color:purple",
                width = "49%"),
              
              # Button to click for FAQs
              actionButton(
                "showTable",
                "FAQ's",
                style="color: white; background-color: purple; border-color:purple",
                width = "49%"),
              
              # Popup window to display FAQs
              bsModal("modalExample", "Data Table", "showTable", size = "large",
                      selectInput("section",
                                  "Section:",
                                  c("Select",
                                    unique(as.character(FAQ$Section)))),
                      DT::dataTableOutput("table")),
            )
            
          ),
          
          # School Profile Text
          column(
            school_profile_output("primary_school_profile"),
            width = 8),
          
          width = 12,
          collapsible = FALSE),
          
          
        # School Profile Value Boxes 
        valueBoxOutput('attendance', width = 4),
        valueBoxOutput('adver_class', width = 4),
        valueBoxOutput('pe', width = 4),
        valueBoxOutput('pup_num', width = 4),
        valueBoxOutput('teach_num', width = 4),
        valueBoxOutput('ptr', width = 4),
        
       
      ),
      
      # 2 - UI - Pupil Profile ---- 
      
      fluidRow(
        
        # Pupil Profile Title Box
        section_header_ui("pupil_header"),
        
        # Pupil Profile Content Box 
        box(
          title = NULL,
          width = 12,
          collapsible = FALSE,
          
          column(plotlyOutput('pup_pop_graph'), width = 12),
          column(plotlyOutput('pup_pop_graph2'), width = 12),
          
         
        ),
        
      ),
      
      
      
      # 2 - UI - Measure Options ----
      
      fluidRow(
        
        # 2 - UI - Attendance ----
        
        # Attendance Title Box 
        section_header_ui("attendance_header"),
        
        # Attendance Content Box
        box(
          title = NULL,
          width = 12,
          collapsible = FALSE,
          
          # Dropdown Filter - Attendance Measure
          selectInput("att_var", 
                      label = "Select attadance measure",
                      choices = c("Attendance", "Authorised Absence",
                                  "Unauthorised Absence"),
                      selected = "Attendance"),
          
          # Attendance Trend Line Chart
          column(plotlyOutput('attendance_graph'), width = 7),
          
          # Attendance Stage Bar Chart
          column(plotlyOutput('attendance_stage_graph'), width = 5)
          
        ),      
        
        # 2 - UI - Attainment ----
        
        # Attainment Title Box
        section_header_ui("attainment_header"),
        
        # Attainment Content Box
        box(
          title = NULL,
          width = 12,
          collapsible = TRUE,
          
         
          # Dropdown Filter - Attainment Year
          column(selectInput("attainment_year", 
                      label = "Select year",
                      choices = c("2016/17", "2017/18",
                                  "2018/19", "2020/21"),
                      selected = "2020/21"), width = 4),
          
          # Dropdown Filter - Attainment BGE Measure
          column(selectInput("attainment_bge", 
                             label = "Select attainment measure",
                             choices = c("Reading", "Listening & Talking",
                                         "Numeracy", "Writing"),
                             selected = "Reading"), width = 4),
          
          # Dropdown Filter - Attainment Stage
          column(selectInput("attainment_stage", 
                             label = "Select stage",
                             choices = c("P1", "P4", "P1, P4 & P7 combined",
                                         "P7"),
                             selected = "P4"), width = 4),
          
          # Attainment BGE Bar Chart
          column(h3("Adverage curriculum for excellence level achieved", align = "center"),
                 plotlyOutput('attainment_bar_graph'), width = 7),
          
          # Attainment BGE Doughnut Chart
          column(h3("Precentage of students meeting curriculum for excellence level",align = "center"),
            ggiraphOutput("donut_plot"), width = 4),
          
        ),      
        
        
        
        # 2 - UI - Population ----
        
        # Population Title Box 
        section_header_ui("population_header"),
        
        # Population Content Box 
        box(
          title = NULL,
          width = 12,
          collapsible = FALSE,
          
          # Dropdown Filter - Population Measure
          selectInput("pop_var", 
                      label = "Select population measure",
                      choices = c("Pupil Numbers", "Teacher Numbers (FTE)",
                                  "Pupil Teacher Ratio", "Average Class"),
                      selected = "Pupil Numbers"),
          
          # Population Trend Line Chart
          plotlyOutput('population_graph'),
          
        ),   
        
      ),
      
    )
    
  )


### 3 - Server ----

server <- function(input, output, session) {
  
  callModule(section_header, "pupil_header", "Pupil", box_colour = "navy")
  callModule(section_header, "attendance_header", "Attendance")
  callModule(section_header, "attainment_header", "Attainment")
  callModule(section_header, "population_header", "Population")
  
    
  # Introduction Popup ----
  
  intro_modal <- 
    modalDialog(
      title = "Introduction",
      p("The School Information Dashboards bring together a range of published information. "),
      p("There are three dashboards -  Primary , Secondary and Special School Dashboards."),
      p("The School Information Dashboards add to the information already provided by schools. 
      This information is designed to help better understand schools and encourage communication between parents/carers and schools."),  
      p("It is important to remember that statistical data alone is not a measure of how well a school is doing. All schools are unique. 
      To understand how well a school is doing it is important to look at a range of different data sources."),
      uiOutput("nif_link"),
      br(),
      p("If you have questions about the information on the dashboard for a particular school, then you should contact that school directly. 
      School contact details are available on the dashboard. School website details are available on Parentzone Scotland."),
      easyClose = TRUE,
      footer = modalButton("OK")
    )
  
  #National Improvement Framework in a Nutshell link
  url <- a("National Improvement Framework in a Nutshell - National Parent Forum of Scotland", 
           href="https://www.npfs.org.uk")
  
  output$nif_link <- renderUI({
    tagList("For more detail on the importance of gathering and sharing accurate information follow this link:", url)
  })

  # Create popup intro modal
  showModal(intro_modal)

  
  # FAQ Button Functions ----
  
  choices_FAQs <- reactive({
    choices_FAQs <- FAQ %>%
      arrange(desc(Section)) 
    
  })
  
  observe({
    updateSelectInput(session = session, inputId = "Section", choices = choices_FAQs())
  })
  
  # Table format   
  output$table <- 
    DT::renderDataTable(DT::datatable({
      data <- FAQ
      if (input$section != "ALL") {
        data <- data[data$Section == input$section, -1]#remove the row numbering
      }
    }, rownames = FALSE))
  
  
  # School Filter Dropdown - Updated based on LA selected ----
  
  observeEvent(
    input$la,
    {updateSelectizeInput(session, 
                          input = "school",
                          choices = 
                            school_lookup %>% 
                            filter(la_name %in% input$la) %>%
                            pull(school_name))
    })
  
  
  # Measure Filter Dropdown
  # Doesn't do anything just now - placeholder for when measure filter 
  # operational
  
  observeEvent(
    input$la, {
      updateSelectizeInput(session, 
                           input = "measure",
                           choices = c("Attendance Profile", 
                                       "Population Profile", 
                                       "Attainment Profile")
      )}
  )
  
  
  ## COVID-19 Popup ----
  
  observeEvent(input$COVID, {
    showModal(modalDialog(
      title = "COVID-19",
      p("Impact on data collection: The Scottish Government did not collect Achievement of CfE Levels data for any pupils in 2019/20."),
      p("Schools were closed in Scotland in March 2020 and January 2021 as a result of the pandemic. This is likely to have had a 
        negative effect on some pupils' progress and attainment. Attainment of socio-economically deprived children may have been 
        amongst those most negatively affected."),
      p("It is therefore likely to have had an impact on the CfE levels some children have achieved. This will be reflected in the 
        2020/21 figures and should be kept in mind when interpreting these. In particular, when comparing with figures for 2018/19 and 
        before."),
      uiOutput("acel_link")
    )
    ) 
  }) 
  
  # Attainment, leaver destination and healthy living link
  url <- a("Attainment, leaver destinations and healthy living: summary statistics", 
           href="https://www.gov.scot/publications/attainment-leaver-destinations-healthy-living-summary-statistics/pages/2/")
  
  output$link <- renderUI({
    tagList("More detail is available using this link here:", url)
  })
  
  # ACEL data link
  url <- a("Scottish Exchange of Data: achievement of Curriculum for Excellence levels", 
           href="https://www.gov.scot/publications/scottish-exchange-of-data-achievement-of-curriculum-for-excellence-levels")
  
  output$acel_link <- renderUI({
    tagList("More detail is available using this link here:", url)
  })
  
  
  ## Map ----
  
  # Filter map data based on LA dropdown
  # This will need to be updated when the lat/long data is moved to school_profile
  
  filter_type <- reactive({
    dat %>%
      filter(la_name == input$la & school_name == input$school)
  })
  
  # Plot the postcode locations on a map
  
  callModule(map, "map", filter_type)
  # output$map <- renderLeaflet({
  #   leaflet() %>% 
  #     addTiles() %>% 
  #     setView(-4.140078, 57.837990, zoom = 5.3) %>%
  #     addCircleMarkers(
  #       data = filter_type(),
  #       lng = ~ lng, lat = ~ lat,
  #       radius = 6,
  #       stroke = FALSE,
  #       opacity = 1,
  #       fillOpacity = 1,
  #       color = "navy")
  # })
  
  
  ## Population chart ----
  
  output$population_graph <- renderPlotly({
    
    ggplotly(population %>%
      filter(la_name == input$la 
             & school_name == input$school 
             & measure == input$pop_var) %>%
      ggplot(aes(year,value,group = 1)) + 
      geom_line() +
      scale_y_continuous(limits = c(0,NA)) +
      labs(x = "Year", y = input$pop_var))
    
  })
  
  
  
  # Attendance trend chart ----
  
  output$attendance_graph <- renderPlotly({
    
    ggplotly(attendance %>%
      filter(la_name == input$la & 
               school_name == input$school & 
               measure == input$att_var &
               stage == "All Stages") %>%
      ggplot(aes(year, value, group = 1)) + 
    geom_line() +
      geom_text_repel(aes(label = paste(value_label,"%")),
                      na.rm = TRUE,
                      nudge_x = 0,
                      check_overlap = TRUE) +
      scale_y_continuous(limits = c(0,NA)) +
      labs(x = "Academic Year", y = paste("%",input$att_var))) 
    
    
    
  })
  
  # Attendance by stage bar chart ----
  
  output$attendance_stage_graph <- renderPlotly({
    
    ggplotly(attendance %>%
      filter(la_name == input$la & 
               school_name == input$school &
               measure == input$att_var & 
               stage != "All Stages") %>%
      ggplot(aes(value, stage)) + 
      geom_col() +
        
        labs(x ="Percentage Attendance" , y = "Pupil Stage"))
    
  })
  
  
  # Attainment charts ----
  
  output$attainment_bar_graph <- renderPlotly({
    
    ggplotly(attainment %>%
      filter(la_name == input$la 
             & school_name == input$school 
             & measure == input$attainment_bge 
             & stage == input$attainment_stage 
             & year == input$attainment_year 
             & dataset == "bge") %>%
      ggplot(aes(comparator, value)) + 
      geom_col() +
      labs(x = NULL , y = NULL))
    
  })
  
 
  
 
  #Doughnut chart 
  output$donut_plot <- renderggiraph({
    
   att_data <- attainment %>% 
      filter(la_name == input$la 
             & school_name == input$school 
             & str_starts(measure, input$attainment_bge)
             & stage == input$attainment_stage 
             & year == input$attainment_year 
             & dataset == "acel") 
              
    
    p <- ggplot(att_data, aes(y = value, fill = rev(measure))) +
    geom_bar_interactive(
      aes(x = 1, #tooltip = hover_text
          ),
      width = 0.5,
      stat = "identity",
      show.legend = FALSE
    ) +
    annotate(
    geom = "text",
    x = 0,
    y = 0,
    label = paste0(filter(att_data,
                         str_ends(measure, "% Meeting Level")) %>%
                           pull(value_label),"%"),
    size = 20,
    color = "#3182bd"
    ) +
      scale_fill_manual(values = c("white","#3182bd")) +
    coord_polar(theta = "y") +
    theme_void()
  
  ggiraph(ggobj = p)
  
  })
  
  
  ## Pupil profile ----
  
  
  
  output$pup_pop_graph <- renderPlotly({
    
    ggplotly(
      population %>%
        filter(la_name == input$la 
               & school_name == input$school
               & measure_category 
               %in% c("sex",
                      "primary_stage",
                      "deprivation"))%>%
        ggplot(aes(measure,value, group = 1)) + 
        geom_col() +
        theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        labs(x = NULL , y = NULL)
    )
    
  })
  
  output$pup_pop_graph2 <- renderPlotly({
    
    ggplotly(
      population %>%
        filter(la_name == input$la 
               & school_name == input$school
               & measure_category 
               %in% c("free_school_meals",
                      "additional_support_needs",
                      "english_additional_language",
                      "ethnicity",
                      "urban_rural"))%>%
        ggplot(aes(measure,value, group = 1)) + 
        geom_col() +
        theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        labs(x = NULL , y = NULL)
    )

  })
  
  
  
  
  ## School profile ----
  
  # Filter profile data based on LA dropdown
  filter_profile <- reactive({
    school_profile %>%
      filter(la_name == input$la & school_name == input$school)
  })
  
  # School Profile Summary Text
  callModule(school_profile_server, "primary_school_profile", filter_profile)
  
  # School Profile Value Box Values
  

  output$attendance <- renderValueBox({
    valueBox(value = tags$p("Attendance", style = "font-size: 75%;"),
    tags$p(h3(paste(filter_profile()$attendance))),
    icon = icon("line-chart"), 
    color = "teal")

  })
  
  output$adver_class <- renderValueBox({
    valueBox(value = tags$p("Average Class Size", style = "font-size: 75%;"),
             tags$p(h3(paste(filter_profile()$average_class))),
             icon = icon("user"), 
             color = "teal")
    
  })
  
 
  
  output$pe <- renderValueBox({
    valueBox(value = tags$p("Meeting PE Target", style = "font-size: 75%;"),
             tags$p(h3(paste(filter_profile()$pe_target))),
             icon = icon("fa-solid fa-bullseye"), 
             color = "teal")
    
  })
 
  
  
  output$pup_num <- renderValueBox({
    valueBox(value = tags$p("Pupil Numbers", style = "font-size: 75%;"),
             tags$p(h3(paste(filter_profile()$roll))),
             icon = icon("bar-chart-o"), 
             color = "teal")
    
  })
  
 
  output$teach_num <- renderValueBox({
    valueBox(value = tags$p("Teacher Numbers", style = "font-size: 75%;"),
             tags$p(h3(paste(filter_profile()$fte_teacher_numbers))),
             icon = icon("fa-regular fa-user-graduate"), 
             color = "teal")
    
  })
  
 
  
  output$ptr <- renderValueBox({
    valueBox(value = tags$p("Attendance", style = "font-size: 75%;"),
             tags$p(h3(paste(filter_profile()$ptr))),
             icon = icon("fa-solid fa-chart-pie"), 
             color = "teal")
    
  })
 
  
  
  
  
  # Click for info - School Profile Value Boxes
  
  onclick('attendance', showModal(modalDialog(
    title = "Attendance",
    p("This shows you the attendance rate for your chosen school/area"),
    p("Attendance and absence data is collected from publicly funded schools every two years")
  ))
  )
  
  onclick('adver_class', showModal(modalDialog(
    title = "Average Class Size",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
  onclick('pe', showModal(modalDialog(
    title = "Meeting PE Target",
    p("Primary schools have a target to provide 120 minutes of Physical Education (PE) a week for pupils."),
    p("This shows whether the school (or the percetnage of schools in your chosen area) is meeting this target or not."),
    p("Information is collected in February every year in the Healthy Living Survey. A link to the Halthy Living Survey can be found here: ")
  ))
  )
  
  onclick('pup_num', showModal(modalDialog(
    title = "Pupil Numbers",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
  onclick('teach_num', showModal(modalDialog(
    title = "Teacher Numbers",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
  onclick('ptr', showModal(modalDialog(
    title = "Pupil Teacher Ratio",
    p("Information on the pupil numbers, teacher numbers and Class sizes are collected from publicly funded schools every year."),
    p("The information shown here is from the Pupil and Teacher Census a link to which is here:"),
    p("Note that overall local authroity level data may not be the same as the sum of all school data within that local authroity, as local authorities may have teahcers recorded at virtual schools. 
       For more information please see Section 7 in the Requently Asked Questions"),
  ))
  )
  
  
  ### Dashboard heading ----
  
  output$la_school_title <- renderText({
    paste(input$la, "-",input$school)
  })
  
 
  
}


### Run the app ----

shinyApp(ui = ui, server = server)


### END OF SCRIPT ###