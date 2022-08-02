
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
          
          width = 12,
          collapsible = FALSE,
          
          column(
            
            width = 4,
            
            map_output("map"),
            br(),
          
            fluidRow(
              
              # Button to click for further COVID-19 information
              covid19_ui("covid19"),
              
              # Button to click for FAQs
              faq_ui("faq")
            )
            
          ),
          
          # School Profile Text
          column(
            width = 8,
            school_profile_output("school_profile_text")
          )
          
        ),
          
        # School Profile Value Boxes 
        school_value_box_output("school_profile_boxes")
        
      ),
      
      # 2 - UI - Pupil Profile ---- 
      
      pupil_profile_ui("pupil_profile"),

      
      # 2 - UI - Measure Options ----
      
      # 2 - UI - Attendance ----
      
      attendance_ui("attendance"),
      
      # 2 - UI - Attainment ----
      
      attainment_ui("attainment", unique(attainment$year)),
      
      fluidRow(
        
        # 2 - UI - Population ----
        
        # Population Title Box 
        section_header_output("population_header"),
        
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

  
  # FAQ Button ----
  callModule(faq_server, "faq", FAQ)
  
  
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

  
  ## Map ----
  
  # Filter map data based on LA dropdown
  # This will need to be updated when the lat/long data is moved to school_profile
  
  filter_type <- reactive({
    dat %>%
      filter(la_name == input$la & school_name == input$school)
  })
  
  # Plot the postcode locations on a map
  callModule(map_server, "map", filter_type)
  
  
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
  
  
  
  # Attendance ----
  
  attendance_filtered <- reactive({
    attendance %>%
      filter(la_name == input$la & school_name == input$school)
  })
  
  callModule(attendance_server, "attendance", attendance_filtered)
  
  
  # Attainment charts ----
  
  attainment_filtered <- reactive({
    attainment %>%
      filter(la_name == input$la & school_name == input$school)
  })
  
  callModule(attainment_server, "attainment", attainment_filtered)
  
  
  ## Pupil profile ----
  
  population_filtered <- reactive({
    population %>%
      filter(la_name == input$la & school_name == input$school)
  })
  
  callModule(pupil_profile_server, "pupil_profile", population_filtered)
  
  
  ## School profile ----
  
  # Filter profile data based on LA dropdown
  filter_profile <- reactive({
    school_profile %>%
      filter(la_name == input$la & school_name == input$school)
  })
  
  # School Profile Summary Text
  callModule(school_profile_server, "school_profile_text", filter_profile)
  
  # School Profile Value Box Values
  callModule(school_value_box_server, "school_profile_boxes", filter_profile)
  
  ### Dashboard heading ----
  
  output$la_school_title <- renderText({
    paste(input$la, "-",input$school)
  })
  
}


### Run the app ----

shinyApp(ui = ui, server = server)


### END OF SCRIPT ###