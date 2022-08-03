
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

  # 2 - UI - Dashboard Title and Header ----
    
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
      # measure_filter_input("measure_filter"),
      selectInput(
        inputId = "measure_filter",
        label = "Measure",
        choices = c("Select",
                    "Attendance Profile", 
                    "Population Profile", 
                    "Attainment Profile"),
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
      
      fluidRow(
        
        # School Profile Title Box 
        valueBox(paste(school_type, "School Information Dashboard"), 
                 tags$p(h4(textOutput("la_school_title"))),
                 color = "yellow",
                 width = 12),
        
        # School Profile Content Box
        school_profile_output("school_profile"),
          
        # School Profile Value Boxes 
        school_value_box_output("school_profile_boxes"),
        
      ),
      
      pupil_profile_ui("pupil_profile"),
      attendance_ui("attendance"),
      attainment_ui("attainment", unique(attainment$year)),
      population_ui("population")
      
    )
    
  )


### 3 - Server ----

server <- function(input, output, session) {
    
  # Introduction Popup ----
  callModule(introduction_server, "introduction")
  
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

  
  ## Map ----
  
  # Filter map data based on LA dropdown
  # This will need to be updated when the lat/long data is moved to school_profile
  
  filter_type <- reactive({
    dat %>%
      filter(la_name == input$la & school_name == input$school)
  })
  
  ## Population chart ----
  population_filtered <- reactive({
    population %>%
      filter(la_name == input$la & school_name == input$school)
  })
  
  callModule(population_server, "population", population_filtered)

  
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
  callModule(school_profile_server, "school_profile", filter_type, filter_profile, FAQ)
  
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