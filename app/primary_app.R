
### 0 - Set up ----

source(here::here("app", "00_shiny_setup.R"))


### 1 - Load data files ----

school_profile <- read_rds(
  here("output", shiny_run_label, 
       paste0(school_type, "_school_profile.rds"))
) %>%
  arrange(as.numeric(la_code), nchar(seed_code), school_name) %>%
  mutate(across(c(roll, fte_teacher_numbers), ~ prettyNum(., big.mark = ","))) %>%
  left_join(
    read_rds(here("map", "lat_long_data_schools.rds")) %>%
      select(seed_code, lng, lat)
  )

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

# Set app language 
# tags$script(HTML("<script html lang= en ></script>"))


### 2 - UI ----

## The UI is the main body of app - it controls mostly what the app looks and 
## functions like when interacting with it.

ui <- 

  dashboardPage(
    
    # 2 - UI - Dashboard Title and Header ----
    
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
      school_filter_input("la_school_filter", unique(school_profile$la_name)),
      
      # Dropdown Filter - Measure
      measure_filter_input("measure_filter"),
      
      # Text instruction to click on boxes for further info
      h3("Click on any box for more information", align = "center"),
      
      br(),
      br(),
      br(),
      
      # Smarter Scotland Logo
      HTML(paste0("<center>", 
                  img(src = "smarter-scotland.jpg", width = 200),
                  "</center>")),
      
      disable = FALSE, 
      width = NULL, 
      collapsed = FALSE
      
    ),
    
    
    # 2 - UI - Main body ----

    dashboardBody(
      
      fluidRow(
        
        # School Profile Title Box 
        dashboard_title_output("title"),
        
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
  la_school <- callModule(school_filter_server, 
                          "la_school_filter", 
                          school_profile)
  
  
  # Filter datasets by LA and School ----
  school_profile_filtered <- reactive({
    school_profile %>% 
      filter(la_name == la_school()$la & school_name == la_school()$school)
  })
  
  attendance_filtered <- reactive({
    attendance %>% 
      filter(la_name == la_school()$la & school_name == la_school()$school)
  })
  
  population_filtered <- reactive({
    population %>% 
      filter(la_name == la_school()$la & school_name == la_school()$school)
  })
    
  attainment_filtered <- reactive({
    attainment %>% 
      filter(la_name == la_school()$la & school_name == la_school()$school)
  })
  
  
  # Dashboard heading ----
  callModule(dashboard_title_server, "title", "Primary", la_school)

  
  ## Profile sections ----
  
  # Pupil Profile
  callModule(pupil_profile_server, "pupil_profile", population_filtered)
  
  # School profile
  callModule(school_profile_server, "school_profile", school_profile_filtered, FAQ)
  callModule(school_value_box_server, "school_profile_boxes", school_profile_filtered)
  
  # Attendance
  callModule(attendance_server, "attendance", attendance_filtered)
  
  # Attainment
  callModule(attainment_server, "attainment", attainment_filtered)
  
  # Population
  callModule(population_server, "population", population_filtered)
  
}


### Run the app ----

shinyApp(ui = ui, server = server)


### END OF SCRIPT ###