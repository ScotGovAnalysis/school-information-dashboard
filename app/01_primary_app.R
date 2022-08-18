
### 0 - Set up ----

source("00_shiny_setup.R")


### 1 - Load data files ----

school_profile <- read_rds(
  paste0("primary_data/", shiny_run_label, "/primary_school_profile.rds")
) %>%
  # TEMP - This can be removed when lat and long added to school profile data
  left_join(
    read_rds(here("map", "lat_long_data_schools.rds")) %>%
      select(seed_code, lng, lat)
  )

attendance <- read_rds(
  paste0("primary_data/", shiny_run_label, "/primary_attendance.rds")
)

population <- read_rds(
  paste0("primary_data/", shiny_run_label, "/primary_population.rds")
) 
      
attainment <- read_rds(
  paste0("primary_data/", shiny_run_label, "/primary_attainment.rds")
)

FAQ <- read_excel("modules/text_content/FAQ.xlsx")

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
    
    dashboardSidebar(absolutePanel(top = 20, left = 10,fixed = TRUE, width = 220,
      sidebar_ui("sidebar", unique(school_profile$la_name))
    )),
    
    
    # 2 - UI - Main body ----

    dashboardBody(
      
      fluidRow(
        
        # School Profile Title Box 
        dashboard_title_output("title"),
        
        # School Profile Content Box
        school_profile_output("school_profile", "Primary"),
          
        # School Profile Value Boxes 
        school_value_box_output("school_profile_boxes", "Primary")
        
      ),
      
      pupil_profile_ui("pupil_profile", "Primary"),
      attendance_ui("attendance", "Primary"),
      primary_attainment_ui("attainment", unique(attainment$year)),
      population_ui("population")
      
    )
    
  )


### 3 - Server ----

server <- function(input, output, session) {
    
  # Introduction Popup ----
  callModule(introduction_server, "introduction")
  
  
  # Sidebar filters ----
  filters <- callModule(sidebar_server, "sidebar", school_profile)
  
  
  # Filter datasets by LA and School ----
  school_profile_filtered <- reactive({
    school_profile %>% 
      filter(la_name == filters()$la & school_name == filters()$school)
  })
  
  attendance_filtered <- reactive({
    attendance %>% 
      filter(la_name == filters()$la & school_name == filters()$school)
  })
  
  population_filtered <- reactive({
    population %>% 
      filter(la_name == filters()$la & school_name == filters()$school)
  })
    
  attainment_filtered <- reactive({
    attainment %>% 
      filter(la_name == filters()$la & school_name == filters()$school)
  })
  
  
  # Dashboard heading ----
  callModule(dashboard_title_server, "title", "Primary", filters)

  
  ## Profile sections ----
  
  # School profile
  callModule(school_profile_server, "school_profile", school_profile_filtered, FAQ, "Primary")
  callModule(school_value_box_server, "school_profile_boxes", school_profile_filtered)
  
  # Pupil Profile
  callModule(pupil_profile_server, "pupil_profile", population_filtered)
  
  # Attendance
  callModule(attendance_server, "attendance", attendance_filtered)
  
  # Attainment
  callModule(primary_attainment_server, "attainment", attainment_filtered)
  
  # Population
  callModule(population_server, "population", population_filtered)
  
}


### Run the app ----

shinyApp(ui = ui, server = server)


### END OF SCRIPT ###