#########################################################################
# Name of file - 04_deploy_apps.R
# Data release - School Information Dashboard
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Deploys apps to scotland.shinyapps.io server
#########################################################################


### 0 - Setup ----
 source("00_shiny_setup.R")

# Check scotland.shinyapps.io account registered on local system 
# and if not, set up

if(!"scotland" %in% accounts(server = "shinyapps.io")$name) {
  setAccountInfo(
    name = "scotland",
    token = rstudioapi::askForPassword("Enter token: "),
    secret = rstudioapi::askForPassword("Enter secret: ")
  )
}


### 1 - Primary App ----

deployApp(
  appFiles = c(
    "00_shiny_setup.R",
    "01_primary_app.R",
    paste0("primary_data/", shiny_run_label),
    "modules",
    "www"
  ),
  appDir = here("app"),
  appPrimaryDoc = "01_primary_app.R",
  appTitle = "sg-primary_school_information_dashboard",
  account = "scotland"
)


### 2 - Secondary App ----

deployApp(
  appFiles = c(
    "00_shiny_setup.R",
    "02_secondary_app.R",
    paste0("secondary_data/", shiny_run_label),
    "modules",
    "www"
  ),
  appDir = here("app"),
  appPrimaryDoc = "02_secondary_app.R",
  appTitle = "sg-secondary_school_information_dashboard",
  account = "scotland"
)


### 3 - Special App ----

deployApp(
  appFiles = c(
    "00_shiny_setup.R",
    "03_special_app.R",
    paste0("special_data/", shiny_run_label),
    "modules",
    "www"
  ),
  appDir = here("app"),
  appPrimaryDoc = "03_special_app.R",
  appTitle = "sg-special_school_information_dashboard",
  account = "scotland"
)


### END OF SCRIPT ###