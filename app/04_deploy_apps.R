
### 0 - Setup ----

source("00_shiny_setup.R")


### 1 - Primary App ----

rsconnect::deployApp(
  appFiles = c(
    "00_shiny_setup.R",
    "01_primary_app.R",
    paste0("primary_data/", shiny_run_label),
    "modules",
    "www"
  ),
  appPrimaryDoc = "01_primary_app.R",
  appTitle = "primary_school_information_dashboard"
)


### 2 - Secondary App ----

rsconnect::deployApp(
  appFiles = c(
    "00_shiny_setup.R",
    "02_secondary_app.R",
    paste0("secondary_data/", shiny_run_label),
    "modules",
    "www"
  ),
  appPrimaryDoc = "02_secondary_app.R",
  appTitle = "secondary_school_information_dashboard"
)


### 3 - Special App ----

rsconnect::deployApp(
  appFiles = c(
    "00_shiny_setup.R",
    "03_special_app.R",
    paste0("special_data/", shiny_run_label),
    "modules",
    "www"
  ),
  appPrimaryDoc = "03_special_app.R",
  appTitle = "special_school_information_dashboard"
)


### END OF SCRIPT ###