measure_filter_input <- function(id) {
  
  ns <- NS(id)
  
  selectInput(
    inputId = ns("measure_filter"),
    label = "Measure",
    choices = c("Select",
                "Attendance Profile", 
                "Population Profile", 
                "Attainment Profile"),
    selected = "Select",
    multiple = FALSE
  )
  
}