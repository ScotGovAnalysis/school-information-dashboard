
section_header_output <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  # Value Box Output
  valueBoxOutput(ns("header"), width = 12)
  
}

section_header_server <- function(input, output, session, section_name, box_colour = "yellow") {
  
  # Render Value Box
  output$header <- renderValueBox({
    valueBox(value = paste(section_name, "Profile"),
             subtitle = "",
             color = box_colour)
  })
  
  # Text to be included in popup for each section
  text <- case_when(
    section_name == "Pupil" ~ "Add pupil profile text here",
    section_name == "Attendance" ~ "Add attendance profile text here",
    section_name == "Attainment" ~ "Add attainment profile text here",
    section_name == "Population" ~ "Add population profile text here"
    # Add content for any new section headers here
  )
  
  # When user clicks value box, display pop up with information text
  onclick(
    id = "header",
    expr = showModal(modalDialog(
      title = paste(section_name, "Profile"),
      p(text)
    ))
  )
  
}