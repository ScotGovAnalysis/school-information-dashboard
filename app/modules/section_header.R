
section_header_ui <- function(id) {
  
  ns <- NS(id)
  
  valueBoxOutput(ns("header"), width = 12)
  
}

section_header <- function(input, output, session, section_name, box_colour = "yellow") {
  
  output$header <- renderValueBox({
    valueBox(value = paste(section_name, "Profile"),
             subtitle = "",
             color = box_colour)
  })
  
  onclick(
    id = "header",
    expr = showModal(modalDialog(
      title = paste(section_name, "Profile"),
      p("ADD TEXT HERE")
    ))
  )
  
}