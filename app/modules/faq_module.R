faq_ui <- function(id, sections) {
  
  ns <- NS(id)
  
  tagList(
    
    # FAQ Button
    actionButton(
      ns("button"),
      "FAQ's",
      style="color: white; background-color: purple; border-color:purple",
      width = "49%"),
    
    # Popup window to display FAQs
    bsModal(
      
      id = ns("popup_window"), 
      title = "FAQ", 
      trigger = ns("button"), 
      size = "large",
      
      # Dropdown to select FAQ section
      selectInput(inputId = ns("section"),
                  label = "Section:",
                  choices = c("Select", sections),
                  selected = "Select"),
      
      # Table of FAQs
      dataTableOutput(ns("table"))
      
    )
    
  )
  
}

faq_server <- function(input, output, session, data) {
  
  output$table <- 
    renderDataTable(
      data %>% filter(Section == input$section),
      rownames = FALSE,
      options = list(columnDefs = list(
        list(targets = '_all', className = 'dt-center'),
        list(targets = 0, visible = FALSE)
      ))
    )
  
}