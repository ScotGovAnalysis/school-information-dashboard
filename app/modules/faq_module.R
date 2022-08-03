faq_ui <- function(id) {
  
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
      
      selectInput(inputId = ns("section"),
                  label = "Section:",
                  choices = NULL),
      
      dataTableOutput(ns("table"))
    )
  )
  
}

faq_server <- function(input, output, session, data) {
  
  observeEvent(
    input$popup_window,
    {updateSelectInput(
      session = session,
      inputId = "section", 
      choices = c("Select", unique(data$Section)),
      selected = "Select")}
  )
  
  # Table format   
  output$table <- 
    renderDataTable(
      data %>% filter(Section == input$section),
      rownames = FALSE
    )

}