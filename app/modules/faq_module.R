
faq_ui <- function(id) {
  
  ns <- NS(id)
  
  # FAQ Button
  actionButton(
    ns("button"),
    "FAQ's",
    style="color: white; background-color: purple; border-color:purple",
    width = "49%"
  )
  
}

faq_server <- function(input, output, session, data) {
  
  output$table <- renderDataTable(
    data %>% filter(Section == {input$section_filter}),
    rownames = FALSE,
    options = list(columnDefs = list(
      list(targets = '_all', className = 'dt-left'),
      list(targets = 0, visible = FALSE)
    ))
  )
  
  onclick(
    "button",
    showModal(
      modal_with_x(
        title = "FAQ",
        size = "l",
        content = tagList(
          # Dropdown to select FAQ section
          selectInput(inputId = session$ns("section_filter"),
                      label = "Section:",
                      choices = c("Select", unique(data$Section)),
                      selected = "Select"),
      
          # Table of FAQs
          dataTableOutput(outputId = session$ns("table"))
        )
      )
    )
  )
  
}