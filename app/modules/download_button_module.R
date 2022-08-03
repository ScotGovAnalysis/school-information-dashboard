download_data_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    downloadButton(
      ns("button"), 
      "Download Data",
      class = "butt"
    ),
    
    tags$head(tags$style(".butt{background-color:#100f3a;} .butt{color: white;}"))
    
  )
  
}

download_data_server <- function(input, output, session, data_name, data) {
  
  output$button <- downloadHandler(
    
    filename = function() {
      paste0(data_name, "-", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write_csv(data, file)
    }
    
  )
  
}