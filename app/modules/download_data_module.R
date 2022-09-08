download_data_ui <- function(id) {
  
  ns <- NS(id)
  
  downloadButton(
    ns("button"), 
    "Download Data",
    style = "color: white; background-color: purple; border-color:purple",
    width = "100%"
  )
  
}

download_data_server <- function(input, output, session, data_name, data) {
  
  output$button <- downloadHandler(
    
    filename = function() {
      paste0(data_name, "-", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      data() %>% 
        select(-value) %>% 
        rename(value = value_label) %>% 
        write_csv(file)
    }
    
  )
  
}