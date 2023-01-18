modal_with_x <- function(title, content, size = c("m", "s", "l")) {
  
  size <- match.arg(size)
  
  div(id = "shiny-modal", class = "modal fade", tabindex = "-1", 
      
      div(class = "modal-dialog", 
          class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg"),
          
          div(class = "modal-content", 
              
              div(
                class = "modal-header", 
                tags$button(class = "close", 
                            `data-dismiss` = "modal", 
                            #tags$img(src = "close.png", alt = "Close")),
                            HTML("&times")),
                tags$h4(class = "modal-title", title)
              ), 
              
              div(class = "modal-body", content), 
              
              div(class = "modal-footer", modalButton("Close"))
              
          )
          
      ), 
      
      tags$script("$('#shiny-modal').modal().focus();")
      
  )
  
}
