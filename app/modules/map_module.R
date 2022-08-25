
map_output <- function(id) {
  
  # Initiate namespace for module
  ns <- NS(id)
  
  # Map Output
  leafletOutput(ns("map"))
  
}

map_server <- function(input, output, session, data) {
  
  # Plot latitude and longitude on map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-4.140078, 57.837990, zoom = 5.3) %>%
      addCircleMarkers(
        data = data(),
        lng = ~ longitude, lat = ~ latitude,
        radius = 6,
        stroke = FALSE,
        opacity = 1,
        fillOpacity = 1,
        color = "navy")
  })
  
}