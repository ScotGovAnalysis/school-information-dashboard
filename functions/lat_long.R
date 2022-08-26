#' @title Convert easting/northing co-ordinates to latitude/longitude
#'
#' @param easting Numeric easting co-ordinate
#' @param northing Numeric northing co-ordinate
#' @param return Character; either 'lat' or 'long' to define which co-ordinate
#' should be returned. 
#' 
#' @export

convert_coordinates <- function(easting, northing, return) {
  
  # Check return is either lat or long, otherwise print error
  if(!return %in% c("lat", "long")) {
    stop("`return` must be either 'lat' or 'long'.")
  }
  
  `%>%` <- magrittr::`%>%`
  
  lat_long <-
    
    # Combine easting and northing into one co-ordinate
    sf::st_point(c(easting, northing)) %>%
    
    # Set co-ordinate reference system to British National Grid code
    # See http://epsg.io/27700
    sf::st_sfc(crs = 27700) %>%
    
    # Transform co-ordinate to longitude and latitude reference system
    # See http://epsg.io/4326
    sf::st_transform(crs = 4326) %>%
    
    # Extract co-ordinates
    sf::st_coordinates()
  
  # Return latitude or longitude co-ordinate depending on `return` argument
  if(return == "lat") return(lat_long[2])
  if(return == "long") return(lat_long[1])
  
}


#' @title Get latitude and longitude data for postcodes
#' 
#' @description `lat_long` extracts easting/northing co-ordinates for postcodes
#' from ADM server and converts these co-ordinates to latitude/longitude.
#'
#' @param adm_connection Microsoft SQL Server database connection
#' @param postcodes Character vector of postcodes for which to return 
#' co-ordinates
#'
#' @return A tibble containing a row for each unique postcode with corresponding
#' latitude and longitude co-ordinates.
#' 
#' @export

lat_long <- function(adm_connection, postcodes) {
  
  `%>%` <- magrittr::`%>%`
  
  # Check adm_connection correct class
  if(!class(adm_connection) == "Microsoft SQL Server") {
    stop("`adm_connection` must be of class `Microsoft SQL Server`.")
  }

  # Remove duplicates and NAs from postcodes
  postcodes <- unique(postcodes[!is.na(postcodes)])
  
  # Extract data from ADM
  extract <- 
    odbc::dbGetQuery(
      adm_connection,
      paste0("select PostcodeUnit, GridReferenceEasting, ",
             "GridReferenceNorthing from postcode where ",
             "PostcodeUnit in (",
             paste0(shQuote(postcodes, type = "sh"), collapse = ", "),
             ")")
    ) %>%
    tibble::as_tibble() %>%
    magrittr::set_names(c("postcode", "easting", "northing")) %>%
    dplyr::mutate(postcode = trimws(postcode)) %>%
    dplyr::mutate(dplyr::across(c(easting, northing), as.numeric))
  
  # If no postcodes are returned from data extract, produce an error
  if(nrow(extract) == 0) {
    stop("No postcodes found in database.")
  }
  
  # Get number of postcodes with no match in ADM data extract
  n_unmatched <- sum(!postcodes %in% extract$postcode)
  
  # If any postcodes are unmatched, produce warning message
  if(n_unmatched > 0) {
    warning(n_unmatched, " of ", length(postcodes),
            " postcode(s) did not have a match in the database.")
  }
  
  # Convert easting/northing to latitude/longitude
  extract %>%
    dplyr::rowwise() %>%
    dplyr::mutate(latitude = convert_coordinates(easting, northing, "lat"),
           longitude = convert_coordinates(easting, northing, "long")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-easting, -northing)
  
}


### END OF SCRIPT ###