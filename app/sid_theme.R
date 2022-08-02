#' @title School Information Dashboard theme for ggplot2 charts.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(x = manufacturer, y = cty)) +
#'    geom_col() +
#'    sid_theme()
#'
#' @export


sid_theme <- function() {
  
  # Use theme_grey as base theme
  ggplot2::theme_grey() +
    
    ggplot2::theme(
      
      # Set panel background to blank
      panel.background = ggplot2::element_blank(),
      
      # Grid/axis lines:
      # Removes minor grid lines and vertical (x-axis) grid lines
      # Sets horizontal (y-axis) grid lines to light grey colour
      # Sets axis lines to black
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#d9d9d9"),
      axis.line = ggplot2::element_line(colour = "#000000"),
      
      # Text:
      # Sets all text to size 16
      # Sets x-axis text to be angled
      text = ggplot2::element_text(size = 16),

      # Legend:
      # Set legend key size and background to be transparant
      legend.key = ggplot2::element_rect(fill = NA),
      legend.key.size = ggplot2::unit(1.5, 'cm'),
      
      # Set overall plot margins
      plot.margin = ggplot2::margin(10, 10, 10, 10))
  
}


### END OF SCRIPT ###