library(shiny)
library(shinythemes)

# copied from shinythemes
allThemes <- function() {
  themes <- dir(
    path = system.file(
      "shinythemes/css", 
      package = "shinythemes"
    ),
    pattern = ".+\\.min.css"
  )
  sub(".min.css", "", themes)
}
