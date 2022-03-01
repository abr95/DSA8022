library(tidyverse)
library(shiny)

function(
  input, 
  output
) {
  output$theme <- renderText({
    input$theme
  })
  observeEvent(input$button, {
    print("Pressed")
  })
}
