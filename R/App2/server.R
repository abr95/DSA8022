library(tidyverse)
library(shiny)

function(
  input, 
  output
) {
  output$shiny_theme <- renderText({
    input$input_theme
  })
  output$text_theme <- renderText({
    input$input_theme
  })
  observeEvent(input$button, {
    print("Pressed")
  })
}
