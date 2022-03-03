library(tidyverse)
library(shiny)

options(shiny.host = '0.0.0.0')
options(shiny.port = 8888)

ui <- fluidPage(
  textInput(inputId = "name", label = "Name", value = "AB"),
  textOutput(outputId = "name"),
  textOutput(outputId = "name2")
)

server <- function(
  input, 
  output
) {
  output$name <- renderText({
    input$name
  })
  output$name2 <- renderText({
    input$name
  })
}

shinyApp(
  ui = ui, 
  server = server
)
