library(tidyverse)
library(shiny)
library(thematic)
options(shiny.host = '0.0.0.0')
options(shiny.port = 8888)

ui <- fluidPage(
  titlePanel(
    title = "My First App",
    windowTitle = "Hello from R"
  ),
  titlePanel(
    title = "."
  ),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      sliderInput(
        inputId = "num",
        label = "Choose a number",
        min = 1,
        max = 500,
        value = 25
      ),
      actionButton(inputId = "HHH", label = "Boo")
      
    ),
    mainPanel = mainPanel(
      plotOutput("hist")
    )
  ),
  shinythemes::themeSelector(),
)

make_plot <- function(num) {
  df <- data.frame(rnorm(num))
  names(df) <- c("n")
  plot <- ggplot(df)
  plot <- plot + labs(
    title = "Plot of Random Normal Histogram",
    x = "Random Values",
    y = "Count"
  )
  plot + geom_histogram(aes(x = n), bins = 25)
}

server <- function(
  input, 
  output,
  session
) {
  print("HElloe")
  output$hist <- renderPlot({
    make_plot(input$num)
  })
}

thematic_shiny()
shinyApp(
  ui= ui, 
  server = server
)
