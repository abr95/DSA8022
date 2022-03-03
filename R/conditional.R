
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.plotType == 1 || input.plotType == 3",
        selectInput(
          "breaks", "Breaks",
          c("Sturges", "Scott", "Freedman-Diaconis", "Custom" = "custom")
        )
      ),
      conditionalPanel(
        condition = "input.plotType == 2",
        radioButtons("controller", "Controller", 1:3, 1)
      ),
      conditionalPanel(
        condition = "input.plotType == 3",
        sliderInput("sample", "Random Sample Size:",
                    min = 1,
                    max = 1000,
                    value = 100
        )
      ),
      conditionalPanel(
        condition = "input.plotType == 4",
        sliderInput("sample", "Random Sample Size:",
                    min = 1,
                    max = 1000,
                    value = c(100, 500)
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "plotType",
        tabPanel("Plot", plotOutput("plot"),value=1),
        tabPanel("Summary", verbatimTextOutput("summary"),value=2),
        tabPanel("Table", tableOutput("table"),value=3),        
        tabPanel("Bar", tableOutput("bar"),value=4)        
      )
    )
  )
)


server <- function(input, output, session) {
}

shinyApp(ui, server)
