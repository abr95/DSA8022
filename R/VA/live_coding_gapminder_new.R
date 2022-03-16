library(gapminder)

## The easiest way to get ggplot2 is to install the whole tidyverse:
#library("tidyverse")
## Alternatively, install just ggplot2:
#library("ggplot2")

## The easiest way to get dplyr is to install the whole tidyverse:
#library("tidyverse")
## Alternatively, install just dplyr:
#library("dplyr")
# To find out more about library(dplyr), please see https://datacarpentry.org/R-genomics/04-dplyr.html. or https://dplyr.tidyverse.org

library(tidyverse) # to include ggplot2 and dplyr
library(shiny)
library(shinythemes)
library(DT)

ui <- navbarPage(
  theme = shinytheme("darkly"), # try different shinytheme values -- see https://rstudio.github.io/shinythemes/
  title = "Dashboard",
  # About tab
  tabPanel(
    title = "About",
    
    h1("Hello world"),
    
    p("some text ", a(href="http://www.google.com", "Google"))
  ),
  
  # Gapminder Analysis tab
  tabPanel(
    title = "Gapminder Analysis",
    
    sidebarLayout(
      
      # sidebar panel
      sidebarPanel(
        
        selectInput("x", "X-Axis:",
                    choices = c(
                      "Year" = "year",
                      "Life Expectancy" = "lifeExp",
                      "Population Size" = "pop",
                      "GDP per Capita" = "gdpPercap"
                    ),
                    selected = "gdpPercap"),
        
        selectInput("y", "Y-Axis:",
                    choices = c(
                      "Year" = "year",
                      "Life Expectancy" = "lifeExp",
                      "Population Size" = "pop",
                      "GDP per Capita" = "gdpPercap"
                    ),
                    selected = "lifeExp"),
        textInput("title", "Title", "GDP vs Life Expectancy"),
        numericInput("size", "Point Size", 1, 1),
        checkboxInput("fit", "Add line of the best fit", value = FALSE),
        radioButtons("colour", "Point Colour", 
                     choices = c("blue", "red", "green", "black")),
        selectInput("continents", "Continents",
                    # choices = factor(gapminder$continent),  # alternatively, un-ordered
                    choices = levels(gapminder$continent), # alternatively, ordered
                    multiple = FALSE,
                    selected = "Europe"),
        sliderInput("years", "Years", 
                    min = min(gapminder$year), 
                    max = max(gapminder$year), 
                    value = c(1977, 2002), sep="")
        
      ),
      
      mainPanel(
        # main panel
        tabsetPanel(
          tabPanel("Plot", 
                   plotOutput("plot")),
          
          tabPanel("Data", 
                   dataTableOutput("table")),
          
          tabPanel("Summary", 
                   verbatimTextOutput("summary")),
          
          tabPanel("Stats", 
                   verbatimTextOutput("summaryStats"))
        )
      )
    )
  )
)
  
server <- function(input, output) {
  
  filtered_data <- reactive({   # filtered_data is a new reactive expression which will respond to reactive values
    f_data <- gapminder %>%   # this is pipe operator in dplyr library -- see dplyr for details
        filter( # this is a function in dplyr library
        continent %in% input$continents, # this is an R operator to check if an element belongs to a vector
        year >= input$years[1],
        year <= input$years[2]
      )
    f_data

    ## Alternatively    
    # gapminder %>%
    #   filter(
    #     continent %in% input$continents,
    #     year >= input$years[1],
    #     year <= input$years[2]
    #   )

  })
  
  output$plot <- renderPlot({
    # If you want, you can create variables to store input values    
    # x_title <- input$x
    # y_title <- input$y
    # year_slider <- input$years
    
    data <- filtered_data() # reactive expression must be called as a function
    
    p <- ggplot(data, aes_string(x = input$x, y = input$y)) +
      geom_point(size = input$size, colour = input$colour) + 
      labs(
        title = input$title
      ) 
    
    if(input$fit){
      p <- p + geom_smooth(method = "lm")
    }
    
    p
  })
  
  # Please note: 
  # The renderDataTable function only provides the server-side version of DataTables (using R to process the data object on the server side). There is a separate package DT (https://github.com/rstudio/DT) that allows you to create both server-side and client-side DataTables, and supports additional DataTables features. 
  # The R package DT provides an R interface to the JavaScript library DataTables.
  
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
    
    # You can also simply call the reactive expression
    # filtered_data()
  })
  
  output$summary <- renderPrint({
    data <- filtered_data()
    glimpse(data) # this is again a function of dplyr library
  })
  
  output$summaryStats <- renderPrint({
    data <- filtered_data()
    data %>% # this is R pipe in dplyr library
      group_by(country, continent) %>% # this is again a function of dplyr library
      summarise(avg = mean(lifeExp), sd = sd(lifeExp)) # this is again a function of dplyr library
  })
  
  
}

shinyApp(ui = ui, server = server)