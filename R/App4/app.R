library(tidyverse)
library(shiny)
library(shinythemes)
library(thematic)
library(DT)

movies <- read_csv("./data/movies.csv")
movies_codebook <- read_csv("./data/movies_codebook.csv")

movies_field_axis_map <- c(
  "IMDB rating" = "imdb_rating",
  "IMDB number of votes" = "imdb_num_votes",
  "Critics Score" = "critics_score",
  "Audience Score" = "audience_score",
  "Runtime" = "runtime"
)

movies_field_color_map <- c(
  "Title Type" = "title_type",
  "Genre" = "genre",
  "MPAA Rating" = "mpaa_rating",
  "Critics Rating" = "critics_rating",
  "Audience Rating" = "audience_rating"
)

movies_field_axis_unmap <- names(movies_field_axis_map)
names(movies_field_axis_unmap) <- unname(movies_field_axis_map)

movies_field_color_unmap <- names(movies_field_color_map)
names(movies_field_color_unmap) <- unname(movies_field_color_map)

title <- "Movie browser, 1970 - 2014"

# the top level panel/layout/window thing
# contains the sidebar and the "main" panel

topLevelPanel <- function(
) {
  panel = sidebarLayout(
    sidebarPanel = topLevelSidebarPanel(),
    mainPanel = topLevelMainPanel(),
  )
  return(panel)
}

# the sidebar panel contains filters and selectors

topLevelSidebarPanel <- function(
) {
  panel <- sidebarPanel(
    plottingControls(),
    samplingControls(),
    tableControls(),
    madeWithShiny()
  )
  return(panel)
}

# the main panel contains the tabs showing the content

topLevelMainPanel <- function(
) {
  panel <- mainPanel(
    mainTabSet()
  )
  return(panel)
}

# controls for plotting sidepanel

plottingControls <- function(
) {
  panel = conditionalPanel(
    condition = "true",
    tags$h3("Plotting"),
    selectInput(
      inputId = "yaxis",
      label = "Y-axis:",
      choices = movies_field_axis_map,
      selected = "audience_score"
    ),
    selectInput(
      inputId = "xaxis",
      label = "X-axis:",
      choices = movies_field_axis_map,
      selected = "critics_score"
    ),
    selectInput(
      inputId = "color",
      label = "Color by:",
      choices = movies_field_color_map,
      selected = "mpaa_rating"
    ),
    sliderInput(
      inputId = "alpha",
      label = "Alpha:",
      min = 0.0,
      max = 1.0,
      value = 0.5
    ),
    sliderInput(
      inputId = "size",
      label = "Size:",
      min = 0.0,
      max = 5.0,
      value = 2.0
    ),
    textInput(
      inputId = "title",
      label = "Plot title:",
      placeholder = "Enter text to be used as plot title"
    )
  )
}

# controls for sub-setting and sampling

samplingControls <- function(
) {
  panel = conditionalPanel(
    condition = "true",
    hr(),
    tags$h3("Subsetting and sampling"),
    radioButtons(
      inputId = "types",
      label = "Select movie type(s):",
      choices = c(
        "Documentary",
        "Feature Film"
      ),
    ),
    numericInput(
      inputId = "sample_size",
      label = "Sample size:",
      value = 50
    )
  )
}

# data table controls

tableControls <- function(
) {
  panel = conditionalPanel(
    condition = "true",
    checkboxInput(
      inputId = "show_table",
      label = "Show data table",
      value = TRUE 
    )
  )
}

# main tab set

mainTabSet <- function(
) {
  tabset <- tabsetPanel(
    id = "tabset",
    plotTab(),
    dataTab(),
    codebookTab()
  )
}

# plot panel

plotTab <- function(
) {
  tab <- tabPanel(
    title = "Plot",
    value = 1,
    plotOutput(
      outputId = "plot"
    ),
    tags$h4(
      textOutput(
        outputId = "plot_tagline", 
        inline = TRUE
      )
    )
  )
}

# data panel

dataTab <- function(
) {
  tab <- tabPanel(
    title = "Data",
    value = 2,
    conditionalPanel(
      condition = "input.show_table",
      dataTableOutput(
        outputId = "table"
      )
    )
  )
}

# codebook panel

codebookTab <- function(
) {
  tab <- tabPanel(
    title = "Codebook",
    value = 3,
    DT::dataTableOutput(
      outputId = "codebook_table"
    )
  )
}

# just for fun

madeWithShiny <- function(
) {
  tags$h5(
    "Made with ",
    tags$image(src = "shiny.png", width = "30px", height = "30px")
  )
}

# ui function

ui <- function(
  ...
) {
  page <- fluidPage(
    title = "Window Title",
    theme = shinytheme("cerulean"),
    titlePanel(
      title = title,
    ),
    topLevelPanel(),
    #shinythemes::themeSelector(),
  )
}

# server function

server <- function(
  input, 
  output,
  session
) {
table
  data_sample <- reactive({
    sample <- movies %>%filter(title_type == input$types)
    sample <- sample %>% slice_sample(n = input$sample_size)
    return(sample)
  })  
  
  output$plot <- renderPlot({
    plot <- ggplot(
      data = data_sample(),
      mapping = aes_string(
        x = input$xaxis,
        y = input$yaxis,
        color = input$color
      ),
    )
    plot <- plot + geom_point(
      alpha = input$alpha,
      size = input$size
    )
    plot <- plot + labs(
      title = input$title,
      x = movies_field_axis_unmap[input$xaxis],
      y = movies_field_axis_unmap[input$yaxis],
      color = movies_field_color_unmap[input$color]
    )
    return(plot)
  })
  
  output$plot_tagline <- renderText({
    text <- paste(
      "There are", input$sample_size, input$types, "movies in this dataset."
    ) 
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(
      data = data_sample(),
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })
  
  output$codebook_table <- DT::renderDataTable({
    movies_codebook
  })
  
}

# shiny app

shinyApp(
  ui = ui, 
  server = server
)
