library(tidyverse)
library(bslib)
library(shiny)
library(shinythemes)
library(htmltools)

options(shiny.host = "0.0.0.0")
options(shiny.port = 8888)

source("shine.R")



ui <- fluidPage(
  title = "App102",
  tags$head(tags$link(rel="stylesheet", type="text/css", href=paste("shinythemes/css/", "shiny_theme", ".min.css", sep = ""))),
  #theme = shinytheme("cosmo"),
  actionButton(inputId = "button", label = "Button"),
  textOutput(outputId = "text_theme"),
  selectInput(
    inputId = "input_theme",
    choices = allThemes(),
    label = "Theme"
  )
)
