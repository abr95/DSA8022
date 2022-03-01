library(tidyverse)
library(bslib)
library(shiny)
library(shinythemes)

options(shiny.host = "0.0.0.0")
options(shiny.port = 8888)

span()

source("shine.R")

rawValue <- function(
  outputId
) {
  text = textOutput(outputId = outputId)
  print(ls(text))
  print(text$attribs)
  print(text$children)
  print(text$name)
  print(text)
  return(text)
}

ui <- fluidPage(
  title = "App102",
  theme = shinytheme("flatly"),
  actionButton(inputId = "button", label = "Button"),
  rawValue(outputId = "theme"),
  selectInput(
    inputId = "theme",
    choices = allThemes(),
    label = "Theme"
  )
)
