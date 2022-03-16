##########################################################################################################
#                                    Code by Damien Dupré : Jan 2019                                     #
##########################################################################################################


using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
using(
  "shinydashboard",
  "flexdashboard",
  "here",
  "plyr",
  "tidyverse",
  "data.table")
options(scipen=999)
options(digits=4)
##########################################################################################################
#                                                     ui                                                 #
##########################################################################################################
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(
                      title = "Emotion Recognition"),
                    dashboardSidebar(                      
                      sidebarMenu(
                        menuItem("Overall Results", tabName = "overall_tab", icon = icon("cogs")),
                        menuItem("Videos", tabName = "videos_tab", icon = icon("youtube-play")),
                        menuItem("Manual Input", tabName = "manual_input", icon = icon("cogs"))
                        ),
                      sliderInput("threshold", "Set Recognition Threshold",
                                  min = 0, max = 1, value = 0.5, step = 0.1
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "videos_tab",
                                tabBox(title = "", width=NULL,
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Neutral"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/rjyvgamJtLs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/-MGKr6jJPhA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Joy"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/HXH6vRkppmw" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/oj-RGpswO-s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Surprise"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/r5ta5RgeQx4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/MEKbeS5Wook" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Fear"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/_eeXh4cx3ig" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/Bx4wJCjpkkM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Disgust"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/SlN6fBjV3No" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/vMMAa1SLiLU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Curiosity"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/fBSIzT_O5SU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/3rWFqiQe-E8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Boredom"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/HkxXnrMYQk0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/ysvZzwVEOqw" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                )
                                       )
                                ),
                        tabItem(tabName = "overall_tab",
                                box(width = 12,
                                  selectInput("system", choices = c("affectiva","kairos","microsoft","crowdemotion","crowdemotion_CNN"), label = NULL)
                                  ),
                                  box(width = 3,title = "Confidence & Sensitivity"
                                  ),
                                  box(width = 6,title = "Accuracy Summary",
                                      plotOutput("accuracy_summary",height = "500px")
                                  ),
                                  box(width = 3,title = "Accuracy"
                                  )
                                )

                        )
                    )
)
##########################################################################################################
#                                                 server                                                 #
##########################################################################################################
server <- function(input, output,session) {
  target_labels <- c("joy","fear","disgust","sadness","anger","surprise")
  all_raw <- isolate(
    readr::read_csv(here::here("R/data/all_raw.csv"))
  )
  all_threshold <- reactive({
  })
  all_accuracy <- reactive({
  })
  system_summary <- reactive({
  })
  all_summary <- reactive({
  })
  ##########################################################################

  ##########################################################################

  ##########################################################################

  output$confT <- renderGauge({
    })
  output$confNT <- renderGauge({
    })
  output$sensT <- renderGauge({
    })
  output$sensNT <- renderGauge({
    })
  }
shinyApp(ui, server)