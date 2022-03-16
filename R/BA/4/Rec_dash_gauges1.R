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
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/rjyvgamJtLs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/oj-RGpswO-s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Surprise"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/rjyvgamJtLs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/MEKbeS5Wook" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Fear"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/rjyvgamJtLs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/Bx4wJCjpkkM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Disgust"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/rjyvgamJtLs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/vMMAa1SLiLU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Curiosity"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/rjyvgamJtLs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/3rWFqiQe-E8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                ),
                                       tabPanel(title=tagList(shiny::icon("cloud-download"), "Boredom"),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/rjyvgamJtLs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                                HTML('<iframe width="320" height="240" src="https://www.youtube.com/embed/ysvZzwVEOqw" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                )
                                       )
                                ),
                        tabItem(tabName = "overall_tab",
                                box(width = 12,
                                  selectInput("system", choices = c("affectiva","kairos","microsoft","crowdemotion","crowdemotion_CNN"), label = NULL)
                                  ),
                                  box(width = 3,title = "Confidence & Sensitivity",
                                      gaugeOutput("confT", width = "170", height = "90px"),
                                      tags$p("Overall Proportion of Confidence for Target Emotions"),
                                      gaugeOutput("confNT", width = "170", height = "90px"),
                                      tags$p("Overall Proportion of Confidence for Non-Target Emotions"),
                                      gaugeOutput("sensT", width = "170", height = "90px"),
                                      tags$p("Overall Proportion of Sensitivity for Target Emotions"),
                                      gaugeOutput("sensNT", width = "170", height = "90px"),
                                      tags$p("Overall Proportion of Sensitivity for Non-Target Emotions")
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
    readr::read_csv(here::here("./data/all_raw.csv"))
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
    data_df_confidence <- all_raw %>%
      dplyr::filter(system == input$system) %>%
      dplyr::filter(video_emotion %in% target_labels) %>%
      dplyr::group_by(video) %>%
      tidyr::gather(key = emotion, value = value,target_labels)  %>%
      dplyr::mutate(value = ifelse(value < input$threshold,0,value)) %>%
      tidyr::spread(key = emotion, value = value)%>%
      dplyr::select(target_labels,video) %>%
      dplyr::summarise_all(funs(sum))
    
    df_confidence_system <- data_df_confidence %>%
      dplyr::select_if(is.numeric) %>%
      dplyr::mutate(sum = rowSums(.)) %>%
      dplyr::mutate_all(funs((. / sum)*100)) %>%
      dplyr::mutate_all(funs(replace(., is.na(.), 0))) %>%
      dplyr::mutate(video = data_df_confidence$video) %>%
      dplyr::select(target_labels,video) %>%
      tidyr::gather(key = emotion, value = emotion_score,target_labels) %>%
      tidyr::separate(video,c("video_emotion","video_gender"),sep="_",remove = FALSE)
    
    df_confidence_mean <- df_confidence_system %>%
      dplyr::select(video_emotion,emotion,emotion_score) %>%
      dplyr::group_by(video_emotion,emotion) %>%
      dplyr::summarise_all(funs(mean)) %>%
      dplyr::mutate(indicator = "confidence")
    
    df_confidence_mean_target <- df_confidence_mean %>%
      dplyr::filter(video_emotion == emotion) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(avg = mean(emotion_score)) %>%
      as.numeric()
    
    gauge(round(as.numeric(df_confidence_mean_target), digits = 2), min = 0, max = 100, symbol = '%',gaugeSectors(danger = c(0, 50), warning = c(50, 90), success = c(90, 100)))
    })
  output$confNT <- renderGauge({
    data_df_confidence <- all_raw %>%
      dplyr::filter(system == input$system) %>%
      dplyr::filter(video_emotion %in% target_labels) %>%
      dplyr::group_by(video) %>%
      tidyr::gather(key = emotion, value = value,target_labels)  %>%
      dplyr::mutate(value = ifelse(value < input$threshold,0,value)) %>%
      tidyr::spread(key = emotion, value = value)%>%
      dplyr::select(target_labels,video) %>%
      dplyr::summarise_all(funs(sum))
    
    df_confidence_system <- data_df_confidence %>%
      dplyr::select_if(is.numeric) %>%
      dplyr::mutate(sum = rowSums(.)) %>%
      dplyr::mutate_all(funs((. / sum)*100)) %>%
      dplyr::mutate_all(funs(replace(., is.na(.), 0))) %>%
      dplyr::mutate(video = data_df_confidence$video) %>%
      dplyr::select(target_labels,video) %>%
      tidyr::gather(key = emotion, value = emotion_score,target_labels) %>%
      tidyr::separate(video,c("video_emotion","video_gender"),sep="_",remove = FALSE)
    
    df_confidence_mean <- df_confidence_system %>%
      dplyr::select(video_emotion,emotion,emotion_score) %>%
      dplyr::group_by(video_emotion,emotion) %>%
      dplyr::summarise_all(funs(mean)) %>%
      dplyr::mutate(indicator = "confidence")
    
    # df_confidence_mean_nontarget <- df_confidence_mean
    
    
    
    gauge(round(as.numeric(df_confidence_mean_nontarget), digits = 2), min = 0, max = 100, symbol = '%',gaugeSectors(danger = c(90, 100), warning = c(50, 89), success = c(0, 50)))
    })
  output$sensT <- renderGauge({
    data_df_sensitivity <- all_raw %>%
      dplyr::filter(system == input$system) %>%
      dplyr::filter(video_emotion %in% target_labels)%>%
      dplyr::group_by(video) %>%
      tidyr::gather(key = emotion, value = value,target_labels)  %>%
      dplyr::mutate(value = ifelse(value < input$threshold,0,1)) %>%
      tidyr::spread(key = emotion, value = value)%>%
      dplyr::select(target_labels,video) %>%
      dplyr::mutate(nframe = 1) %>%
      dplyr::summarise_all(funs(sum))
    
    df_sensitivity_system<- data_df_sensitivity %>%
      dplyr::select_if(is.numeric) %>%
      dplyr::mutate(sum = rowSums(.)) %>%
      dplyr::mutate_all(funs((. / sum)*100)) %>%
      dplyr::mutate_all(funs(replace(., is.na(.), 0))) %>%
      dplyr::mutate(video = data_df_sensitivity$video) %>%
      dplyr::select(target_labels,video) %>%
      tidyr::gather(key = emotion, value = emotion_score,target_labels) %>%
      tidyr::separate(video,c("video_emotion","video_gender"),sep="_",remove = FALSE)
    
    df_sensitivity_mean <- df_sensitivity_system %>%
      dplyr::select(video_emotion,emotion,emotion_score) %>%
      dplyr::group_by(video_emotion,emotion) %>%
      dplyr::summarise_all(funs(mean)) %>%
      dplyr::mutate(indicator = "sensitivity")
    
    df_sensitivity_mean_target <- df_sensitivity_mean %>%
      dplyr::filter(video_emotion == emotion) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(avg = mean(emotion_score)) %>%
      as.numeric()
    
    gauge(round(as.numeric(df_sensitivity_mean_target), digits = 2), min = 0, max = 100, symbol = '%',gaugeSectors(danger = c(0, 50), warning = c(50, 90), success = c(90, 100)))
    })
  output$sensNT <- renderGauge({

    #.....



    #.....
    df_sensitivity_mean_nontarget <- df_sensitivity_mean
    #.....



    #.....


    gauge(round(as.numeric(df_sensitivity_mean_nontarget), digits = 2), min = 0, max = 100, symbol = '%',gaugeSectors(danger = c(90, 100), warning = c(50, 89), success = c(0, 50)))
  })
}
shinyApp(ui, server)