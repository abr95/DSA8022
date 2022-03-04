##########################################################################################################
#                                    Code by Damien Dupré : Jan 2019                                    #
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
#                                                     ui                                                  #
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
                                  box(width = 3,title = "Accuracy",
                                      gaugeOutput("TP", width = "170", height = "90px"),
                                      tags$p("Overall Proportion of True Positives"),
                                      gaugeOutput("TN", width = "170", height = "90px"),
                                      tags$p("Overall Proportion of True Negatives"),
                                      gaugeOutput("FP", width = "170", height = "90px"),
                                      tags$p("Overall Proportion of False Positives"),
                                      gaugeOutput("FN", width = "170", height = "90px"),
                                      tags$p("Overall Proportion of False Negatives")
                                      )
                                ),
                        tabItem(tabName = "manual_input",
                                box(title = "Data Selection & Processing", width = 3,
                                    fileInput("data_upload", "Upload data File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                    selectInput("inSelectJoy", "Select Joy values", choices = "Pending Upload"),
                                    selectInput("inSelectSurprise", "Select Surprise values", choices = "Pending Upload"),
                                    selectInput("inSelectAnger", "Select Anger values", choices = "Pending Upload"),
                                    selectInput("inSelectSadness", "Select Sadness values", choices = "Pending Upload"),
                                    selectInput("inSelectDisgust", "Select Disgust values", choices = "Pending Upload"),
                                    selectInput("inSelectFear", "Select Fear values", choices = "Pending Upload"),
                                    actionButton("process", "Process file")
                                ),
                                tabBox(title = "", width=9,
                                       tabPanel(title=tagList("Raw Timelines"),
                                                verbatimTextOutput("test"),
                                                plotOutput("rawplot")
                                       ),
                                       tabPanel(title=tagList("Confidence & Sensitivity"),
                                                plotOutput("indicatorplot")
                                       )
                                       
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
    all_raw <- all_raw %>% #create new emotion column recoded according threshold value
    mutate(anger_c = ifelse(anger > input$threshold,1,0)) %>%
    mutate(disgust_c = ifelse(disgust > input$threshold,1,0)) %>%
    mutate(fear_c = ifelse(fear > input$threshold,1,0)) %>%
    mutate(joy_c = ifelse(joy > input$threshold,1,0)) %>%
    mutate(sadness_c = ifelse(sadness > input$threshold,1,0)) %>%
    mutate(surprise_c = ifelse(surprise > input$threshold,1,0))
    ##########################################################################
    all_Target <- all_raw %>% # copy original data frame, replace value by O for non target emotions and calculate rowmax
      select(anger_c,disgust_c,fear_c,joy_c,sadness_c,surprise_c,video_emotion) 
    all_Target$anger_c[all_Target$video_emotion != "anger"] <- 0
    all_Target$disgust_c[all_Target$video_emotion != "disgust"] <- 0
    all_Target$fear_c[all_Target$video_emotion != "fear"] <- 0
    all_Target$joy_c[all_Target$video_emotion != "joy"] <- 0
    all_Target$sadness_c[all_Target$video_emotion != "sadness"] <- 0
    all_Target$surprise_c[all_Target$video_emotion != "surprise"] <- 0
    all_Target <- all_Target %>%
      select(-video_emotion)
    all_Target$Target <- apply(all_Target, 1, max)
    ##########################################################################
    all_NTarget <- all_raw %>% # copy original data frame, replace value by O for target emotions and calculate rowmax
      select(anger_c,disgust_c,fear_c,joy_c,sadness_c,surprise_c,video_emotion)
    all_NTarget$anger_c[all_NTarget$video_emotion == "anger"] <- 0
    all_NTarget$disgust_c[all_NTarget$video_emotion == "disgust"] <- 0
    all_NTarget$fear_c[all_NTarget$video_emotion == "fear"] <- 0
    all_NTarget$joy_c[all_NTarget$video_emotion == "joy"] <- 0
    all_NTarget$sadness_c[all_NTarget$video_emotion == "sadness"] <- 0
    all_NTarget$surprise_c[all_NTarget$video_emotion == "surprise"] <- 0
    all_NTarget <- all_NTarget %>%
      select(-video_emotion)
    all_NTarget$NTarget <- apply(all_NTarget, 1, max)
    ##########################################################################
    all_raw$Target <- all_Target$Target # merge target and non target wih original values
    all_raw$NTarget <- all_NTarget$NTarget
    rm(all_NTarget,all_Target)
    all_raw$TruePositive <- ifelse(all_raw$Target == 1 & all_raw$NTarget == 0,1,0)#when only the target label is recognised
    all_raw$TrueNegative <- ifelse(all_raw$Target == 0 & all_raw$NTarget == 0,1,0)#when no label at all is recognised
    all_raw$FalsePositive <- ifelse(all_raw$Target == 1 & all_raw$NTarget == 1,1,0)#when nontarget label are recognised
    all_raw$FalseNegative <- ifelse(all_raw$Target == 0 & all_raw$NTarget == 1,1,0)#when only the non target label is recognised
    all_raw
  })
  all_accuracy <- reactive({
    all_threshold() %>%
      dplyr::filter(video_emotion %in% c("joy","fear","disgust","sadness","anger","surprise")) %>%
      dplyr::select(system,video,FalseNegative, FalsePositive, TruePositive, TrueNegative) %>%
      tidyr::gather(key = indicator,value = indicator_score,FalseNegative, FalsePositive, TruePositive, TrueNegative) %>%
      dplyr::mutate(indicator = ordered(indicator, levels = c("TruePositive", "TrueNegative", "FalsePositive","FalseNegative")))
  })
  system_summary <- reactive({
    all_accuracy() %>%
      dplyr::filter(system == input$system) %>%
      dplyr::group_by(indicator) %>%
      dplyr::summarise(mean=mean(indicator_score), sd=sd(indicator_score))
  })
  all_summary <- reactive({
    all_accuracy() %>%
      dplyr::filter(system == input$system) %>%
      dplyr::group_by(indicator,video) %>%
      dplyr::summarise(mean=mean(indicator_score), sd=sd(indicator_score))
  })
  ##########################################################################
  output$TP <- renderGauge({
    gauge(round(as.numeric(system_summary()[which(system_summary()$indicator=="TruePositive"),"mean"]) * 100, digits = 2), min = 0, max = 100, symbol = '%',gaugeSectors(danger = c(0, 50), warning = c(50, 90), success = c(90, 100)))
  })
  output$TN <- renderGauge({
    gauge(round(as.numeric(system_summary()[which(system_summary()$indicator=="TrueNegative"),"mean"]) * 100, digits = 2), min = 0, max = 100, symbol = '%',gaugeSectors(danger = c(0, 50), warning = c(50, 90), success = c(90, 100)))
  })
  output$FP <- renderGauge({
    gauge(round(as.numeric(system_summary()[which(system_summary()$indicator=="FalsePositive"),"mean"]) * 100, digits = 2), min = 0, max = 100, symbol = '%',gaugeSectors(danger = c(90, 100), warning = c(50, 89), success = c(0, 50)))
  })
  output$FN <- renderGauge({
    gauge(round(as.numeric(system_summary()[which(system_summary()$indicator=="FalseNegative"),"mean"]) * 100, digits = 2), min = 0, max = 100, symbol = '%',gaugeSectors(danger = c(90, 100), warning = c(50, 89), success = c(0, 50)))
  })
  ##########################################################################
  output$accuracy_summary <- renderPlot({
    ggplot(all_summary(), aes(x=mean*100, y=video)) +
      geom_segment(aes(yend=video), xend=0, colour="grey50") +
      geom_point(size=3, aes(colour=video))+
      facet_grid(indicator ~ ., scales="free_y", space="free_y")+ 
      theme(legend.position="none",text = element_text(size=16)) +
      labs(x = "Indicator Proportion") +
      scale_x_continuous(breaks = seq(0, 100, by = 25), labels = paste(seq(0,100, by = 25) , "%", sep = ""))
  })
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
    
    df_confidence_mean_nontarget <- df_confidence_mean %>%
      dplyr::filter(video_emotion != emotion) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(avg = mean(emotion_score)) %>%
      as.numeric()

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
    
    df_sensitivity_mean_nontarget <- df_sensitivity_mean %>%
      dplyr::filter(video_emotion != emotion) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(avg = mean(emotion_score)) %>%
      as.numeric()
    
    gauge(round(as.numeric(df_sensitivity_mean_nontarget), digits = 2), min = 0, max = 100, symbol = '%',gaugeSectors(danger = c(90, 100), warning = c(50, 89), success = c(0, 50)))
  })
  ################################################################################################
  ##################################### Manual input #############################################
  ################################################################################################
  data_raw <- reactive({
    inFile <- input$data_upload
    if (is.null(inFile)) return(NULL)
    data <- fread(inFile$datapath)
    return(data)
  })
  #
  observe({
    updateSelectInput(session, "inSelectJoy", choices = names(data_raw()))
  })
  observe({
    updateSelectInput(session, "inSelectSurprise", choices = names(data_raw()))
  })
  observe({
    updateSelectInput(session, "inSelectDisgust", choices = names(data_raw()))
  })
  observe({
    updateSelectInput(session, "inSelectFear", choices = names(data_raw()))
  })
  observe({
    updateSelectInput(session, "inSelectSadness", choices = names(data_raw()))
  })
  observe({
    updateSelectInput(session, "inSelectAnger", choices = names(data_raw()))
  })
  data_df <- eventReactive(input$process, {
    data <- data_raw() %>%
      subset(., select=which(!duplicated(names(.)))) %>%
      dplyr::select(input$inSelectJoy,input$inSelectFear,input$inSelectDisgust,input$inSelectSadness,input$inSelectAnger,input$inSelectSurprise)
    if (length(colnames(data))!=6){
      return("column selection incorrect")
    } 
    colnames(data) <- c("joy","fear","disgust","sadness","anger","surprise")
    data %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::mutate(time = 1:nrow(data)) %>%
      tidyr::gather(key = emotion,value = emotion_score,c("joy","fear","disgust","sadness","anger","surprise")) %>%
      dplyr::mutate(emotion_score = scales::rescale(emotion_score,to = c(0, 1)))
  })
  
  output$test <- renderPrint({
    print(str(data_df()))
  })
  output$rawplot <- renderPlot({
    ggplot(data = data_df(), aes(time, emotion_score))+
      geom_line(aes(colour= emotion),size=1)+
      geom_hline(aes(yintercept=input$threshold), colour="#990000", linetype="dashed") +
      ylim(0, 1) + 
      theme(text = element_text(size=13)) +
      labs(x = "Video Frames",
           y = "Rescaled Raw Data") +
      guides(colour=guide_legend(title="Emotions"))
  })
  output$indicatorplot <- renderPlot({
    confidence_raw_df <- data_df() %>%
      dplyr::mutate(emotion_score = ifelse(emotion_score < input$threshold,0,emotion_score)) %>%
      tidyr::spread(key = emotion, value = emotion_score)%>%
      dplyr::select(target_labels) %>%
      dplyr::summarise_all(funs(sum))
    
    confidence_df <- confidence_raw_df%>%
      dplyr::select_if(is.numeric) %>%
      dplyr::mutate(sum = rowSums(.)) %>%
      dplyr::mutate_all(funs((. / sum)*100)) %>%
      dplyr::mutate_all(funs(replace(., is.na(.), 0))) %>%
      dplyr::select(-sum) %>%
      tidyr::gather(key = emotion, value = emotion_score,target_labels) %>%
      dplyr::mutate(indicator = "confidence")
    
    sensitivity_raw_df <- data_df() %>%
      dplyr::mutate(emotion_score = ifelse(emotion_score < input$threshold,0,1)) %>%
      tidyr::spread(key = emotion, value = emotion_score)%>%
      dplyr::select(-time) %>%
      dplyr::mutate(nframe = 1) %>%
      dplyr::summarise_all(funs(sum))
    
    sensitivity_df <- sensitivity_raw_df%>%
      dplyr::select_if(is.numeric) %>%
      dplyr::mutate_all(funs((. / nframe)*100)) %>%
      dplyr::mutate_all(funs(replace(., is.na(.), 0)))%>%
      dplyr::select(-nframe) %>%
      dplyr::gather(key = emotion, value = emotion_score,target_labels) %>%
      dplyr::mutate(indicator = "sensitivity")
    
    df_indicator <- rbind(confidence_df,sensitivity_df)
    df_indicator$emotion_score <- round(df_indicator$emotion_score,2)
    
    ggplot(df_indicator, aes(x=emotion, y=emotion_score, fill = indicator, label = emotion_score)) +
      geom_bar(stat="identity", position="dodge") +
      theme(text = element_text(size=14),
            axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "Emotions",
           y = "Percentage Indicator") +
      ylim(0, 100) +
      geom_text(position = position_dodge(0.9),angle = 90,hjust = -0)
  })
}
shinyApp(ui, server)