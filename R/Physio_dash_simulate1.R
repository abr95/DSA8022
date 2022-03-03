##########################################################################################################
#                                    Code by Damien Dupré : Jan 2018                                     #
##########################################################################################################
library(shiny)
library(shinydashboard)
library(zoo)
library(xts)
library(RHRV)
library(dplyr)
library(mgcv)
library(ggplot2)
library(dygraphs)
#######################################################################################################
#                                                  general                                            #
#######################################################################################################
options(scipen = 999)
t_start <- 1500000000000 #time start in UNIX time
length_final <- 100

#############################################
############# peak detection ################
#############################################
peaks <- function (x, y = NULL, mode = "maxmin"){
  if (!mode %in% c("max", "min", "maxmin"))
    stop("unknown mode:", mode)
  xy <- xy.coords(x, y)
  x <- xy$x
  y <- xy$y
  l <- length(y)
  ym1 <- c(y[-1], y[l])
  yp1 <- c(y[1], y[-l])
  if (mode == "min") {
    xx <- x[y < ym1 & y < yp1]
    yy <- y[y < ym1 & y < yp1]
  }
  else if (mode == "max") {
    xx <- x[y > ym1 & y > yp1]
    yy <- y[y > ym1 & y > yp1]
  }
  else {
    xx <- x[y > ym1 & y > yp1 | y < ym1 & y < yp1]
    yy <- y[y > ym1 & y > yp1 | y < ym1 & y < yp1]
  }
  list(x = xx, y = yy)
}
#############################################
################## dyUnzoom #################
#############################################
dyUnzoom <-function(dygraph) { #function to add the unzoom button
  dyPlugin(
    dygraph = dygraph,
    name = "Unzoom",
    path = system.file("plugins/unzoom.js", package = "dygraphs")
  )
}

##########################################################################################################
#                                                    ui                                                  #
##########################################################################################################

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Input", tabName = "input_tab", icon = icon("files-o")),
                        menuItem("Features", tabName = "features_tab", icon = icon("sliders")),
                        menuItem("Biometrics", tabName = "biometrics_tab", icon = icon("dashboard"))
                        )
                      ),
                    dashboardBody(
                      tabItems(
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                                                 
                        tabItem(tabName = "input_tab",
                                fluidRow(tags$head(tags$style(type="text/css", "
                                                              #loadmessage {
                                                              position: fixed;
                                                              top: 500px;
                                                              left: 0px;
                                                              width: 100%;
                                                              padding: 5px 0px 5px 0px;
                                                              text-align: center;
                                                              font-weight: bold;
                                                              font-size: 100%;
                                                              color: #000000;
                                                              background-color: #808080;
                                                              z-index: 105;
                                                              }")
                                  ),
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("Loading...",id="loadmessage")
                                  ),
                                  tabBox(title = "", width=NULL,
                                         ################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                         tabPanel(title=tagList(shiny::icon("cloud-download"), "API connect"),
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      actionButton(inputId = "connect_api", label = "API connection",style = "color: black; background-color: #FF8000"),
                                                      dateInput("date",label = "Date input:",value = "2017-08-17",weekstart = 1),
                                                      DT::dataTableOutput("sessionidtable")
                                                    ),
                                                    mainPanel(
                                                      tags$p("first two rows of the Heartrate file"),
                                                      tableOutput("HR_db_table"),
                                                      tags$p("first two rows of the ECG file"),
                                                      tableOutput("ECG_db_table"),
                                                      tags$p("first two rows of the Breathingrate file"),
                                                      tableOutput("BR_db_table"),
                                                      tags$p("first two rows of the Skintemperature file"),
                                                      tableOutput("ST_db_table"),
                                                      tags$p("first two rows of the GSR file"),
                                                      tableOutput("GSR_db_table")
                                                    )
                                                  )
                                         ),
                                         ################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                         tabPanel(title=tagList(shiny::icon("sign-in"), "Data Upload"),
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      fileInput("HR_upload", "Choose Heart Rate CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      ),
                                                      fileInput("ECG_upload", "Choose ECG CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      ),
                                                      fileInput("BR_upload", "Choose Breathing Rate CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      ),
                                                      fileInput("ST_upload", "Choose Skin Temperature CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      ),
                                                      fileInput("GSR_upload", "Choose GSR CSV File",
                                                                accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                                      )
                                                    ),
                                                    mainPanel(
                                                      tags$p("first two rows of the Heartrate file"),
                                                      tableOutput("HR_upload_table"),
                                                      tags$p("first two rows of the ECG file"),
                                                      tableOutput("ECG_upload_table"),
                                                      tags$p("first two rows of the Breathingrate file"),
                                                      tableOutput("BR_upload_table"),
                                                      tags$p("first two rows of the Skintemperature file"),
                                                      tableOutput("ST_upload_table"),
                                                      tags$p("first two rows of the GSR file"),
                                                      tableOutput("GSR_upload_table")
                                                    )
                                                  )
                                         ),
                                         ################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                         ############################ simulation ui #####################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
                                         ################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         

                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                  ) #close tabBox
                                ) #close fluidpage
                                ) #close tabItem "input_tab"

################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                         
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################                                                                 
                      ) #close tabItems
                    ) #close Dashboardbody
) #close Dashboardpage

##########################################################################################################
#                                                    server                                              #
##########################################################################################################
server <- function(input, output) {
  options(shiny.maxRequestSize=60*1024^2) # size of the upload files This is 60MB
  ##################################################################################
  ################# input_tab ######################################################
  ##################################################################################
  
  ################# API connect ####################################################
  day_id <- eventReactive(input$connect_api, {
    pg <- dbDriver("PostgreSQL") 
    db <- dbConnect(drv = pg, 
                    user="xxxxxx",
                    password="xxxxxxx",
                    host="xxxxxxxx",
                    dbname="xxxxx")
    day_query1 <- as.character(input$date)
    day_query2 <- as.character(base::as.Date(day_query1)+1)
    qr <- paste0("SELECT DISTINCT session_id FROM ts_combined WHERE time >= '",day_query1,"' AND time < '",day_query2,"';")
    res <- dbGetQuery(db, qr)
    # closing database
    dbDisconnect(db)
    # sending dataframe
    res
  })
  
  output$sessionidtable <- DT::renderDataTable(day_id(), selection = 'single',options = list(lengthChange = FALSE,searching = FALSE))
  
  raw_db <- eventReactive(input$sessionidtable_cell_clicked, {
    pg <- dbDriver("PostgreSQL") 
    db <- dbConnect(drv = pg, 
                    user="xxxxxxx",
                    password="xxxxxx",
                    host="xxxxxxxx",
                    dbname="eai")
    session_id_query <- input$sessionidtable_cell_clicked$value
    day_query1 <- as.character(input$date)
    day_query2 <- as.character(base::as.Date(day_query1)+1)
    qr <- paste0("SELECT metric, value, EXTRACT(EPOCH FROM time AT TIME ZONE 'UTC') *1000 AS date_part FROM ts_combined WHERE session_id = '",session_id_query,"' AND time >= '",day_query1,"' AND time < '",day_query2,"';")
    res <- dbGetQuery(db, qr) 
    # closing database
    dbDisconnect(db)
    # sending dataframe
    res
  })
  
  api_db_session_date <- reactive({
    if (length(raw_db()) > 0) {
      res <- raw_db() %>%
        distinct(date_part,metric,.keep_all = TRUE) %>% # remove duplicates
        group_by(date_part) %>% # select variables to spread the table
        spread(metric, value) %>% 
        ungroup(date_part) %>%
        mutate(time_date = as.POSIXct(as.numeric(as.character(date_part))/1000, origin = "1970-01-01", tz="Europe/London"))
      res
    }
  })
  output$HR_db_table <- renderTable({
    if (length(grep("heartrate", colnames(api_db_session_date()))) > 0) {
      db_HR <- api_db_session_date() %>%
        select(time_date,heartrate)
      head(db_HR,2)
    }
  })  
  output$ECG_db_table <- renderTable({
    if (length(grep("ECG", colnames(api_db_session_date()))) > 0) {
      db_ECG <- api_db_session_date() %>%
        select(time_date,ecg)
      head(db_ECG,2)
    }
  })
  output$BR_db_table <- renderTable({
    if (length(grep("breathingrate", colnames(api_db_session_date()))) > 0) {
      db_BR <- api_db_session_date() %>%
        select(time_date,breathingrate)
      head(db_BR,2)
    }
  })
  output$ST_db_table <- renderTable({
    if (length(grep("skintemperature", colnames(api_db_session_date()))) > 0) {
      db_ST <- api_db_session_date() %>%
        select(time_date,skintemperature)
      head(db_ST,2)
    }
  })
  output$GSR_db_table <- renderTable({
    if (length(grep("gsr", colnames(api_db_session_date()))) > 0) {
      db_GSR <- api_db_session_date() %>%
        select(time_date,gsr)
      head(db_GSR,2)
    }
  })
  
  ################# Upload data ####################################################
  # upload buttons
  data_HR_upload <- reactive({
    inFile <- input$HR_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  data_ECG_upload <- reactive({
    inFile <- input$ECG_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  data_BR_upload <- reactive({
    inFile <- input$BR_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  data_ST_upload <- reactive({
    inFile <- input$ST_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  data_GSR_upload <- reactive({
    inFile <- input$GSR_upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath)
    data <- dplyr::rename(data, time = Time)
    return(data)
  })
  # upload tables
  output$HR_upload_table <- renderTable({
    head(data_HR_upload(),2)
  })  
  output$ECG_upload_table <- renderTable({
    head(data_ECG_upload(),2)
  })
  output$BR_upload_table <- renderTable({
    head(data_BR_upload(),2)
  })
  output$ST_upload_table <- renderTable({
    head(data_ST_upload(),2)
  })
  output$GSR_upload_table <- renderTable({
    head(data_GSR_upload(),2)
  })
  ################# Data simulation ####################################################
  # "simulate data" button
  data_HR_sim <- eventReactive(input$simulate_data,{
#### enter the code here
  })
  data_ECG_sim <- eventReactive(input$simulate_data,{

  })
  data_BR_sim <- eventReactive(input$simulate_data,{

  })
  data_ST_sim <- eventReactive(input$simulate_data,{

  })
  data_GSR_sim <- eventReactive(input$simulate_data,{

  })
  # simulate tables
  output$HR_simulate_table <- renderTable({
    head(data_HR_sim(),2)
  })  
  output$ECG_simulate_table <- renderTable({
    head(data_ECG_sim(),2)
  })
  output$BR_simulate_table <- renderTable({
    head(data_BR_sim(),2)
  })
  output$ST_simulate_table <- renderTable({
    head(data_ST_sim(),2)
  })
  output$GSR_simulate_table <- renderTable({
    head(data_GSR_sim(),2)
  })
  
}# close server

shinyApp(ui, server)