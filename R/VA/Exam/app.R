
library(tidyverse)
library(shiny)
library(shinythemes)
library(shiny)
library(DT)

ui <- navbarPage(
  theme = shinytheme("simplex"),
  title = "Student Dashboard",
  #navbarMenu(menuName = "Options",
    tabPanel(
             title = "Student Overview",
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 h2("Parameters:"),
                 conditionalPanel(
                   condition = "input.tabs < 3",
                     selectInput(
                     inputId = "xaxis",
                     label = "X-axis:",
                     choices = c(
                       "Student's age" = "age",
                       "Weekly Study Time" = "studytime",
                       "Free time after school" = "freetime",
                       "Going out with Friends" = "goout",
                       "Current Health Status" = "health",
                       "Number of School Absences" = "absences",
                       "Grade in Period 1" = "G1",
                       "Grade in Period 2" = "G2",
                       "Grade in Period 3" = "G3"
                     ),
                     selected = "G1"
                   ),
                 ),
                 conditionalPanel(
                   condition = "input.tabs == 2",
                   selectInput(
                     inputId = "yaxis",
                     label = "Y-axis:",
                     choices = c(
                       "Student's age" = "age",
                       "Weekly Study Time" = "studytime",
                       "Free time after school" = "freetime",
                       "Going out with Friends" = "goout",
                       "Current Health Status" = "health",
                       "Number of School Absences" = "absences",
                       "Grade in Period 1" = "G1",
                       "Grade in Period 2" = "G2",
                       "Grade in Period 3" = "G3"
                     ),
                     selected = "G3"
                   ),
                 ),
                 conditionalPanel(
                   condition = "input.tabs < 3",
                   selectInput(
                     inputId = "color",
                     label = "Colour by:",
                     choices = c(
                       "Subject" = "subject",
                       "School" = "school",
                       "Sex" = "sex",
                       "Family size" = "famsize",
                       "Mother's Education" = "Medu",
                       "Father's Education" = "Fedu"
                     )
                   ),
                 ),
                 h2("Student Filters:"),
                 sliderInput(
                   inputId = "samplesize",
                   label = "Random sample size:",
                   min = 100,
                   max = 1044,
                   value = 522
                 ),
                 radioButtons(
                   inputId = "medu_filter",
                   label = "Filter by Mother's Education:",
                   choices = c(
                     "All" = "ALL",
                     "0 - None" = "0 - none",
                     "1 - Primary Education" = "1 - primary education",
                     "2 - 5th to 9th Grade" = "2 - 5th to 9th grade",
                     "3 - Secondary Education" = "3 - secondary education",
                     "4 - Higher Education" = "4 - higher education"
                   )
                 ),
                 sliderInput(
                   inputId = "agefilter",
                   label = "Filter by age:",
                   min = 15,
                   max = 22,
                   value = c(16, 22)
                 ),
                 actionButton(
                   inputId = "applyfilter",
                   label = "Apply Filters"
                 )
               ),
               mainPanel = mainPanel(
                 tabsetPanel(id = "tabs",
                   tabPanel(value = 1,
                     title = "Bar Chart",
                     plotOutput(
                       outputId = "barchart"
                     ),
                     textOutput(
                       outputId = "batcharttext"
                     )
                   ),
                   tabPanel(value = 2,
                     title = "Scatter Polt",
                     plotOutput(
                       outputId = "scatterplot"
                     ),
                     textOutput(
                       outputId = "scatterplottext"
                     )
                   ),
                   tabPanel(value = 3,
                     title = "Data Table",
                     dataTableOutput(
                       outputId = "datatable"
                     ),
                     textOutput(
                       outputId = "datatabletext"
                     )
                   )
                 )
               )
             )
    ), 
    tabPanel(
      title = "About",
      fluidRow(
        column(
          width = 5,
          h1("About Data:A Summary"),
          textOutput(outputId = "datatext"),
          p("Breakdown of data, grouped by school and sex and summarised"),
          p("by mean and standard deviation of absences, is given below:"),
          tableOutput(outputId = "abouttable"),
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 5,
          h1("About Author"),
          p("My name is Andrew Brown"),
          p(
            "A ",
            a("Queen's University Belfast", href = "http://www.qub.ac.uk"),
            " student."
          ),
        ),
        column(
          width = 4
        ),
        column(
          width = 3,
          img(src = "http://bit.ly/3pQpS4b", height = 150)
        )
      ),
    )
  #)
)

server <- function(
  input, 
  output, 
  session
) {
  
  data <- reactive({
    df = read.csv("student.csv")
    return(df)
  })

  filtered_data <- eventReactive(
    input$applyfilter, {
      df <- data()
      df <- df %>% slice_sample(n = input$samplesize)
      if (input$medu_filter == "ALL") {
      } else {
        df <- df %>% filter(Medu == input$medu_filter)
      }
      df <- df %>% filter(age >= input$agefilter[[1]])
      df <- df %>% filter(age <= input$agefilter[[2]])
      return(df)
    }
  )
  
  output$batcharttext <- renderText({
    total <- nrow(data())
    samples <- nrow(filtered_data())
    text <- paste0(
      "In a sample of ",
      isolate(input$samplesize),
      " randomly selected students, there are ",
      samples,
      " students that satisfy the filter consitions"
    )
    return(text)
  })
  
  output$scatterplottext <- renderText({
    total <- nrow(data())
    samples <- nrow(filtered_data())
    text <- paste0(
      "In a sample of ",
      isolate(input$samplesize),
      " randomly selected students, there are ",
      samples,
      " students that satisfy the filter consitions"
    )
    return(text)
  })
  
  output$datatabletext <- renderText({
    total <- nrow(data())
    samples <- nrow(filtered_data())
    text <- paste0(
      "In a sample of ",
      isolate(input$samplesize),
      " randomly selected students, there are ",
      samples,
      " students that satisfy the filter consitions"
    )
    return(text)
  })
  
  output$datatext <- renderText({
    total <- nrow(data())
    cols <- ncol(data())
    text <- paste0(
      "Size of the entire dataset is: ",
      total,
      " dimensionality is: ",
      cols,
      "!"
    )
    return(text)
  })
  
  output$abouttable <- renderTable({
    df <- data()
    df <- df %>% group_by(sex, school)
    df <- df %>% summarise(
      avg_absences = mean(absences),
      sd_absences = sd(absences)
    )
    return(df)
  })
  
  output$barchart <- renderPlot({
    plot <- ggplot(
      data = filtered_data(),
      mapping = aes_string(
        x = input$xaxis,
        fill = input$color
      )
    )
    plot <- plot + geom_bar(position = "dodge")
    return(plot)
  })
  
  output$scatterplot <- renderPlot({
    plot <- ggplot(
      data = filtered_data(),
      mapping = aes_string(
        x = input$xaxis,
        y = input$yaxis
      )
    )
    plot <- plot + geom_smooth()
    plot <- plot + geom_point(aes_string(color = input$color))
    return(plot)
  })
  
  output$datatable <- DT::renderDataTable({
    return(filtered_data())
  })
  
}

shinyApp(ui, server)
