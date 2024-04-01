
#library(shiny)
library(ggplot2)
library(DT)
library(readr)
library(dplyr)
library(tools)

data <- read.csv("death_1942_1943.csv", stringsAsFactors = FALSE)
data$Residence <- as.factor(data$Residence)
data <- read.csv("death_1942_1943.csv", stringsAsFactors = FALSE)
data$Residence <- as.factor(data$Residence)
data$Birthplace_First_Letter <- substr(data$Birthplace, 1, 1)
data$Residence_First_Letter <- substr(data$Residence, 1, 1)

ui <- fluidPage(
  titlePanel("Auschwitz Death Certificates Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataSelection",
                  "Select Data for Graph:",
                  choices = c("Birthplace", "Residence", "Religion")),
      uiOutput("dynamicInput")
    ),
    mainPanel(
      plotOutput("murderPlot"),
      DTOutput("murderTable")
    )
  )
)

server <- function(input, output, session) {
  output$dynamicInput <- renderUI({
    if (input$dataSelection %in% c("Birthplace", "Residence")) {
      choices <- sort(unique(c(data$Birthplace_First_Letter, data$Residence_First_Letter)))
      checkboxGroupInput("selectedFactors",
                         "Select Letters for Graph:",
                         choices = choices,
                         selected = choices[1])
    } else {
      checkboxGroupInput("selectedFactors",
                         "Select Religions:",
                         choices = sort(unique(data$Religion)),
                         selected = unique(data$Religion)[1])
    }
  })
  
  observe({
    plotData <- reactive({
      if (input$dataSelection %in% c("Birthplace", "Residence")) {
        letterField <- ifelse(input$dataSelection == "Birthplace", "Birthplace_First_Letter", "Residence_First_Letter")
        filterField <- input$selectedFactors
        
        data %>%
          filter(get(letterField) %in% filterField) %>%
          count(!!sym(letterField)) %>%
          rename(Count = n, Factor = !!sym(letterField))
      } else {
        data %>%
          filter(Religion %in% input$selectedFactors) %>%
          count(Religion) %>%
          rename(Count = n, Factor = Religion)
      }
    })
    
    output$murderPlot <- renderPlot({
      ggplot(plotData(), aes(x = Factor, y = Count, fill = Factor)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "", y = "Count", 
             title = paste("Number of People Murdered by", ifelse(input$dataSelection %in% c("Birthplace", "Residence"), "First Letter of", ""), input$dataSelection)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$murderTable <- renderDT({
      datatable(plotData(), options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}

shinyApp(ui = ui, server = server)


