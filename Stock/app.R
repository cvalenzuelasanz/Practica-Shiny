

library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(quantmod)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      selectInput(inputId = "stocks",
                  label = "Select stock",
                  choices = list("Google" = "GOOG","Apple"= "AAPL","Microsoft"= "MSFT","Banco Santander" = "SAN.MC",
                                 "BBVA" = "BBVA.MC"),
                  selected = 1
    ),
    dateRangeInput("dates", 
                   "Date range",
                   start = "2013-01-01", 
                   end = as.character(Sys.Date()))
    
),
    dashboardBody(
      box(plotOutput("plot"))
    )
)


server <- function(input, output) {
  
  
    dataInput <- reactive({
      getSymbols(input$stocks, src = "yahoo", 
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)
    })

  
  output$plot <- renderPlot({
    
    chartSeries(dataInput(), theme = chartTheme("white"),
                type = "line", TA = NULL)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

