

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
                   end = as.character(Sys.Date())),
    sidebarMenu(
      menuItem("Ramdom",tabName = "Random_strategy",icon=icon("line_chart")),
      menuItem("Limits",tabName = "Limits_strategy",icon=icon("balance-scale")),
      menuItem("Programming",tabName = "Programming_strategy",icon=icon("edit"))
    )
    
  ),
  dashboardBody(
    box(plotOutput("plot")),
    tabItems(
      tabItem(tabName = "Random_strategy",
              fluidPage(
                mainPanel(
                  verbatimTextOutput("muestra"),
                    fluidRow(
                      column(6,numericInput("cantidad_comprada",
                                            label = "Buy",
                                            value = 10),
                             numericInput("cantidad_vendida",
                                          label = "Sell",
                                          value = 5)
                      ),
                      column(6,numericInput("porcentaje_compras",
                                            label = "% of the days",
                                            value = 10),
                             numericInput("porcentaje_ventas",
                                          label = "% of the days",
                                          value = 2)
                             )
                    )))),
      tabItem(tabName = "Limits_strategy",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(),
                  mainPanel()
                  ))),
      tabItem(tabName = "Programming_strategy",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(),
                  mainPanel()
                )))
    )
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

