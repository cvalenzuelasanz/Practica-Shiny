

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
                 
                  tags$div(style= "position: absolute; left: 10%; top: 0%",
                    tags$input(type= "text",ID= "cantidad_comprada",
                             tags$label(style = "position: absolute; right: 110%; botton:0","buy"),
                             value = 1)),
                  tags$div(style= "position: absolute; right: 40%; top: 0%",
                    tags$input(type="text",ID= "porcentaje_dias_compra",
                           tags$label(style="position: float; left: 100%; botton: 0%","% of days"),
                           value=0))
                  # tags$div(style = "position: absolute; right: 100%; top: 100",
                  #          dateInput("start_date","start date",value = "2017-01-01")),
                  # tags$div(style = "position: absolute; left: 100%; top: 100",
                  #   dateInput("end_date","end date",value = "2017-12-01")),
                  # actionButton("Run_random","Run")
                  ))),
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

