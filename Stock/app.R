

library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(quantmod)

#Definimos las fechas del historico

valores_ibex <- read_excel("C:/Users/valen/Desktop/Master Datascience/Visualizacion/Practicas/Stock/MscDatascience/Stock/Valores IBEX.xlsx", col_names = FALSE)
valores_ibex <- as.data.frame(valores_ibex)
nombres <- valores_ibex[,1]
valores <- valores_ibex[,2]

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    selectInput(inputId = "stocks",
                label = "Select stock",
                choices = list(nombres = valores),
                selected = 1
    ),
    dateRangeInput("dates", 
                   "Date range",
                   start = "2013-01-01", 
                   end = as.character(Sys.Date())),
    sidebarMenu(
      menuItem("Ramdom",tabName = "Random_strategy",icon=icon("line_chart")),
      menuItem("Limits",tabName = "Limits_strategy",icon=icon("balance-scale"))
    )
  ),
  dashboardBody(
    box(plotOutput("plot")),
    tabItems(
      tabItem(tabName = "Random_strategy",
              fluidPage(
                mainPanel(
                  fluidRow(
                    column(6,numericInput("cantidad_compra_random",
                                          label = "Buy",
                                          value = 10),
                           numericInput("cantidad_venta_random",
                                        label = "Sell",
                                        value = 5),
                           dateInput("dia_comienzo","start day",value="2017-01-01"),
                           actionButton("Run_random","Run")
                    ),
                    column(6,numericInput("porcentaje_dias_compra_random",
                                          label = "% of the days",
                                          value = 10),
                           numericInput("porcentaje_dias_venta_random",
                                        label = "% of the days",
                                        value = 2),
                           dateInput("dia_fin","End day",value="2017-12-01")
                           
                    ),
                    verbatimTextOutput("resultado_random")
                  )))),
      tabItem(tabName = "Limits_strategy",
              fluidPage(
                mainPanel(
                  fluidRow(
                    column(12,numericInput("precio_venta_limits",
                                           label = "Sell when stock price reaches",
                                           value = 10)),
                    column(4,
                           numericInput("cantidad_compra_limits1",
                                        label = "Buy",
                                        value = 5),
                           numericInput("cantidad_compra_limits2",
                                        label = "Buy",
                                        value = 5),
                           actionButton("Run_limit","Run")),
                    column(8, 
                           numericInput("precio_compra_limits1",
                                        label = "When stock price reaches",
                                        value = 5),
                           numericInput("precio_compra_limits2",
                                        label = "When stock price reaches",
                                        value = 5)
                    ),
                    verbatimTextOutput("resultado_limits")
                  ))))
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
    
    chartSeries(dataInput(), theme = chartTheme("white"),name=valores_ibex[valores_ibex[,2]== input$stocks,1],
                type = "line", TA = NULL)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

