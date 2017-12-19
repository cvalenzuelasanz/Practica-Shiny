

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
                selected = 1),
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
                           dateInput("dia_comienzo","start day",format = "yyyy-mm-dd",value="2017-01-01"),
                           actionButton("Run_random","Run")
                    ),
                    column(6,numericInput("porcentaje_dias_compra_random",
                                          label = "% of the days",
                                          value = 10),
                           numericInput("porcentaje_dias_venta_random",
                                        label = "% of the days",
                                        value = 2),
                           dateInput("dia_fin","End day",format = "yyyy-mm-dd",value="2017-12-01")
                           
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
  
  
  valores <- reactiveValues(random= NULL,limit= NULL)
  
  dataInput <- reactive({
    getSymbols(input$stocks, src = "yahoo", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  observeEvent(input$Run_random,{
    valores$random <- rnorm(1)
  })
  
  observeEvent(input$Run_limit,{
    valores$limit <- rnorm(1)
  })
  
  output$resultado_random <- renderText({
    #creamos un xts con los precios medios de cada día del periodo de la estrategia aleatoria
    #Hemos considerado los precios medios como la media entre el precio mínimo y el precio máximo
    #(Columnas 2 y 3) del objeto reactivo dataInput
    df_precios_medios <- ((isolate(dataInput())[paste(isolate(input$dia_comienzo),isolate(input$dia_fin),sep="/")][,2]+
                           isolate(dataInput())[paste(isolate(input$dia_comienzo),isolate(input$dia_fin),sep="/")][,3])/2)
    #contamos el numero de dias que tiene la estategia aleatoria
    numero_dias <- nrow(df_precios_medios)
    #Hacemos un muestreo del numero de dias de compra
    #en funcion del porcentaje de dias de compra definido por el usuario
    dias_compra <- sample(numero_dias,numero_dias*(isolate(input$porcentaje_dias_compra_random)/100))
    #del xts df precios medios nos quedamos solo con los precios medios de los dias muestreados
    #los multiplicamos por la cantidad de compra definida por el usuario
    #de esta manera obtenemos el importe comprado cada dia
    df_compra <- df_precios_medios[c(dias_compra),]* isolate(input$cantidad_compra_random)
    #Calculamos el precio medio de compra como la suma de las cantidades compradas cada dia
    #dividido entre el numero de dias que se han comprado acciones
    precio_compra_medio <- sum(df_compra)/(isolate(input$cantidad_compra_random)*(numero_dias*(isolate(input$porcentaje_dias_compra_random)/100)))
    
    #Hacemos un muestreo del numero de dias de venta
    #en funcion del porcentaje de dias de compra definido por el usuario
    dias_venta <- sample(numero_dias,numero_dias*(isolate(input$porcentaje_dias_venta_random)/100))
    #del xts df precios medios nos quedamos solo con los precios medios de los dias muestreados
    #los multiplicamos por la cantidad de venta definida por el usuario
    #de esta manera obtenemos el importe vendido cada dia
    df_ventas <- df_precios_medios[c(dias_venta),]* isolate(input$cantidad_venta_random)
    #Calculamos el precio medio de venta como la suma de las cantidades vendidas cada dia
    #dividido entre el numero de dias que se han vendido acciones
    precio_venta_medio <- sum(df_ventas)/(isolate(input$cantidad_venta_random)*(numero_dias*(isolate(input$porcentaje_dias_venta_random)/100)))
    
    #El resultado de esta estrategia es la diferencia entre el precio medio de venta y el precio medio compra
    #multiplicado por el numero de dias que se han realizado ventas
    #no hemos tenido en cuenta para este analisis si hay titulos por vender (posicion larga del usuario) o 
    #si por el contrario se han vendido mas títulos de los comprados (posicion corta)
    resultado_rd <- (precio_venta_medio-precio_compra_medio)*(numero_dias*(isolate(input$porcentaje_dias_venta_random)/100))
    
    #Cuando el usuario utilice el action button se hara un print del resultado obtenido con esta estrategia
    if (!is.null(valores$random)){
      print(paste0("You would gain/loss"," ",resultado_rd,"€"))
    }
    
    
  })

  output$resultado_limits <- renderText({
    #Calculamos el importe de compra total como la suma del importe de compra con el primer y segundo precio
    importe_compra_limits <- isolate(input$precio_compra_limits1)* isolate(input$cantidad_compra_limits1) +
    isolate(input$precio_compra_limits2) * isolate(input$cantidad_compra_limits2)
    #Calculamos el importe de venta como el precio de venta mas la cantidad total de titulos comprados (m1 + m2)
    importe_venta_limits <-  isolate(input$precio_venta_limits) * (isolate(input$cantidad_compra_limits1)+isolate(input$cantidad_compra_limits2))
    
    #El resultado de la estrategia es la diferencia entre importe total de compra y venta
    resultado_lm <- importe_venta_limits - importe_compra_limits
    
    #Cuando el usuario utilice el action button se hara un print del resultado obtenido con esta estrategia
    if (!is.null(valores$limit)){
      print(paste0("You would gain/loss"," ",resultado_lm,"€"))
    }
    
  })
  
  
  output$plot <- renderPlot({
    
    chartSeries(dataInput()[,4], theme = chartTheme("white"),name=valores_ibex[valores_ibex[,2]== input$stocks,1],
                type = "line", TA = NULL)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

