

library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(quantmod)
library(readxl)

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
    df_precios_medios <- ((dataInput()[paste(isolate(input$dia_comienzo),isolate(input$dia_fin),sep="/")][,2]+
                           dataInput()[paste(isolate(input$dia_comienzo),isolate(input$dia_fin),sep="/")][,3])/2)
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
    
    df_precios_limits <- na.omit((dataInput()[,2]+dataInput()[,3])/2)
    
    fechas_umbral_compra_limits1 <- NULL
    for (i in (2:nrow(df_precios_limits))){
      contador1 <- i-1
      if (df_precios_limits[i,] <= isolate(input$precio_compra_limits1) && df_precios_limits[contador1,] > isolate(input$precio_compra_limits1)){
        fechas_umbral_compra_limits1 <- c(fechas_umbral_compra_limits1,i)
      }
    }
    fechas_umbral_compra_limits2 <- NULL
    for (j in (2:nrow(df_precios_limits))){
      contador2 <- j-1
      if (df_precios_limits[j,] <= isolate(input$precio_compra_limits2) && df_precios_limits[contador2,] > isolate(input$precio_compra_limits2)){
        fechas_umbral_compra_limits2 <- c(fechas_umbral_compra_limits2,j)
    }}
    
      fechas_umbral_venta_limits <- NULL
    for (k in (2:nrow(df_precios_limits))){
      contador3 <- k-1
      if (df_precios_limits[k,] >= isolate(input$precio_venta_limits) && df_precios_limits[contador3,] < isolate(input$precio_venta_limits)){
        fechas_umbral_venta_limits <- c(fechas_umbral_venta_limits,k)
      }}
      
      #Definimos los valores, vectores y contadores que vamos a utilizar en el bucle
      resultado_neto_1 <- NULL
      resultado_neto_2 <- NULL
      resultado_ventas_1 <- NULL
      resultado_ventas_2 <- NULL
      total_compra_limits_1 <- NULL
      total_compra_limits_2 <- NULL
      resultado_ventas <- NULL
      contador_1 <- 1
      contador_compra_1 <- c(1)
      contador_compra_2 <- c(1)
      contador_while_1 <- 0
      contador_while_2 <- 0
      contador_2 <- 1
      resultado <- NULL
      precios_compra_limits2 <- NULL
      importe_compra_limits2 <- NULL
      cantidad_total_compra_limits2 <- NULL
      precios_compra_limits1 <- NULL
      importe_compra_limits1 <- NULL
      cantidad_total_compra_limits1 <- NULL
      
      for (l in (1:length(fechas_umbral_venta_limits))){
        while(fechas_umbral_venta_limits[l] > fechas_umbral_compra_limits1[contador_1] && 
              contador_1<length(fechas_umbral_compra_limits1+2)){
          for (t in (1:length(fechas_umbral_compra_limits1))){
            if(fechas_umbral_venta_limits[l] > fechas_umbral_compra_limits1[t]){
              precios_compra_limits1 <- c(precios_compra_limits1,df_precios_limits[fechas_umbral_compra_limits1[t],])
              cantidad_total_compra_limits1 <- c(cantidad_total_compra_limits1,isolate(input$cantidad_compra_limits1))
              importe_compra_limits1 <- c(importe_compra_limits1, precios_compra_limits1[t]*isolate(input$cantidad_compra_limits1))
              contador_1 <- contador_1 +1
            }
          }
          contador_while_1 <- contador_while_1+1
          contador_compra_1 <- c(contador_compra_1,length(importe_compra_limits1))
        }
        while(fechas_umbral_venta_limits[l] > fechas_umbral_compra_limits2[contador_2] && 
              contador_2<length(fechas_umbral_compra_limits2+2)){
          for (t in (1:length(fechas_umbral_compra_limits2))){
            if(fechas_umbral_venta_limits[l] > fechas_umbral_compra_limits2[t]){
              precios_compra_limits2 <- c(precios_compra_limits2,df_precios_limits[fechas_umbral_compra_limits2[t],])
              cantidad_total_compra_limits2 <- c(cantidad_total_compra_limits2,isolate(input$cantidad_compra_limits2))
              importe_compra_limits2 <- c(importe_compra_limits2, precios_compra_limits2[t]*isolate(input$cantidad_compra_limits2))
              contador_2 <- contador_2 +1
            }
          }
          contador_while_2 <- contador_while_2+1
          contador_compra_2 <- c(contador_compra_2,length(importe_compra_limits2))
        }
        
        if(length(contador_compra_1)>l){
          resultado_ventas_1 <- c(resultado_ventas_1,sum(cantidad_total_compra_limits1[contador_compra_1[l+1]-contador_compra_1[l]+1])* df_precios_limits[fechas_umbral_venta_limits[l],])
          total_compra_limits_1 <- c(total_compra_limits_1,sum(importe_compra_limits1[contador_compra_1[l+1]-contador_compra_1[l]+1]))
          resultado_neto_1 <- c(resultado_neto_1,resultado_ventas_1[l]-total_compra_limits_1[l])
        }
        if(length(contador_compra_1)>l){
          resultado_ventas_2 <- c(resultado_ventas_2,sum(cantidad_total_compra_limits2[contador_compra_2[l+1]-contador_compra_2[l]+1])* df_precios_limits[fechas_umbral_venta_limits[l],])
          total_compra_limits_2 <- c(total_compra_limits_2,sum(importe_compra_limits2[contador_compra_2[l+1]-contador_compra_2[l]+1]))
          resultado_neto_2 <- c(resultado_neto_2,resultado_ventas_2[l]-total_compra_limits_2[l])
        }
      }
      
      resultado_total <- sum(resultado_neto_1) + sum(resultado_neto_2)
      
    
  
    #Cuando el usuario utilice el action button se hara un print del resultado obtenido con esta estrategia
    if (!is.null(valores$limit)){
      print(paste0("You would gain/loss"," ",round(resultado_total,2),"€"))
    }
    
  })
  
  
  output$plot <- renderPlot({
    
    chartSeries(dataInput()[,4], theme = chartTheme("white"),name=valores_ibex[valores_ibex[,2]== input$stocks,1],
                type = "line", TA = NULL)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

