

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
  dashboardHeader(title = "Simulador de stock"),
  dashboardSidebar(
    selectInput(inputId = "stocks",
                label = "Select stock",
                choices = list(nombres = valores),
                selected = 4),
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
                           dateInput("dia_comienzo","start day",format = "yyyy-mm-dd",value="2015-01-01"),
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
                                           value = 11)),
                    column(6,
                           numericInput("cantidad_compra_limits1",
                                        label = "Buy",
                                        value = 5000),
                           numericInput("cantidad_compra_limits2",
                                        label = "Buy",
                                        value = 5000),
                           actionButton("Run_limit","Run")),
                    column(6, 
                           numericInput("precio_compra_limits1",
                                        label = "When stock price reaches",
                                        value = 10),
                           numericInput("precio_compra_limits2",
                                        label = "When stock price reaches",
                                        value = 9)
                           
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
  
  dataset_precios <- function(dataset,inicio,fin){
    #xts con los precios de cierre de cada día del periodo de la estrategia aleatoria
    df_precios <- (dataset[paste(inicio,fin,sep="/")][,4])
    return(df_precios)
  }
  
  precios_aleatorios <- function(df,porcentaje,cantidad){
    
    #contamos el numero de dias que tiene la estategia aleatoria
    numero_dias <- nrow(df)
    
    #total dias = numero dias por porcentaje de dias
    total_dias <- numero_dias* (porcentaje/100)
    
    #muestreo del numero de dias da en funcion del porcentaje de dias de compra
    dias_muestreo <- sample(numero_dias,total_dias)
    
    #importe = precios dia por la cantidad fijada
    df_cantidades <- df[c(dias_muestreo),]* cantidad
    
    #precio medio =  suma cantidade dividido numero de dias
    precio_medio <- sum(df_cantidades[,1])/(cantidad*total_dias) 
    
    return(list(pvp_medio = precio_medio,importe_total = sum(df_cantidades[,1]),dias=total_dias))
  }
  
  estrategia_aleatoria <- function(bbdd,dia_inicial,dia_final,porcentaje_dias_compra,
                                   cantidad_compra_rd,porcentaje_dias_venta,cantidad_venta_rd){
    
    df_aleatorio <- dataset_precios(dataset= bbdd,inicio=dia_inicial,fin=dia_final)
    
    compra <- precios_aleatorios(df = df_aleatorio,
                                 porcentaje = porcentaje_dias_compra,
                                 cantidad = cantidad_compra_rd)
    
    venta <- precios_aleatorios(df = df_aleatorio ,
                                porcentaje= porcentaje_dias_venta,
                                cantidad = cantidad_venta_rd)
    
    resultado_rd <- round((venta$pvp_medio - compra$pvp_medio)*venta$dias*cantidad_venta_rd,0)
    
    Posicion_neta <- -round(venta$importe_total-compra$importe_total,0)
    
    print(paste0("You would gain/loss"," ",resultado_rd," and the net position is"," ",Posicion_neta))
  }
  
  
  #Funcion ara encontrar los dias en los que el precio se encuentra dentro del intervalo de compra
  Umbrales_compra <- function(df_precios_limits,precio){
    umbral <- NULL
    for (i in (1:nrow(df_precios_limits))){
      if (df_precios_limits[i,] <= precio){
        umbral <- c(umbral,i)
      }
    }
    return(umbral)
  }
  
  #Funcion ara encontrar los dias en los que el precio se encuentra dentro del intervalo de venta
  Umbrales_venta <- function(df_precios_limits,precio){
    umbral <- NULL
    for (k in (1:nrow(df_precios_limits))){
      if (df_precios_limits[k,] >= precio){
        umbral <- c(umbral,k)
      }}
    return(umbral)
  }
  
  #La siguiente funcion obtiene el resultado de la estrategia de inversion de acuerdo a los
  #umbrales de compra y venta y de las cantidades de compra establecidad
  Resultado_limits <- function(df_precios_limits,umbral_venta,umbral_compra,cantidad_compra){
    
    contador <- 1
    contador_compra <- c(1)
    precios <- NULL
    vector_compras <- NULL
    vector_total_compras <- NULL
    vector_ventas <- NULL
    vector_compras_totales <- NULL
    resultado_neto <- NULL
    
    #siempre y cuando la fecha de venta sea posterior a la fecha de compra
    #se registra el importe total de compra (precio por cantidad)
    #Definimos el resultado de venta como la diferencia entre cantidad compra
    #y la cantidad vendida

    
    for (l in (1:length(umbral_venta))){
      while(umbral_venta[l] > umbral_compra[contador] && 
            contador<length(umbral_compra+2)){
        for (t in (1:length(umbral_compra))){
          if(umbral_venta[l] > umbral_compra[t]){
            precios <- c(precios,df_precios_limits[umbral_compra[t],])
            vector_compras <- c(vector_compras,cantidad_compra)
            vector_total_compras <- c(vector_total_compras, 
                                      precios[t]*cantidad_compra)
            contador <- contador +1
          }
        }
        contador_compra <- c(contador_compra,length(vector_total_compras))
      }
      if(length(contador_compra)>l){
        vector_ventas <- c(vector_ventas,sum(vector_compras[contador_compra[l+1]-
                                                              contador_compra[l]+1])*
                             df_precios_limits[umbral_venta[l],])
        vector_compras_totales <- c(vector_compras_totales,
                                    sum(vector_total_compras[contador_compra[l+1]
                                                             -contador_compra[l]+1]))
        resultado_neto <- c(resultado_neto,vector_ventas[l]-vector_compras_totales [l])
      }
    }
    return(resultado_neto)
  }
  
  estrategia_limits <- function(bbdd,precio_compra1,precio_compra2,
                                precio_venta,cantidad_compra1,cantidad_compra2){
    
    
    umbral_compra1 <- Umbrales_compra(df_precios_limits = bbdd[,4],precio = precio_compra1) 
    umbral_compra2 <- Umbrales_compra(df_precios_limits = bbdd[,4],precio = precio_compra2)
    umbral_ventas <- Umbrales_venta(df_precios_limits = bbdd[,4],precio = precio_venta)
    
    Resultado_1 <- Resultado_limits(df_precios_limits =bbdd[,4],umbral_venta = umbral_ventas,
                                    umbral_compra = umbral_compra1,cantidad_compra = cantidad_compra1)
    Resultado_2 <- Resultado_limits(df_precios_limits =bbdd[,4],umbral_venta = umbral_ventas,
                                    umbral_compra = umbral_compra2,cantidad_compra = cantidad_compra2)
    
    resultado_total <- sum(Resultado_1) + sum(Resultado_2)
    
    print(paste0("You would gain/loss"," ",round(resultado_total,2)))
    
  }
  
  
  observeEvent(input$Run_random,{
    output$resultado_random <- renderText(
      
      estrategia_aleatoria(bbdd= isolate(dataInput()),
                           dia_inicial = isolate(input$dia_comienzo),
                           dia_final = isolate(input$dia_fin),
                           porcentaje_dias_compra= isolate(input$porcentaje_dias_compra_random) ,
                           cantidad_compra_rd = isolate(input$cantidad_compra_random),
                           porcentaje_dias_venta = isolate(input$porcentaje_dias_venta_random),
                           cantidad_venta_rd = isolate(input$cantidad_venta_random))
    )
  })
  
  
  observeEvent(input$Run_limit,{
    
    output$resultado_limits <- renderText(
      estrategia_limits(bbdd = isolate(dataInput()),
                        precio_compra1 = isolate(input$precio_compra_limits1),
                        precio_compra2 = isolate(input$precio_compra_limits2),
                        precio_venta= isolate(input$precio_venta_limits),
                        cantidad_compra1 =isolate(input$cantidad_compra_limits1),
                        cantidad_compra2 = isolate(input$cantidad_compra_limits2))
    )
  })
  
  
  output$plot <- renderPlot({
    
    chartSeries(dataInput()[,4], theme = chartTheme("white"),name=valores_ibex[valores_ibex[,2]== input$stocks,1],
                type = "line", TA = NULL)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

