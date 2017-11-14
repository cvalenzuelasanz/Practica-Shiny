library(shiny)
library(ggplot2)
library(MBESS)
library(matrixcalc)
library(MASS)

function(input, output) {

  correlacion <- eventReactive(input$boton_muestra,{
    input$correlacion
  })
  
  tamaño_muestral <- eventReactive(input$boton_muestra,{
    input$muestra
  })
  v <- reactiveValues(
    click1 = NULL  
  )
  
  observeEvent(input$plot_click, {
    v$click1 <- input$plot_click
  })
    
  output$texto <- renderText({
    correlacion()
    #hay que añadir dos parentésis despues de muestras, porque es una función reactive.
  })
  
  output$grafica <- renderPlot({
    
    
    #La correlación y el tamaño muestral vienen definidos en UI
    #y los valores dependen de lo que seleccione el usuario
    
    # Definimos la desviación típica y la media de las dos variables como vectores de dos elementos
    desviacion_tipica <- c(0.5,1)
    media_variables <- c(2,4)
    
    #Definimos la matriz de correlación como una matriz 
    #que depende del valor de la correlación establecida por el usuario
    
    matriz_corr <- matrix(c(1,correlacion(),correlacion(),1),nrow =2)
    
    #Calculamos la matriz de covarianzas en función de la matriz de correlaciones y la desviación típica fijada
    #mediante el uso de la función Cor2Cov
    
    matriz_cov <- cor2cov(matriz_corr,desviacion_tipica)
    
    #Las muestras aleatorias bivariantes se generan mediante la función mvrornm.
    #En está función hay que incluir el tamaño muestral, la media de las variables
    # y la matriz de covarianzas (que depende de la matriz de correlación y la desviación típica)
    
    muestra <- data.frame(mvrnorm(tamaño_muestral(),media_variables,matriz_cov))
    #cambiamos el nombre de las columnas del Dataframe muestras
    colnames(muestra, do.NULL = FALSE)
    colnames(muestra) <- c("X","Y")
    
    #representación gráfica
    grafico_dispersion <- ggplot(muestra, aes(x= X,y = Y)) + geom_point()
    print(grafico_dispersion)
    
    if (!is.null(v$click1$x))
      print(paste(v$click1$x, v$click1$y, sep = " / "))
    
  })
}