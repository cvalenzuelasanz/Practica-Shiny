library(shiny)
library(ggplot2)
function(input, output) {
  output$grafica <- renderPlot({
    
      graficaBase <- ggplot(mpg, aes_string(x=input$variablex, y=input$variabley)) +
        geom_point()
      
      
      if (input$facet)
        graficaBase + facet_wrap(~manufacturer, ncol=4)
      else
        graficaBase

    
    })

  output$texto <- renderText(
    
  paste0("Este el diagrama de de dispersión de ",input$variablex," y ",input$variabley,collapse = ", ")
    
  )
}