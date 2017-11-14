library(shiny)
library(ggplot2)
function(input, output) {
  output$grafica <- renderPlot({
    
      graficaBase <- ggplot(mpg, aes_string(x=input$variablex, y=input$variabley))
      
      
      if (input$facet & input$color){
        graficaBase + facet_wrap(~manufacturer, ncol=4)+
          geom_point(aes(colour=class)) 
      }
      else{
        if(input$color & !input$facet) {graficaBase + geom_point(aes(colour=class))}
        
        else{
          if(input$facet) {graficaBase + facet_wrap(~manufacturer, ncol=4)+
            geom_point()}
          
          else{graficaBase + geom_point()}
        }}
    })
}