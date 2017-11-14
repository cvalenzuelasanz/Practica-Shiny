library(shiny)
library(ggplot2)

function(input, output) {
  output$grafica <- renderPlot({
    ggplot(mpg, aes_string(x = input$variablex, y = input$variabley)) +
    geom_point()}
  )
  output$tabla1 <- renderTable({
    nearPoints(mpg,input$plot_click)})
  output$tabla2 <- renderTable({
    nearPoints(mpg,input$plot_dblclick)})
  output$tabla3 <- renderTable({
    nearPoints(mpg,input$plot_hover)})
}