library(shiny)
library(ggplot2)

ui <- shinyUI(
  fluidPage(
   plotOutput("gráfica",hover="clickGrafica"),
   tableOutput("texto")
  )
)




server <- shinyServer(function(input,output){
  output$gráfica <- renderPlot(
    ggplot(mpg) +
    geom_point(aes(x = cty, y = hwy))
    )
  
  output$texto <- renderTable({

    #toString(nearPoints(mpg,input$clickGrafica))
    nearPoints(mpg,input$clickGrafica,threshold = 5)
    
    })
    
  }
)

shinyApp(ui, server)

