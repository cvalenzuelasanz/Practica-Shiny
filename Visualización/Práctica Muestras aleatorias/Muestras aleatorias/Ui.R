library(shiny)

shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        numericInput("muestra",
                    label="Tamaño muestral",
                    value = 100),
        numericInput("correlacion",
                    label="Coeficiente de Correlación de Pearson",
                    value = 1)),
        
      actionButton("boton_muestra", "Generar muestra")
      ),
      mainPanel(
        verbatimTextOutput("texto"),
        plotOutput("grafica",click = "plot_click")
      )
    )
  )
