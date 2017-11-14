library(shiny)
library(ggplot2)
shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("variablex",
                    label="Eje horizontal",
                    choices= c("Cylinders"="cyl",
                               "Miles per galon"="hwy",
                               "Displacement"="displ")),
        selectInput("variabley",
                    label="Eje vertical",
                    choices= c("Cylinders"="cyl",
                               "Miles per galon"="hwy",
                               "Displacement"="displ"))
      ),
      mainPanel(
        plotOutput("grafica",click = "plot_click", dblclick = "plot_dblclick",hover= "plot_hover"),
        tabsetPanel(type = "tabs",
            tabPanel("Click",tableOutput("tabla1")),
            tabPanel("DobleClick", tableOutput("tabla2")),
            tabPanel("Hover", tableOutput("tabla3"))
          )
        )
      )
    )
)
