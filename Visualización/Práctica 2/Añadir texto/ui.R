  library(shiny)

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
                               "Displacement"="displ")),
        
        checkboxInput("facet", label="División", value=F)
        
      ),
      mainPanel(
        textOutput("texto"),
        plotOutput("grafica")
        
      )
    )
  )
)