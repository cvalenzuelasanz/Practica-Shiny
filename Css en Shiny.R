tags$div(style= "position: absolute; left: 10%; top: 0%",
         tags$input(type= "text",ID= "cantidad_comprada",
                    tags$label(style = "position: absolute; right: 110%; botton:0","buy"),
                    value = 1)),
tags$div(style= "position: absolute; right: 40%; top: 0%",
         tags$input(type="text",ID= "porcentaje_dias_compra",
                    tags$label(style="position: float; left: 100%; botton: 0%","% of days"),
                    value=0)),

tags$div(style= "position: absolute; left: 0%; botton: 50%",
         tags$input(type="date",ID="start_date",
                    tags$label(style = "position: absolute; right: 110%; botton:0","start date"),
                    value = "2017-01-01"))
))),


# tags$div(style = "position: absolute; left: 100%; top: 100",
#   dateInput("end_date","end date",value = "2017-12-01")),
# actionButton("Run_random","Run")
# ))),