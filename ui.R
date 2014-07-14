
shinyUI(fluidPage(
  
  titlePanel("Predictive Dashboard"),
  fluidRow(
  column(width=5,
         
         radioButtons("checkGroup", 
                            label = "Prediction length", 
                            choices = list("30 days" = 30, 
                                           "45 days" = 45, 
                                           "60 days" = 60), 
                      selected = 30)
  
         ),
  column(width=5, uiOutput("choose_dept_name"))
  ),
  tabsetPanel("Header",
    tabPanel("Scatterplot",
             plotOutput("distPlot")
    ),
    tabPanel("Heatmap"
             ,plotOutput("heatmapchart")
    ),
    tabPanel("Time series prediction",
             plotOutput("forecastplot")
             
    ),
    tabPanel("Geographic Info",
             plotOutput("geoplot")
             
    )
  )
))
