
shinyUI(fluidPage(
  
  titlePanel("Predictive Dashboard"),
  fluidRow(column(width= 8,  textOutput("text1"))),
  fluidRow(

  column(width=2,
         
         radioButtons("checkGroup", 
                            label = "Prediction length", 
                            choices = list("30 days" = 30, 
                                           "45 days" = 45, 
                                           "60 days" = 60), 
                      selected = 30)
  
         ),
  column(width=4, uiOutput("choose_dept_name")
         ),
  column(width=4, uiOutput("choose_time")                     
        )
  
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
