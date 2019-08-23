library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Selectivity (adjustable)" = "superzip",
  "Hospital monthly expenses" = "centile",
  "Selectivity" = "treatment",
  "Patients" = "income",
  "Enrollment" = "adultpop", 
  "log(Enrollment)" = "l_enorllment"
)





shinyUI(navbarPage("Hospital Servicability", id="nav", 

  tabPanel("Interactive map",
    div(class="outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      leafletMap("map", width="100%", height="100%",
        initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(
          center = c(6.46, 3.40),
          zoom = 6,
          maxBounds = list(list(4.483,2.711), list(13.368,13.618)) # Show Nigeria
        )
      ),
      
      absolutePanel(
       id = "controls", 
       #class = "modal-body", 
        fixed = TRUE, draggable = TRUE,
        top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        
        h2("Health Services Explorer"),
        
        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "Selectivity threshold (admit rate less than)", 10)
        ),
        
        plotOutput("histCentile", height = 200),
        plotOutput("scattertreatmentIncome", height = 350), 
#       #img(src="TCA_Logo_K (2).png", width = 150))
       div(style = "margin: 0 auto;text-align: center;", 
          HTML("</br> 
              <a href=http://www.thirdcoastanalytics.com target=_blank>
              <img style='width: 0px;' src='http://i.imgur.com/ZZkas87.png'/> </a>"))
#        http://imgur.com/kBhR1Q8
#        http://i.imgur.com/TfGZPss.png
#[Imgur](http://i.imgur.com/ZZkas87.png)
       #tags$a(href="www.rstudio.com", "Click here!")
      ),
      
      tags$div(id="cite",
        HTML('Contact <a href="http://www.wdatagis.com/" target="_blank" >WilliamsOJO </a> (williamojo@gmail.com) with questions,speical thanks to Rich.Majerus.This application was built from code developed by the <a href="https://github.com/rstudio/shiny-examples" target="_blank" >RStudio Team</a>. Data Source: Ministry of Health. This visualization presents Healthcare availability data “as is." '
      ))
    )
  ),


  tabPanel("Data explorer",
   fluidRow(
#     column(3,
#       selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), 
#                                               "cross_river"="cross_river"), multiple=TRUE)
#      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    #fluidRow(
    #  column(5,
    #    numericInput("minScore", "Min HospitalExpenses", min=0, max=50000, value=0)
    #  ),
    #  column(5,
    #    numericInput("maxScore", "Max HospitalExpenses", min=0, max=50000, value=50000)
    #  )
    #),
    hr(),
    dataTableOutput("ziptable")
  ),
  
  conditionalPanel("false", icon("crosshair"))

 # HTML("<i class=fa fa-crosshairs></i>")

))
