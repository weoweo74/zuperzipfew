
## app.R ##
library(dplyr)
library(zipcode)
library(RCurl)
library(zipcode)
library(ggthemes)

x <- getURL("https://raw.githubusercontent.com/weoweo74/zuperzipfew/master/superzipV2.csv")
allzips <- read.csv(text = x)


#allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
#allzips$treatment <- allzips$treatment * 100


allzips$zip <- clean.zipcodes(allzips$HD2013.ZIP.code)


row.names(allzips) <- allzips$unitid
#allzips$state.x <- allzips$state.x
#allzips$state.x <- ifelse(is.na(allzips$state.x) & allzips$city.x=='cross_river', 'cross_river', as.character(allzips$state.x))  # make DC a state so it is not missing from analysis

# transform variables 
allzips$l_enorllment <- log(allzips$DRVEF122013.12.month.full.time.equivalent.enrollment..2012.13 + 1)
allzips$ID <- allzips$zipcode

allzips$city.x <- as.character(allzips$city.x)

cleantable <- allzips %>%
  select(
    hospital = hospital.name, 
    City = city.x,
    State = state.x,
    Zipcode = zip,
    #Rank = rank,
    HospitalExpenses = centile,
    #Superzip = superzip,
    Enrollment = adultpop,
    Selectivity = treatment,
    Patients = income,
    Lat = latitude,
    Long = longitude, 
    ID = zipcode
  )


# handle missing data 
allzips$treatment <- ifelse(is.na(allzips$treatment), 101, allzips$treatment)  # missing admit rates to 101 so they work with adjustable selectivity 
allzips[is.na(allzips)] <- 0  # all other missing values to 0 
allzips <- na.omit(allzips) # rows that are still missing values are dropped 




# temporarily remove treatments with missing data 
# cleantable <- na.omit(cleantable)





#Create the ui functions for the dashboard  
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







# create the server functions for the dashboard  


library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(bitops)
library(RCurl)

x <- getURL("https://raw.githubusercontent.com/weoweo74/zuperzipfew/master/superzipV2few.csv")
allzips <- read.csv(text = x)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

#removed for treatment analysis
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
zipdata <- allzips

# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  map <- createLeafletMap(session, "map")
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(zipdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    
    hist(zipsInBounds()$centile[zipsInBounds()$centile!=0],
         breaks = centileBreaks,
         main = "Hospital monthly expenses (visible zips)",
         xlab = "Hospital monthly expenses",
         xlim = range(allzips$centile, na.rm=T),
         col = '#00DD00',
         border = 'white')
  })
  
  
  
  output$scattertreatmentIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(centile ~ treatment, data = subset(zipsInBounds(), zipsInBounds()$centile!=0 & zipsInBounds()$treatment!=0),
                 xlim = range(allzips$treatment), ylim = range(allzips$centile), 
                 xlab = "Admit Rate", ylab = "HospitalExpenses"))
  })
  
  # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
  # integration; without it, the addCircle commands arrive in the browser
  # before the map is created.
  session$onFlushed(once=TRUE, function() {
    paintObs <- observe({
      colorBy <- input$color
      sizeBy <- input$size
      
      colorData <- if (colorBy == "superzip") {
        as.numeric(allzips$treatment < (input$threshold))
      } else {
        allzips[[colorBy]]
      }
      colors <- brewer.pal(7, "Spectral")[cut(colorData, 7, labels = FALSE)]
      colors <- colors[match(zipdata$zipcode, allzips$zipcode)]
      
      # Clear existing circles before drawing
      map$clearShapes()
      # Draw in batches of 1000; makes the app feel a bit more responsive
      chunksize <- 1000
      for (from in seq.int(1, nrow(zipdata), chunksize)) {
        to <- min(nrow(zipdata), from + chunksize)
        zipchunk <- zipdata[from:to,]
        # Bug in Shiny causes this to error out when user closes browser
        # before we get here
        try(
          map$addCircle(
            zipchunk$latitude, zipchunk$longitude,
            (zipchunk[[sizeBy]] / max(allzips[[sizeBy]])) * 30000,
            zipchunk$zipcode,
            list(stroke=FALSE, fill=TRUE, fillOpacity=0.4),
            list(color = colors[from:to])
          )
        )
      }
    })
    
    # TIL this is necessary in order to prevent the observer from
    # attempting to write to the websocket after the session is gone.
    session$onSessionEnded(paintObs$suspend)
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(df, zipcode, lat, lng) {
    selectedZip <- df[df$zipcode == zipcode,]
    selectedZip$income <- ifelse(selectedZip$income==0, NA, selectedZip$income)
    selectedZip$treatment <- ifelse(selectedZip$treatment==101, NA, selectedZip$treatment)
    selectedZip$adultpop <- ifelse(selectedZip$adultpop==0, NA, selectedZip$adultpop)
    selectedZip$centile <- ifelse(selectedZip$centile==0, NA, selectedZip$centile)
    
    content <- as.character(tagList(
      #tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$h4(selectedZip$hospital.name),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedZip$city.x, selectedZip$state.x
      ))), tags$br(),
      
      
      if(!(is.na(selectedZip$income))){
        sprintf("Patients: %s",   format(as.integer(selectedZip$income), big.mark=",",scientific=F))}, tags$br(),
      
      if(!(is.na(selectedZip$treatment))){
        sprintf("Admit Rate: %s%%", as.integer(selectedZip$treatment))}, tags$br(),
      
      if(!(is.na(selectedZip$adultpop))){
        sprintf("Enrollment: %s", format(as.integer(selectedZip$adultpop), big.mark=",",scientific=F))}, tags$br(),
      
      if(!(is.na(selectedZip$centile))){
        sprintf("Hospital monthly expenses: %s", paste('$', format(as.integer(selectedZip$centile), big.mark=",",scientific=F), sep=''))}
      
    ))
    map$showPopup(lat, lng, content, zipcode)
  }
  
  
  # When map is clicked, show a popup with city info
  clickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(allzips, event$id, event$lat, event$lng)
    })
  })
  
  session$onSessionEnded(clickObs$suspend)
  
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %.%
        `$`('City') %.%
        unique() %.%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %.%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %.%
        `$`('Zipcode') %.%
        unique() %.%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map$clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      #showZipcodePopup(allzips, zipcode, allzips$latitude, allzips$longitude)
      map$fitBounds(lat - dist, lng - dist,
                    lat + dist, lng + dist)
    })
  })
  
  output$ziptable <- renderDataTable({
    cleantable %>%
      filter(
        #Score >= input$minScore,
        #Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  }, escape = FALSE)
})










#run/call the shiny app
#shinyApp(ui, server)