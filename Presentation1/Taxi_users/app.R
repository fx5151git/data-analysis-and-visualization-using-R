#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
library(pacman)
library(ggmap)
library(data.table)
library(jsonlite)
library(leaflet)

p_load(tidyverse, stringr, rvest)
# Define UI for application that draws a histogram
ui <-  bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Dates",as.POSIXct(1477958400, origin="1970-01-01"), as.POSIXct(1480550399, origin="1970-01-01"),
                            value = as.POSIXct(1477958400, origin="1970-01-01"), step = 3600, timeFormat = "%F %T"
                )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$map <-renderLeaflet({
    #load("CAB_DATA_NOVEMBER_2016.Rda")
    
    #calculated for every input
    daystart <- as.numeric(input$range)
    temp <- DAY_TAXI_DATA %>% filter((daystart %% 86400)+1 & tpep_pickup_datetime >= daystart & tpep_pickup_datetime < daystart+3600 & PUlon == DOlon & PUlat == DOlat)
    temp$ID <-  as.numeric(row.names(temp))
    temp1 <- temp[, c(1:4,7:10)]
    names(temp1)[names(temp1) == "DOlon"] <- "lon"
    names(temp1)[names(temp1) == "DOlat"] <- "lat"
    temp1$Destination <- "Drop Out"
    temp2 <- temp[, c(1:6,9:10)]
    names(temp2)[names(temp2) == "PUlon"] <- "lon"
    names(temp2)[names(temp2) == "PUlat"] <- "lat"
    temp2$Destination <- "Pick Up"
    temp <- bind_rows(temp1, temp2)
    rm(temp1,temp2)
    names(temp)[names(temp) == "ID"] <- "fare_id"
    temp$id <-  as.numeric(row.names(temp))
    temp <- temp %>% filter(Destination == "Pick Up")
    
    geojson <- readLines("community_districts.geojson", warn = FALSE) %>%
      paste(collapse = "\n") %>%
      fromJSON(simplifyVector = FALSE)
    
    # Default styles for all features
    geojson$style = list(
      weight = 1,
      color = "#691A99",
      opacity = 1,
      fillOpacity = 0.2
    )
    men = makeIcon("men-silhouette.png", "men-silhouette.png", 24, 24)
    map <- leaflet(data=temp) %>% addTiles() %>% addGeoJSON(geojson) %>%
      addMarkers(temp$lon, temp$lat,icon=men,label=as.character(temp$passenger_count), clusterOptions = markerClusterOptions())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

