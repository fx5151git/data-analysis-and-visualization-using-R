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
                            value = as.POSIXct(1477958400, origin="1970-01-01"), step = 600, timeFormat = "%F %T"
                )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$map <-renderLeaflet({
    #load("FHV_DATA_NOVEMBER_2016.Rda")
    
    #calculated for every input
    daystart <- as.numeric(input$range)
    temp <- FHV_DATA %>% filter((daystart %% 86400)+1 & Pickup_date >= daystart & Pickup_date < daystart+600)
    
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
    cars <- iconList(
      uber = makeIcon("car-black-side-silhouette.png", "car-black-side-silhouette.png", 24, 24),
      other = makeIcon("car-green-side-silhouette.png", "car-black-side-silhouette.png", 24, 24)
    )
    men = makeIcon("men-silhouette.png", "men-silhouette.png", 24, 24)
    map <- leaflet(data=temp) %>% addTiles() %>% addGeoJSON(geojson) %>%
      addMarkers(temp$lon, temp$lat,icon=~cars[dba_category],label=as.character(temp$base_name), clusterOptions = markerClusterOptions())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)