#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pacman)
library(data.table)
library(leaflet)
p_load(tidyverse, stringr, rvest)
library(geosphere)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <-  bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
   # Sidebar with a slider input for number of bins 
  absolutePanel(top = 10, right = 10,
        sliderInput("range", "Time interval (3 minutes skip)",as.POSIXct(1477958400, origin="1970-01-01", tz="Europe/Berlin"), as.POSIXct(1480550400, origin="1970-01-01"),
                    value = as.POSIXct(1477958400, origin="1970-01-01", tz="CET"), step = 180, timeFormat = "%F %T")
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$map <-renderLeaflet({
    #load("DAY_TAXI_DATA_NOVEMBER_2016_CLEANED.Rda")
    #calculated for every input
    daystart <- as.numeric(input$range)
    temp <- DAY_TAXI_DATA %>% filter((daystart %% 86400)+1 & tpep_pickup_datetime >= daystart & tpep_pickup_datetime < daystart+180)
    
    #delete unique routes
    temp <- temp[duplicated(temp[c("DOlat", "DOlon", "PUlat","PUlon")]),]
    
    #aggregate routes that can be shared
    temp <- aggregate(passenger_count ~ DOlat + DOlon + PUlat + PUlon , temp , sum)
    
    #take only the fares that have more than 3 people waiting for a cab (for the sake of visualisation and profit)
    temp <- temp %>% filter( passenger_count > 3)
    #id needed for the coloring of the lines
    temp$ID <-  as.numeric(row.names(temp))
    #draw the lines as Spartial Lines     
    geo_lines <- gcIntermediate(
      temp %>%
        select(PUlon, PUlat),
      temp %>%
        select(DOlon, DOlat),
      sp = TRUE, # SpatialLines are what Leaflet wants
      addStartEnd = TRUE, # By default this is FALSE, and would be inaccurate
      n = 1000 #number of intermediate points
    )
    
    #initialize color for the different paths (each shared path is unique)
    colorr = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    pal <- colorFactor(palette = colorr, domain = temp$ID)
    
    temp1 <- temp[, c(1:2,5)]
    names(temp1)[names(temp1) == "DOlon"] <- "lon"
    names(temp1)[names(temp1) == "DOlat"] <- "lat"
    temp2 <- temp[, c(3:5)]
    names(temp2)[names(temp2) == "PUlon"] <- "lon"
    names(temp2)[names(temp2) == "PUlat"] <- "lat"
    
    taxi = makeIcon("taxi.png", "taxi.png", 24, 24)
    flag = makeIcon("flag.png", "flag.png", 24, 24)
    
    leaflet() %>%
      addTiles()  %>% addMarkers(data = temp1, icon = flag) %>%
      addMarkers(data = temp2, icon = taxi, popup = ~paste("Passengers last 3 minutes waiting for a cab: ", temp2$passenger_count), clusterOptions = markerClusterOptions()) %>%
      addPolylines(data = geo_lines, color = pal(temp$ID), opacity = 0.7)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

