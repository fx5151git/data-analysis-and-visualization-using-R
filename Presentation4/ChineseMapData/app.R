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
                sliderInput("range", "Time interval (3 minutes skip)",as.POSIXct(1478016000, origin="1970-01-01"), as.POSIXct(1478102400, origin="1970-01-01"),
                            value = as.POSIXct(1478016000, origin="1970-01-01"), step = 180, timeFormat = "%F %T")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  #chinacabdata <- fread("order_20161102.csv")
  output$map <-renderLeaflet({
    #load("CAB_DATA_NOVEMBER_2016_CLEANED.Rda")
    #daystart <- 1478016000
    #calculated for every input
    summary(chinacabdata)
    daystart <- as.numeric(input$range)
    temp <- chinacabdata %>% filter(V2 >= daystart & V2 < daystart+180)
    #delete unique routes

    

    #id needed for the coloring of the lines
    temp$ID <-  as.numeric(row.names(temp))
    #draw the lines as Spartial Lines     
    geo_lines <- gcIntermediate(
      temp %>%
        select(V4, V5),
      temp %>%
        select(V6, V7),
      sp = TRUE, # SpatialLines are what Leaflet wants
      addStartEnd = TRUE, # By default this is FALSE, and would be inaccurate
      n = 1000 #number of intermediate points
    )
    
    #initialize color for the different paths (each shared path is unique)
    colorr = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    pal <- colorFactor(palette = colorr, domain = temp$ID)
    
    temp1 <- temp[, c(1,6:8)]
    names(temp1)[names(temp1) == "V6"] <- "lon"
    names(temp1)[names(temp1) == "V7"] <- "lat"
    temp2 <- temp[, c(1,4:5,8)]
    names(temp2)[names(temp2) == "V4"] <- "lon"
    names(temp2)[names(temp2) == "V5"] <- "lat"
    
    taxi = makeIcon("taxi.png", "taxi.png", 24, 24)
    flag = makeIcon("flag.png", "flag.png", 24, 24)
    
    leaflet() %>%
      addTiles()  %>% addMarkers(data = temp1, icon = flag) %>%
      addMarkers(data = temp2, icon = taxi, popup = ~paste("Passengers last 3 minutes waiting for a cab: "), clusterOptions = markerClusterOptions()) %>%
      addPolylines(data = geo_lines, color = pal(temp$ID), opacity = 0.7)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

