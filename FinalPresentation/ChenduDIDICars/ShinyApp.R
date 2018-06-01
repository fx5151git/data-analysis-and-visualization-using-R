#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(sp)
require(rgdal)
library(pacman)
library(data.table)
library(leaflet)
p_load(tidyverse, stringr, rvest)
library(geosphere)
library(RColorBrewer)
library(maptools)
library(googleway)
options(digits=10)
library(plotly)
#load("graph_data.Rda")
#load("shared_trips_1121.Rda")
#load("gps1121.Rda")
#load("acummulated_trips.Rda")


#all the function needed

coords_to_path1 <- function(slon, slat, elon, elat){
  
  ## using a valid Google Maps API key
  key <- "AIzaSyD91EtcUDC8qGRwuY_xOBsEllrnJdQ4u34"
  
  ## Using the first and last coordinates as the origin/destination
  origin <- c(slat,slon)
  destination <- c(elat,elon)
  
  ## get the directions from Google Maps API
  res <- google_directions(origin = origin,
                           destination = destination,
                           key = key)  ## include simplify = F to return data as JSON
  
  result <- decode_pl(res$routes$overview_polyline$points)
  result <- result[,c("lon", "lat")]
  return(result)
}

get_shared_trips <- function(temp, cuttree){
  sp.start <- SpatialPointsDataFrame(temp[, c("slon", "slat")], temp, 
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  sp.end <- SpatialPointsDataFrame(temp[, c("elon", "elat")], temp, 
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  mdist <- distm(sp.start)
  hc <- hclust(as.dist(mdist), method="complete")
  sp.start$clust <- cutree(hc, h=cuttree)
  mdist <- distm(sp.end)
  hc <- hclust(as.dist(mdist), method="complete")
  sp.end$clust <- cutree(hc, h=cuttree)
  
  temp$startclusters <- sp.start$clust
  temp$endclusters <- sp.end$clust
  
  temp$ID <-  1
  temp <- temp %>% group_by(startclusters, endclusters) %>% filter(ID!=sum(ID)) %>% select(-ID) %>% ungroup()
  
  temp$group_id <- temp %>% group_indices(startclusters, endclusters) 
  temp <- temp %>% select(-startclusters, -endclusters)
  
  return (temp)
}

shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }  

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

#changes lon/lat coordinates to sp lines path
coords_to_path <- function(slon, slat, elon, elat){
  
  ## using a valid Google Maps API key
  key <- "AIzaSyAZ2Dk74vpR8mVpw0S-WfB_x7-hMDPdiVw"
  key1<- "AIzaSyCLah2X5XiqHdGdKzV_ErwEkLDWgjR-WHs"
  
  ## Using the first and last coordinates as the origin/destination
  origin <- c(slat,slon)
  destination <- c(elat,elon)
  
  ## get the directions from Google Maps API
  res <- google_directions(origin = origin,
                           destination = destination,
                           key = key1)  ## include simplify = F to return data as JSON
  
  result <- decode_pl(res$routes$overview_polyline$points)
  result <- result[,c("lon", "lat")]
  return(result)
}


# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Car sharing", id="nav",
                         tabPanel("Taxi Sharing Benefits",
                                  fluidRow(
                                    br(),
                                    column(width=4,
                                           plotlyOutput("fuel")
                                           
                                    ),
                                    column(width = 4,
                                           plotlyOutput("money")
                                    ),
                                    column(width = 4,
                                           plotlyOutput("co2")
                                    ),
                                    column(width = 4,
                                           plotlyOutput("distance")
                                    ), 
                                    column(width = 4,
                                           plotlyOutput("similartrips")
                                    ),
                                    column(width = 4,
                                           plotlyOutput("similartripsall")
                                    )
                                  )
                         ),
                         tabPanel("GPS Data Analysis",
                                  div(class="outer",
                                      
                                      #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                      tags$head(
                                        includeCSS("styles.css")
                                        ),
                                      
                        
                                      leafletOutput("comparedgoogleroutine", width = "100%", height = "100%"),
                                       absolutePanel(id="controls", class = "panel panel-default", fixed = TRUE,
                                                     draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
                                                     width = 500, height = "auto",
                                               
                                             h2("Data Exploration") ,        
                                                           
                                             br(),    
                                             sliderInput("slider_time", label=NULL,
                                                         min = as.POSIXct(1479682800, origin="1970-01-01", tz="CST6CDT"),
                                                         max =  as.POSIXct(1479682800+86400, origin="1970-01-01",tz="CST6CDT"),
                                                         value = as.POSIXct(1479682800, origin="1970-01-01", tz="CST6CDT"),
                                                         step = 1800,
                                                         timeFormat = "%T"),
                                             h4(textOutput("output_slider_time")),
                                             br(),
                                             plotOutput("speed",height = 300)
                                             )
                                             
                                      
                         ),
                         
                         tags$div(id="cite",
                                  'data comes from',tags$em('DIDI'))),
                         
                         tabPanel("What if users used busses?",
                                  div(class="outer",
                                      #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                      tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                      
                                      leafletOutput("busleaflet", width = "100%", height = "100%"),
                                      absolutePanel(id="controls", class = "panel panel-default", fixed = TRUE,
                                                    draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
                                                    width = 400, height = "auto",
                                                    br(),
                                                    selectInput("day", "Day of November 2016", choices = c(19:23), selected = 19),
                                                    selectInput("trips", "Group routes that have more than", choices = c(3:6), selected = 4),
                                                    numericInput("meters", "Radious of grouping: ", min = 1, max = 2000, value = 500)
                                                    ))
        
                                  )
                        
)                 
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  
  
  shared_trips_1121 <- shared_trips_1121 %>% filter(slon <= 104.1296 & elon <= 104.1296 & slon >= 104.0421 & elon >= 104.0421 & slat <= 30.72775 & elat <= 30.72775 & slat >= 30.65294 & elat >= 30.65294)
  
  #deleting other gps data that do not have the start and end in the quartile.
  gps_data_raw <- gps_data_raw[gps_data_raw$orderid %in% shared_trips_1121$orderid,]
 
  output$output_slider_time  <- renderText({
    paste0("Selected Time: ", format(input$slider_time,"%H:%M:%S", tz="Europe/Berlin"))
  })
  
  order_data <- reactive({
    shared_trips_1121 %>% filter(stime >= input$slider_time-25200 & stime < input$slider_time+1800-25200) # 25200 +7h for china time to be dispayed as berlin time
  })
  
  gps_data <- reactive({
    gps_data_raw[gps_data_raw$orderid %in% order_data()$orderid,]
  })
  
  accumulated <- reactive({
    get_shared_trips(acummulated_trips %>% filter(stime >= 1479398400+(5*3600)+(as.numeric(input$day)-18)*86400 & stime < 1479398400+((22*3600)+((as.numeric(input$day)-18)*86400))) %>% filter(similartrips > input$trips), input$meters)
  })
  
  accumulated1 <- reactive({
    acummulated_trips %>% filter(stime >= 1479398400+(5*3600)+(as.numeric(input$day)-18)*86400 & stime < 1479398400+((22*3600)+((as.numeric(input$day)-18)*86400)))
  })
  

  

    
  output$comparedgoogleroutine <- renderLeaflet({
    
    temp_gps_data<- gps_data()
    temp_order_data <- order_data()
    #id needed for the coloring of the lines
    temp_order_data$ID <-  as.numeric(row.names(temp_order_data))
    #initialize color for the different paths (each shared path is unique)
    colorr = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    pal <- colorFactor(palette = colorr, domain = temp_order_data$ID + 10)
    
    temp1 <- temp_order_data[, c(1,5:7)]
    names(temp1)[names(temp1) == "elon"] <- "lon"
    names(temp1)[names(temp1) == "elat"] <- "lat"
    temp2 <- temp_order_data[, c(1,4:5)]
    names(temp2)[names(temp2) == "slon"] <- "lon"
    names(temp2)[names(temp2) == "slat"] <- "lat"
    
    taxi = makeIcon("taxi.png", "taxi.png", 24, 24)
    
    flag = makeIcon("flag.png", "flag.png", 24, 24)
    
    leaflet() %>% addMarkers(data = temp1, icon = flag) %>% 
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
      addMarkers(data = temp2, icon = taxi)
  })


  
   
  data <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$comparedgoogleroutine_marker_click,{
    data$clickedMarker <- input$comparedgoogleroutine_marker_click
    
    output$speed <- renderPlot({

      
      temp_gps_data<- gps_data()
      # #generate gps Lines 
      line <- points_to_line(data = temp_gps_data,long = "lon", lat = "lat", id_field = "orderid", sort_field = "time")
      

      
        # convert data frame into a list where the tripid is unique
        temp_gps_data <- split(temp_gps_data,temp_gps_data[,2],drop=TRUE)

        #for loop to calculate all gps data
        for(i in 1:length(temp_gps_data))  {
          # Shift vectors for lat and lon so that each row also contains the next position.
          temp_gps_data[[i]]$lat.p1 <- shift.vec(temp_gps_data[[i]]$lat, -1)
          temp_gps_data[[i]]$lon.p1 <- shift.vec(temp_gps_data[[i]]$lon, -1)

          # Calculate distances (in metres)
          temp_gps_data[[i]]$dist.to.prev <- apply(temp_gps_data[[i]], 1, FUN = function (row) {
            distGeo(c(as.numeric(row["lon.p1"]), as.numeric(row["lat.p1"])),
                    c(as.numeric(row["lon"]), as.numeric(row["lat"])))
          })
          # Shift the time vector, too.
          temp_gps_data[[i]]$time.p1 <- shift.vec(temp_gps_data[[i]]$time, -1)
          # Calculate the number of seconds between two positions.
          temp_gps_data[[i]]$time.diff.to.prev <- temp_gps_data[[i]]$time.p1 - temp_gps_data[[i]]$time
          # Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
          temp_gps_data[[i]]$speed.m.per.sec <- temp_gps_data[[i]]$dist.to.prev / temp_gps_data[[i]]$time.diff.to.prev
          temp_gps_data[[i]]$speed.km.per.h <- temp_gps_data[[i]]$speed.m.per.sec * 3.6
          temp_gps_data[[i]]$speed.km.per.h <- ifelse(is.na(temp_gps_data[[i]]$speed.km.per.h), 0, temp_gps_data[[i]]$speed.km.per.h)
          temp_gps_data[[i]]$lowess.speed <- lowess(temp_gps_data[[i]]$speed.km.per.h, f = 0.2)$y
        }
        length(as.numeric(row.names(temp_gps_data[[1]][250])))
        
        temp_order_data<- order_data()
        temp_order_data$ID <-  as.numeric(row.names(temp_order_data))
        id <- temp_order_data %>% filter(slon==data$clickedMarker$lng,slat == data$clickedMarker$lat)
        # #generate google Lines
        google_liness <- SpatialLines(list(Lines(list(Line(coords_to_path(slon = temp_order_data[id$ID,]$slon, slat = temp_order_data[id$ID,]$slat, elon = temp_order_data[id$ID,]$elon, elat = temp_order_data[id$ID,]$elat))), "line1")))
        id <- grep(id$orderid, temp_gps_data)
        proxy <- leafletProxy("comparedgoogleroutine")
        
        proxy %>%
          addPolylines(data = line[id], opacity = 0.7) %>% addPolylines(data = google_liness, opacity = 0.7, color = "red")
        # Plot speeds and smoother
        
        plot(temp_gps_data[[id]]$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "",
             col = "grey40")
        lines(temp_gps_data[[id]]$lowess.speed, col = "blue", lwd = 3)
        legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
               col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
        abline(h = mean(temp_gps_data[[id]]$speed.km.per.h), lty = 2, col = "blue")
        title(main = "Speed  of the line selected")
    })
    })
 
  observeEvent(input$busleaflet_marker_click,{
    data$clickedMarker <- input$busleaflet_marker_click
    colorid <- sample(10:60, 1, replace=F)
    temp <- accumulated() %>% filter(slon==data$clickedMarker$lng,slat == data$clickedMarker$lat)
    if(length(as.numeric(row.names(temp))) > 1){
    sp_lines <- SpatialLines(list(Lines(list(Line(coords_to_path1(slon = temp[1,]$slon, slat = temp[1,]$slat, elon = temp[1,]$elon, elat = temp[1,]$elat))), paste0("line", colorid))))
    for (p in 2:length(as.numeric(row.names(temp)))) {
      id <- paste0("line", colorid+p)
      path <- coords_to_path1(slon = temp[p,]$slon, slat = temp[p,]$slat, elon = temp[p,]$elon, elat = temp[p,]$elat)
      l <- SpatialLines(list(Lines(list(Line(path)), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    } else if(length(as.numeric(row.names(temp))) != 0)sp_lines <- SpatialLines(list(Lines(list(Line(coords_to_path1(slon = temp[1,]$slon, slat = temp[1,]$slat, elon = temp[1,]$elon, elat = temp[1,]$elat))), paste0("line", colorid))))
    #initialize color for the different paths (each shared path is unique)
    colorr = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    colorr = colorr[-grep('white', colorr)]
    pal <- colorFactor(palette = colorr, domain = 1:60)
    proxy <- leafletProxy("busleaflet")
    proxy %>% addPolylines(data = sp_lines, color = pal(colorid), opacity = 1)
  })
      
   output$busleaflet <- renderLeaflet({
     
  
     #variable for the clustering
     temp <- accumulated()
     
     #filter data and extract important info
     temp1 <- temp[,c(8,12)]
     temp1$similartripsperday = 1
     temp1 <- temp1 %>% group_by(group_id) %>% summarise_at(c("similartripsperday", "similartrips"),funs(sum))
     temp <- temp[!duplicated(temp$group_id), ]
     temp <- temp %>% select(-similartrips)
     temp <- merge(temp, temp1, by = "group_id")
     temp <- temp %>% select(-group_id)
     temp$bustrips <- floor((temp$similartripsperday * temp$similartrips)*2/21) #of buss trips approx if each taxi has at most 2 users 
     temp$fuel_saved <- (((temp$distanc/1000) * temp$similartripsperday * temp$similartrips)*6.5/100) - (((temp$distanc/1000) * ((temp$similartripsperday * temp$similartrips)*2/21))*50/100) 
     temp$pollution_reduced <- ((temp$distanc/1000) * temp$similartripsperday * temp$similartrips)*(127-70)
     #data for calculations taken from 
     #https://www.delijn.be/en/overdelijn/organisatie/zorgzaam-ondernemen/milieu/co2-uitstoot-voertuigen.html
     temp$moneysaved <- temp$fuel_saved * 0.93 #data taken from 
     # http://www.globalpetrolprices.com/gasoline_prices/
     
     temp$ID <-  as.numeric(row.names(temp))
     
     temp1 <- temp[, c(1,5:7)]
     names(temp1)[names(temp1) == "elon"] <- "lon"
     names(temp1)[names(temp1) == "elat"] <- "lat"
     temp2 <- temp[, c(1,4:5)]
     names(temp2)[names(temp2) == "slon"] <- "lon"
     names(temp2)[names(temp2) == "slat"] <- "lat"
     
     buss = makeIcon("buss.png", "buss.png", 24, 24)
     
     flag = makeIcon("flag.png", "flag.png", 24, 24)
     
     
     leaflet() %>%setView(lng=104.08278,lat=30.68039,zoom=13) %>% addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>% addMarkers(data = temp1, icon = flag) %>% 
       addMarkers(data = temp2, icon = buss,popup= paste(paste(
         "<strong>", "Using Busses instead of Cabs", "</strong>"), 
         paste("<div>","Nr of busses x day", temp$bustrips, "</div>"), 
         paste("<div>","Money Saved", round(temp$moneysaved, 2), "Euro", "</div>"), 
         paste("<div>","Fuel Saved", round(temp$fuel_saved, 2),  "Liters", "</div>"),    
         paste("<div>","<em>", "Pollution Reduced by", "</em>"),
         paste(round(temp$pollution_reduced, 2), "g CO2", "</div>")))
   }) 
   
   output$fuel <- renderPlotly({
     
     temp <- graph_data
     temp$date <- as.Date(graph_data$hour)
     temp <- subset(temp, date == "2016-11-20")
     sum <- graph_data %>% summarise(sum=sum(fuelsaved))
     plot_ly(data=temp,x=~hour,y=~fuelsaved,type="scatter",mode="line",line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
       layout(title = paste0("Fuelsaved in total(5 days):",round(sum$sum,2), "Liters"),
              xaxis = list(title = "Day"),
              yaxis = list (title = "Fuelsaved"))
   
    
   })
    
  output$money <- renderPlotly({
    
    temp <- graph_data
    temp$date <- as.Date(graph_data$hour)
    temp <- subset(temp, date == "2016-11-21")
    sum <- graph_data %>% summarise(sum=sum(moneysaved))
    plot_ly (data=temp,x=~hour,y=~moneysaved,type="scatter",mode="line",line = list(color = 'rgb(235, 12, 24)', width = 4))%>%
      layout(title = paste0("Moneysaved in total(5 days): ",round(sum$sum,2), " Euro"),
             xaxis = list(title = "Day"),
             yaxis = list (title = "Moneysaved"))
  })
  
  output$co2 <- renderPlotly({
    
    temp <- graph_data
    temp$date <- as.Date(graph_data$hour)
    temp <- subset(temp, date == "2016-11-22")
    sum <- graph_data %>% summarise(sum=sum(co2emmision))
    plot_ly(data=temp,x=~hour,y=~co2emmision/1000,type="scatter",mode="scatter",line = list(color = 'rgb(205, 125, 24)', width = 4))%>%
      layout(title = paste0("Co2 Emission saved in total(5 days): ",round(sum$sum/1000,2), " kg") ,
             xaxis = list(title = "Hour"),
             yaxis = list (title = "Co2 Emission saved"))
    })   
  
  output$distance <- renderPlotly({
    
    temp <- graph_data
    temp$date <- as.Date(graph_data$hour)
    temp <- subset(temp, date == "2016-11-19")
    sum <- temp %>% summarise(sum=sum(similartrips))
    plot_ly(data=temp,x=~hour,y=~floor(similartrips/2),type="scatter",mode="scatter",line = list(color = 'rgb(155, 62, 24)', width = 4))%>%
      layout(title = paste0("Similar Trips Max(day): ",floor(sum$sum/2.5)) ,
             xaxis = list(title = "Hour"),
             yaxis = list (title = "Number of Trips"))
  }) 
  
  output$similartrips <- renderPlotly({
    
    temp <- graph_data
    temp$date <- as.Date(graph_data$hour)
    temp <- subset(temp, date == "2016-11-23")
    sum <- temp %>% summarise(sum=sum(similartrips))
    plot_ly(data=temp,x=~hour,y=~floor(similartrips/2),type="scatter",mode="scatter",line = list(color = 'rgb(105, 12, 24)', width = 4))%>%
      layout(title = paste0("Similar Trips Min(day): ",floor(sum$sum/2.5)) ,
             xaxis = list(title = "Hour"),
             yaxis = list (title = "Number of Trips"))
  }) 
  
  output$similartripsall <- renderPlotly({
    
    
    sum <- graph_data %>% summarise(sum=sum(similartrips))
    
    plot_ly(data=graph_data,x=~hour,y=~floor(similartrips/2.5),type="scatter",mode="scatter",line = list(color = 'rgb(105, 202, 24)', width = 4))%>%
      layout(title = paste0("Similar Trips in the last (5 days): ",floor(sum$sum/2.5)) ,
             xaxis = list(title = "Day"),
             yaxis = list (title = "Number of Trips"))
  }) 
 
})

# Run the application 
shinyApp(ui = ui, server = server)

