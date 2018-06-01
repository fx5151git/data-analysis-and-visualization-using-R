#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("/Users/apple/Desktop/code/final")
load("gowork.Rda")
load("office.Rda")
load("residential.Rda")
load("animation_map1.Rda")
load("order1111test.Rda")
load("ChengduMap.Rda")


load("animation_map13.Rda")
load("panda.Rda")
load("relax.Rda")
load("bar.Rda")
load("officepoi.Rda")
load("residentialpoi.Rda")

load("lineplot4weekday.Rda")
load("lineplot4weekend.Rda")
load("order1112test.Rda")



library(shinydashboard)
require(pacman)
p_load(data.table,tidyverse,dplyr,tidyr,plotly,RColorBrewer,geosphere,rgdal,ggplot2,ggmap,maptools,leaflet,sp,lubridate)

header <- dashboardHeader(
  title = "The Story of Chengdu: Car Sharing Data Exploration",titleWidth=600
)

siderbar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Chengdu Life Note",tabName = "dashboard",icon =icon("dashboard"),
             menuSubItem("Start Working", tabName = "dashboard2",icon =icon("dashboard")),
             menuSubItem("Working Traffic", tabName = "dashboard3",icon =icon("dashboard")),
             menuSubItem("Bird's Eye View", tabName = "dashboard5",icon =icon("dashboard")),
             menuSubItem("At Closer Range", tabName = "dashboard6",icon =icon("dashboard")),
             menuSubItem("Working Places", tabName = "dashboard4",icon =icon("dashboard")),
             menuSubItem("Go Shopping", tabName = "dashboard7",icon =icon("dashboard")),
             menuSubItem("Food & Drink", tabName = "dashboard8",icon =icon("dashboard")),
             menuSubItem("Sleepless City", tabName = "dashboard9",icon =icon("dashboard")),
             menuSubItem("Famous Attractions", tabName = "dashboard10",icon =icon("dashboard"))
    )
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard2",
                     box(width = NULL,
                         title = "POI: Office Building & Residential Area",
                         solidHeader = TRUE, status = "primary",
                         fluidRow(
                           column(width = 6,
                                  leafletOutput("office")),
                           column(width = 6,
                                  leafletOutput("residential"))
                           )),
                     box(width = NULL,
                         title = "When most people go to work? Orders from Residential Area(Pickup) to Office Building(Dropoff)",
                         solidHeader = TRUE, status = "primary",
                         plotlyOutput("density_1"))),
    tabItem(tabName = "dashboard3",
            box(title = "Car Sharing Traffic in Morning Rush Hour (7am-9am): Vehicle Flow Heatmap Animation",
                width = NULL,solidHeader = TRUE, status = "primary",height = 800,
              fluidRow(
                column(width=8,offset=3,
                     imageOutput("roaddensity"))
                )
              )
            
    ),
    tabItem(tabName = "dashboard4",
            fluidRow(
             column(width = 6,
                     box(width = NULL,
                         title = "Popular Drop-off Area at Weekday 9am",
                         solidHeader = TRUE, status = "primary",
                         plotOutput("statdensity2D9"))),
             column(width = 6,
                    box(width = NULL,
                        title = "Contour Extraction & Office Markers",
                        solidHeader = TRUE, status = "primary",
                        leafletOutput("office2")))
                         
                     )),
    tabItem(tabName = "dashboard5",
            fluidRow(
              column(width = 7,
                     box(width = NULL,
                         title="Popular Pick-up/Drop-off Places: Filled Contour Plot on Map",
                         fluidRow(
                           splitLayout(cellWidths = c("50%", "50%"), 
                                       plotOutput("statdensity2D"),
                                       plotOutput("statdensity2D2")
                           )),
                         fluidRow(
                           splitLayout(cellWidths = c("50%", "50%"), 
                                       plotOutput("statdensity2Dweekend"),
                                       plotOutput("statdensity2D2weekend")
                           )))
                    
              ),
              column(width = 5, 
                     box(width = NULL,height=100, status = "warning",
                         selectInput("hour","select the hour you want to visualiza",
                                     choices = seq(from=0,to=23,by=1)
                         ),
                         selected = "8"),
                     box(width = NULL,
                         plotlyOutput("lineplot"))
                     )
            )
    ),
    tabItem(tabName = "dashboard6",
            fluidRow(
              column(width = 10,
                     box(width = NULL,
                         title = "Deep Analysis on Popular Places: High Level Contour Extraction",
                         fluidRow(
                           splitLayout(cellWidths = c("50%", "50%"), 
                                       leafletOutput("leafletstatdensity2D"),
                                       leafletOutput("leafletstatdensity2D2")
                           )))),
              column(width = 2,
                     box(width = NULL,height=400, status = "warning",
                         selectInput("hour1","select the hour you want to visualiza",
                                     choices = seq(from=0,to=23,by=1)
                         ),
                         selected = "8")
              )
            )
            ),
    tabItem(tabName = "dashboard7",
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = "Popular Drop-off Area at Weekend 13pm",
                         solidHeader = TRUE, status = "primary",
                         plotOutput("statdensity2D2weekend13")
                         
                     )),
              column(width = 6,
                     box(width = NULL,
                         title = "Contour Extraction & Commercial Center",
                         solidHeader = TRUE, status = "primary",
                         leafletOutput("shopping"))))
    ),
    tabItem(tabName = "dashboard8",
            br(),
            br(),
            fluidRow(
            column(width=6,
                   box(width = NULL,
                               title = "When Go Relaxation: Hotpot vs. Teahouse",
                               solidHeader = TRUE, status = "primary",
                               plotlyOutput("food"))),
            column(width=6,
                   box(width = NULL,
                       imageOutput("foodimage"))))
            
            
    ),
    tabItem(tabName = "dashboard9",
            br(),
            br(),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = "Night life: Orders Pickup/Dropoff at Pubs",
                         solidHeader = TRUE, status = "primary",
                         plotlyOutput("bar")
                         
                     )),
              column(width = 6,
                     box(width = NULL,
                         
                         imageOutput("bar2"))))),
    tabItem(tabName = "dashboard10",
            br(),
            br(),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = "Do You Love Panda?",
                         solidHeader = TRUE, status = "primary",
                         plotlyOutput("panda")
                         
                     )),
              column(width = 6,
                     box(width = NULL,
                         imageOutput("pandaimage")))))
              

              )
)

ui<-dashboardPage(
  header,
  siderbar,
  body 
)

server <- function(input, output) {
  output$office <- renderLeaflet({
    leaflet() %>%setView(lng=104.066261,lat=30.663007,zoom=13)%>%
      addTiles("http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga")%>%
      addMarkers(
        data=officepoi,lng=officepoi$lon, lat=officepoi$lat,clusterOptions = markerClusterOptions())
  })
  
  output$office2 <- renderLeaflet({
    basestat<-ChengduMap+stat_density2d(aes(x = elon, y = elat, fill = ..level.., alpha = ..level..),
                                        bins = 6, data = order1111test[order1111test$ehour == 9,], geom = "polygon")+
      scale_fill_gradient(low = "black", high = "red")+
      scale_alpha_continuous(range = c(0.1,0.3) )+
      guides(fill=guide_legend(title="drop off 9am",reverse = TRUE)) +
      guides(alpha=FALSE) 
    
    #see the attributes of each color layer and different panel.
    ggplot_build(basestat)
    hi<-ggplot_build(basestat)$data[[4]]
    
    kk<-hi%>%filter(fill=="#C81708")
    
    kk12<-kk%>%filter(group=="-1-012")
    Sr12 = Polygon(cbind(kk12$x,kk12$y))
    srs12 = Polygons(list(Sr12), "s12")
    
    SpP = SpatialPolygons(list(srs12))
    
    leaflet() %>%setView(lng= 104.076835,lat=30.661435,zoom=15)%>% #density center 2
      addTiles("http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga")%>%
      addMarkers(data=officepoi,lng=officepoi$lon, lat=officepoi$lat,
                 clusterOptions = markerClusterOptions(zoom = 14))%>%
      addPolygons(data=SpP, color = "red")
    #add density polygon
  })
  
  output$residential <- renderLeaflet({
    leaflet() %>%setView(lng=104.066261,lat=30.663007,zoom=13)%>%
      addTiles("http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga")%>%
      addMarkers(
        data=residentialpoi,lng=residentialpoi$lon, lat=residentialpoi$lat,clusterOptions = markerClusterOptions())
  })
  
  output$roaddensity <- renderImage({
    list(src = "image/roaddensity.gif",
         contentType = 'image/gif')
         # alt = "This is alternate text"

  }, deleteFile = FALSE)
  
  
  
  output$density_1 <- renderPlotly({
    dens <- with(gowork, tapply(hour, INDEX = type, density))
    df <- data.frame(
      hour = unlist(lapply(dens, "[[", "x")),
      density = unlist(lapply(dens, "[[", "y")),
      cut = rep(names(dens), each = length(dens[[1]]$x))
    )
    
    plot_ly(df, x = ~hour, y = ~density, color = ~cut) %>%
      add_lines()%>%
      layout(xaxis = list(zeroline = FALSE))
  })
  
  
  output$statdensity2D9 <- renderPlot({
    
    animation_map1
  })
  
  
  output$statdensity2D <- renderPlot({
    hour <- input$hour
    animation_map <- function(hour){
      ChengduMap + stat_density2d(
        aes(x = slon, y = slat, fill = ..level.., alpha = ..level..),
        size = 2, bins = 10, data = order1111test[order1111test$shour == hour,],
        geom = "polygon")+
        scale_fill_gradient(low = "black", high = "blue")+
        scale_alpha_continuous(range = c(0.1,0.3) )+
        guides(alpha=FALSE)+
      ggtitle("Pick-up Places on Weekdays")
    }
    animation_map(hour)
  })
  
  output$statdensity2D2 <- renderPlot({
  
    hour <- input$hour
    animation_map <- function(hour){
      ChengduMap + stat_density2d(
        aes(x = elon, y = elat, fill = ..level.., alpha = ..level..),
        size = 2, bins = 10, data = order1111test[order1111test$ehour == hour,],
        geom = "polygon")+
        scale_fill_gradient(low = "black", high = "red")+
        scale_alpha_continuous(range = c(0.1,0.3) )+
        guides(alpha=FALSE)+
      ggtitle("Drop-off Places on Weekdays")
    }
    animation_map(hour)
  })
  
  
  


  output$statdensity2Dweekend <- renderPlot({
    ChengduMap<-get_map(location=c(104.064924,30.665384),zoom=13,source = "google", 
                        maptype = "roadmap",col="bw",language="en-US")%>%ggmap()
    hour <- input$hour
    animation_map <- function(hour){
      ChengduMap + stat_density2d(
        aes(x = slon, y = slat, fill = ..level.., alpha = ..level..),
        size = 2, bins = 10, data = order1112test[order1112test$shour == hour,],
        geom = "polygon")+
        scale_fill_gradient(low = "black", high = "blue")+
        scale_alpha_continuous(range = c(0.1,0.3) )+
        guides(alpha=FALSE)+
        ggtitle("Pick-up Places on Weekends)")
    }
    animation_map(hour)
  })
  
  
  output$statdensity2D2weekend <- renderPlot({
    ChengduMap<-get_map(location=c(104.064924,30.665384),zoom=13,source = "google", 
                        maptype = "roadmap",col="bw",language="en-US")%>%ggmap()
    hour <- input$hour
    animation_map <- function(hour){
      ChengduMap + stat_density2d(
        aes(x = elon, y = elat, fill = ..level.., alpha = ..level..),
        size = 2, bins = 10, data = order1112test[order1112test$ehour == hour,],
        geom = "polygon")+
        scale_fill_gradient(low = "black", high = "red")+
        scale_alpha_continuous(range = c(0.1,0.3) )+
        guides(alpha=FALSE)+
        ggtitle("Drop-off Places on Weekends")
    }
    animation_map(hour)
  })
  
  output$lineplot <- renderPlotly(
    {
      
      
      
      line_plot <- plot_ly(lineplotp4weekday, x = ~hour, y = ~pick_upn,  type = 'scatter', name = 'Weekday Pickup',mode = 'lines',
                           line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
        add_trace(y = ~drop_offn, name = 'Weekday Dropoff', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
        add_trace(data =lineplotp4weekend, y = ~pick_upn, name = 'Weekend Pickup', line = list(color = 'rgb(22, 96, 167)', width = 4,dash = 'dash')) %>%
        add_trace(y = ~drop_offn, name = 'Weekend Dropoff', line = list(color = 'rgb(205, 12, 24)',width = 4,dash = 'dash'))%>%
        
        layout(shapes=list(type='line', x0 = input$hour, x1=input$hour, y0=0, y1=14000, line=list(dash='dot', width=1,color="red")),
               title = "Car Sharing Order Amount during Day",
               xaxis = list(title = "hour in day"),
               yaxis = list (title = "The amount of orders"),
               legend = list(x = 0.5, y = 0.1)
        ) 
      
      
      
      line_plot
    }
  )
  
  
  output$leafletstatdensity2D <- renderLeaflet({

    
    miao<-ChengduMap+stat_density2d(aes(x = slon, y = slat, fill = ..level.., alpha = ..level..),
                                    bins = 6, data = order1111test[order1111test$shour == input$hour1,], geom = "polygon")+
      scale_fill_gradient(low = "black", high = "red")+
      scale_alpha_continuous(range = c(0.1,0.3) )+
      guides(fill=guide_legend(title="pick up 9am",reverse = TRUE)) +
      guides(alpha=FALSE) 
    
    #see the attributes of each color layer and different panel.
    ggplot_build(miao)
    hi<-ggplot_build(miao)$data[[4]]
    
    kk<-hi%>%filter(fill=="#C81708")
    
    kk1<-kk%>%filter(group=="-1-010")
    Sr1 = Polygon(cbind(kk1$x,kk1$y))
    srs1 = Polygons(list(Sr1), "s1")
    
    kk2<-kk%>%filter(group=="-1-011")
    Sr2 = Polygon(cbind(kk2$x,kk2$y))
    srs2 = Polygons(list(Sr2), "s2")
    
    kk3<-kk%>%filter(group=="-1-013")
    Sr3 = Polygon(cbind(kk3$x,kk3$y))
    srs3 = Polygons(list(Sr3), "s3")
    
    kk4<-kk%>%filter(group=="-1-014")
    Sr4 = Polygon(cbind(kk4$x,kk4$y))
    srs4 = Polygons(list(Sr4), "s4")
    
    kk5<-kk%>%filter(group=="-1-015")
    Sr5 = Polygon(cbind(kk5$x,kk5$y))
    srs5 = Polygons(list(Sr5), "s5")
    
    kk6<-kk%>%filter(group=="-1-016")
    Sr6 = Polygon(cbind(kk6$x,kk6$y))
    srs6 = Polygons(list(Sr6), "s6")
    
    kk7<-kk%>%filter(group=="-1-017")
    Sr7 = Polygon(cbind(kk7$x,kk7$y))
    srs7 = Polygons(list(Sr7), "s7")
    
    kk8<-kk%>%filter(group=="-1-009")
    Sr8 = Polygon(cbind(kk8$x,kk8$y))
    srs8 = Polygons(list(Sr8), "s8")
    
    kk9<-kk%>%filter(group=="-1-008")
    Sr9 = Polygon(cbind(kk9$x,kk9$y))
    srs9 = Polygons(list(Sr9), "s9")
    
    kk10<-kk%>%filter(group=="-1-012")
    Sr10 = Polygon(cbind(kk10$x,kk10$y))
    srs10 = Polygons(list(Sr10), "s10")
    
    kk11<-kk%>%filter(group=="-1-007")
    Sr11 = Polygon(cbind(kk11$x,kk11$y))
    srs11 = Polygons(list(Sr11), "s11")
    SpP = SpatialPolygons(list(srs1,srs2,srs3,srs4,srs5,srs6,srs7,srs8,srs9,srs10,srs11))
    
    
    leaflet() %>%setView(lng=104.066261,lat=30.663007,zoom=12)%>% 
      addTiles("http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga")%>%
      addPolygons(data = SpP)%>%
      addCircleMarkers(data=officepoi,lng=officepoi$lon, lat=officepoi$lat,  radius = 8,
                       stroke = TRUE,  weight = 2, opacity = 0.8, color="aqua",
                       fillOpacity = 0.5, label = "Office Buliding",
                       clusterOptions = markerClusterOptions())%>%
      addCircleMarkers(data=residentialpoi,lng=residentialpoi$lon, lat=residentialpoi$lat,  radius = 8,
                       stroke = TRUE,  weight = 2, opacity = 0.8, color="pink",
                       fillOpacity = 0.5, label = "Residential",
                       clusterOptions = markerClusterOptions()) %>%
      addLegend("topright", 
                colors = "blue",labels = "popular places",
                title = "pickup exploration",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1
      )
  
    
  })
  
  output$leafletstatdensity2D2 <- renderLeaflet({
    
    basestat<-ChengduMap+stat_density2d(aes(x = elon, y = elat, fill = ..level.., alpha = ..level..),
                                        bins = 6, data = order1111test[order1111test$ehour == input$hour1,], geom = "polygon")+
      scale_fill_gradient(low = "black", high = "red")+
      scale_alpha_continuous(range = c(0.1,0.3) )+
      guides(fill=guide_legend(title="drop off 9am",reverse = TRUE)) +
      guides(alpha=FALSE) 
    
    #see the attributes of each color layer and different panel.
    ggplot_build(basestat)
    hi<-ggplot_build(basestat)$data[[4]]
    
    kk<-hi%>%filter(fill=="#C81708")
    
    kk1<-kk%>%filter(group=="-1-010")
    Sr1 = Polygon(cbind(kk1$x,kk1$y))
    srs1 = Polygons(list(Sr1), "s1")
    
    kk2<-kk%>%filter(group=="-1-011")
    Sr2 = Polygon(cbind(kk2$x,kk2$y))
    srs2 = Polygons(list(Sr2), "s2")
    
    kk3<-kk%>%filter(group=="-1-013")
    Sr3 = Polygon(cbind(kk3$x,kk3$y))
    srs3 = Polygons(list(Sr3), "s3")
    
    kk4<-kk%>%filter(group=="-1-014")
    Sr4 = Polygon(cbind(kk4$x,kk4$y))
    srs4 = Polygons(list(Sr4), "s4")
    
    kk5<-kk%>%filter(group=="-1-015")
    Sr5 = Polygon(cbind(kk5$x,kk5$y))
    srs5 = Polygons(list(Sr5), "s5")
    
    kk6<-kk%>%filter(group=="-1-016")
    Sr6 = Polygon(cbind(kk6$x,kk6$y))
    srs6 = Polygons(list(Sr6), "s6")
    
    kk7<-kk%>%filter(group=="-1-017")
    Sr7 = Polygon(cbind(kk7$x,kk7$y))
    srs7 = Polygons(list(Sr7), "s7")
    
    kk8<-kk%>%filter(group=="-1-009")
    Sr8 = Polygon(cbind(kk8$x,kk8$y))
    srs8 = Polygons(list(Sr8), "s8")
    
    kk9<-kk%>%filter(group=="-1-008")
    Sr9 = Polygon(cbind(kk9$x,kk9$y))
    srs9 = Polygons(list(Sr9), "s9")
    
    kk10<-kk%>%filter(group=="-1-012")
    Sr10 = Polygon(cbind(kk10$x,kk10$y))
    srs10 = Polygons(list(Sr10), "s10")
    
    kk11<-kk%>%filter(group=="-1-007")
    Sr11 = Polygon(cbind(kk11$x,kk11$y))
    srs11 = Polygons(list(Sr11), "s11")
    SpP = SpatialPolygons(list(srs1,srs2,srs3,srs4,srs5,srs6,srs7,srs8,srs9,srs10,srs11))
    
    
    leaflet() %>%setView(lng=104.066261,lat=30.663007,zoom=12)%>%
      addTiles("http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga")%>%
      addPolygons(data = SpP,color="red")%>%
      addCircleMarkers(data=officepoi,lng=officepoi$lon, lat=officepoi$lat,  radius = 8,
                       stroke = TRUE,  weight = 2, opacity = 0.8, color="aqua",
                       fillOpacity = 0.5, label = "Office Buliding",
                       clusterOptions = markerClusterOptions())%>%
      addCircleMarkers(data=residentialpoi,lng=residentialpoi$lon, lat=residentialpoi$lat,  radius = 8,
                       stroke = TRUE,  weight = 2, opacity = 0.8, color="pink",
                       fillOpacity = 0.5, label = "Residential",
                       clusterOptions = markerClusterOptions()) %>%
      addLegend("topright", 
                colors = "red",labels = "popular places",
                title = "dropoff exploration",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1
      )
    
    
  })
  
  output$statdensity2D2weekend13 <- renderPlot({
    animation_map13
  })
  
  output$shopping <- renderLeaflet({
    
    shoparea<-ChengduMap+stat_density2d(aes(x = elon, y = elat, fill = ..level.., alpha = ..level..),
                                        bins = 6, data = order1111test[order1111test$ehour == 13,], geom = "polygon")+
      scale_fill_gradient(low = "black", high = "red")+
      scale_alpha_continuous(range = c(0.1,0.3) )+
      guides(fill=guide_legend(title="13pm",reverse = TRUE)) +
      guides(alpha=FALSE) 
    ggplot_build(shoparea)
    hi<-ggplot_build(shoparea)$data[[4]]
    
    kk<-hi%>%filter(fill=="#C81708")
    
   
    kk9<-kk%>%filter(group=="-1-009")
    Sr9 = Polygon(cbind(kk9$x,kk9$y))
    srs9 = Polygons(list(Sr9), "s9")
    
    SpP = SpatialPolygons(list(srs9))
    
    
    
    yanshikou<-data.frame(lat=c(30.658634,30.658707,30.656788,30.652339,30.655819),
                          lon=c(104.070883, 104.077117, 104.081451, 104.078340, 104.070980))
    yanshikou <- Polygon(cbind(yanshikou$lon,yanshikou$lat))
    yanshikou <- Polygons(list(yanshikou), "yanshikou")%>%list()%>%SpatialPolygons()
    
    leaflet() %>%setView(lng= 104.076140 ,lat=30.656234,zoom=15)%>% #density center 1
      addTiles("http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga")%>%
      addPolygons(data = yanshikou,color="green",weight=4,label="Yanshikou-ChunxiRd Commercial Area")%>%
      addPolygons(data = SpP,color="red")
  })
  
  output$food <- renderPlotly({
    dens <- with(relax, tapply(ehour, INDEX = place, density))
    df <- data.frame(
      hour = unlist(lapply(dens, "[[", "x")),
      density = unlist(lapply(dens, "[[", "y")),
      cut = rep(names(dens), each = length(dens[[1]]$x))
    )
    
    plot_ly(df, x = ~hour, y = ~density, color = ~cut) %>%
      add_lines()%>%
      layout(xaxis = list(zeroline = FALSE))
  })
  
  output$foodimage <- renderImage({
    return(list(
      src = "image/hotpottea.jpeg",
      contentType = "image/jpeg",
      height = 400,
      width = 480,
      alt = "https://image.baidu.com/search/detail?ct=503316480&z=0&ipn=d&word=%E6%88%90%E9%83%BD%E7%81%AB%E9%94%85&step_word=&hs=0&pn=1&spn=0&di=121591548430&pi=0&rn=1&tn=baiduimagedetail&is=0%2C0&istype=2&ie=utf-8&oe=utf-8&in=&cl=2&lm=-1&st=-1&cs=113418275%2C479393800&os=3467713101%2C186216198&simid=4188363231%2C740408216&adpicid=0&lpn=0&ln=1983&fr=&fmq=1516788481946_R&fm=result&ic=0&s=undefined&se=&sme=&tab=0&width=&height=&face=undefined&ist=&jit=&cg=&bdtype=0&oriquery=&objurl=http%3A%2F%2Fwww.028tty.com%2Fuploads%2Fmain%2Flitimg%2F20140407%2F20140407221224.jpg&fromurl=ippr_z2C%24qAzdH3FAzdH3Fooo_z%26e3Badbppy_z%26e3Bv54AzdH3FstgjfAzdH3Ffi5o_ln_z%26e3Bip4s&gsm=0&rpstart=0&rpnum=0"
    ))
  }, deleteFile = FALSE)

  output$bar <- renderPlotly(
    {
      
      dens <- with(bar, tapply(hour, INDEX = type, density))
      df <- data.frame(
        hour = unlist(lapply(dens, "[[", "x")),
        density = unlist(lapply(dens, "[[", "y")),
        cut = rep(names(dens), each = length(dens[[1]]$x))
      )
      
      plot_ly(df, x = ~hour, y = ~density, color = ~cut) %>%
        add_lines()%>%
        layout(xaxis = list(zeroline = FALSE))
    }
  )
  
  output$bar2 <- renderImage(
    {
      return(list(
        src = "image/bar.jpg",
        contentType = "image/jpeg",
        height = 400,
        width = 480,
        alt = "https://image.baidu.com/search/detail?ct=503316480&z=0&ipn=d&word=%E6%88%90%E9%83%BD%E7%81%AB%E9%94%85&step_word=&hs=0&pn=1&spn=0&di=121591548430&pi=0&rn=1&tn=baiduimagedetail&is=0%2C0&istype=2&ie=utf-8&oe=utf-8&in=&cl=2&lm=-1&st=-1&cs=113418275%2C479393800&os=3467713101%2C186216198&simid=4188363231%2C740408216&adpicid=0&lpn=0&ln=1983&fr=&fmq=1516788481946_R&fm=result&ic=0&s=undefined&se=&sme=&tab=0&width=&height=&face=undefined&ist=&jit=&cg=&bdtype=0&oriquery=&objurl=http%3A%2F%2Fwww.028tty.com%2Fuploads%2Fmain%2Flitimg%2F20140407%2F20140407221224.jpg&fromurl=ippr_z2C%24qAzdH3FAzdH3Fooo_z%26e3Badbppy_z%26e3Bv54AzdH3FstgjfAzdH3Ffi5o_ln_z%26e3Bip4s&gsm=0&rpstart=0&rpnum=0"
      ))
    }, deleteFile = FALSE)
  
  output$panda <- renderPlotly({
    dens <- with(panda, tapply(hour, INDEX = type, density))
    df <- data.frame(
      hour = unlist(lapply(dens, "[[", "x")),
      density = unlist(lapply(dens, "[[", "y")),
      cut = rep(names(dens), each = length(dens[[1]]$x))
    )
    
    plot_ly(df, x = ~hour, y = ~density, color = ~cut) %>%
      add_lines()%>%
    layout(xaxis = list(zeroline = FALSE))
    #%>%add_trace(x1=)
  })
  
  output$pandaimage <- renderImage(
    {
      return(list(
        src = "image/panda.jpg",
        contentType = "image/jpeg",
        height = 380,
        width = 480,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
  
}  


# Run the application 
shinyApp(ui = ui, server = server)

