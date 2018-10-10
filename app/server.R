
source("../app/global.R")

## Server Part #########################################################################

shinyServer(function(input, output,session) { 
  
  ## Interactive map ###################################################################
  
  output$map <- renderLeaflet({
    
    ## Intermediate Data ##
    if (input$days == "All day"){
      count_intermediate = count_result %>% apply(c(1,2), sum)
      FPD_intermediate = FPD_result %>% apply(c(1,2), mean, na.rm = T)
    }else{
      count_intermediate = count_result[ , , (input$days == "Not Business Day") + 1]
      FPD_intermediate = FPD_result[ , , (input$days == "Not Business Day") + 1]
    }
    if (!input$showhr){
      subdat@data$count = count_intermediate %>% apply(1, sum)
      subdat@data$FPD = FPD_intermediate %>% apply(1, mean, na.rm = T)
    }else{
      subdat@data$count = count_intermediate[, input$hr_adjust+1]
      subdat@data$FPD = FPD_intermediate[, input$hr_adjust+1]
    }
    
    ######
    
    blocks_coord = data.frame(center_lng = rep(NA, 195), center_lat = rep(NA, 195)) # Combine borough coord for future marking purpose
    for (i in 1:195){ blocks_coord[i,] = subdat@polygons[[i]]@labpt }    # One more update: add long/lat permanently into myShape@data as

    subdat_top5_intermediate = cbind(subdat@data, blocks_coord)

     if (input$boroSelect == "All"){ # filter borough
       subdat_top5 = subdat_top5_intermediate
     } else {
       subdat_top5 = subdat_top5_intermediate[subdat@data$BoroName == input$boroSelect, ]
     }
    #if (!input$showbr){
    #  subdat_top5 = subdat_top5_intermediate
    #}

    subdat_top5 = subdat_top5 %>% 
      subset(select = c("NTACode", "NTAName", "count", "FPD", "center_lng", "center_lat"))

    top5count = subdat_top5[order(subdat_top5$count, decreasing = T),
                            c("NTAName", "count", "center_lng", "center_lat")] %>% head(5) # fetch top-5-rows with the most counts/FPD
    top5FPD = subdat_top5[order(subdat_top5$FPD, decreasing = T),
                          c("NTAName", "FPD", "center_lng", "center_lat")] %>% head(5)
    
    ######
    
    subdat_data=subdat@data[,c("NTACode", "NTAName", "count", "FPD")]
    subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
    
    # print leaflet
    pal = colorBin(color[[1]], bins = bin[[1]])
    pal_FPD = colorBin(color[[2]], bins = bin[[2]])
    pal2 = colorBin(c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"), 1:10)
    pal3 = colorBin(c("#005A32", "#74C476", "#F7FCF5"), 0:0.125:1)
    
    ######
    
    popup1 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Count of pick-ups: </strong><br>', subdat_data$count)
    popup2 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Fair Per Distance: </strong><br>', subdat_data$FPD)
    popup3 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName)
    popup4 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Percentage Paying Cash: </strong><br>', payper$PercentagePaying)
    
    ######
    
    greenLeafIcon <- makeIcon(
      iconUrl = "https://cdn1.iconfinder.com/data/icons/weather-19/32/fire-512.png",
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 0, iconAnchorY = 0
      # shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      # shadowWidth = 50, shadowHeight = 64,
      # shadowAnchorX = 4, shadowAnchorY = 62
    )
    redLeafIcon <- makeIcon(
      iconUrl = "https://maxcdn.icons8.com/Share/icon/Finance//usd1600.png",
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 0, iconAnchorY = 0
      # shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      # shadowWidth = 50, shadowHeight = 64,
      # shadowAnchorX = 4, shadowAnchorY = 62
    )
    
    ######
    
    pic1<-leaflet(subdat) %>%
      clearMarkers() %>% clearShapes() %>% clearWebGLHeatmap() %>%
      setView(lat=40.7128, lng=-74.0059, zoom=10) %>%
      ## Add Locate Me botton ##
      addEasyButton(easyButton(
        position = "bottomleft",
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true});alert('Locate your position?');map.setZoom(17); }"))) %>%
      ## Add Measure botton ##
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479")%>%
      addProviderTiles('CartoDB.Positron') 

    if (input$CF == "count"){
      pic1<-pic1 %>%
        addPolygons(fillColor = ~pal(count), color = 'grey', weight = 1, 
                    popup = popup1, fillOpacity = .6, group = group1) %>%
        addLegend(position = "bottomright",
                  colors = color[[1]],
                  labels = label[[1]],
                  opacity = 0.6,
                  title = title[[1]])
    }
    else if (input$CF == "FPD"){
      pic1<-pic1 %>%
        addPolygons(fillColor = ~pal_FPD(FPD), color = 'grey', weight = 1, 
                    popup = popup2, fillOpacity = .6, group = group2) %>%
        addLegend(position = 'bottomright',
                  colors = color[[2]],
                  labels = label[[2]], ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = title[[2]])
    }
    
    else if(input$CF == "cluster1"){
      pic1<-pic1 %>%
      addPolygons(fillColor = ~pal2(count_result1$fit.cluster), color = 'grey', weight = 1, popup = popup3, fillOpacity = .6)

    }
    
    else if (input$CF == "cash"){
      pic1<-pic1 %>%
        addPolygons(fillColor =  ~pal3(payper$PercentagePayingCash), color = 'grey', weight = 1, 
                    popup = popup4, fillOpacity = .6, group = group3) %>%
        addLegend(position = 'bottomright',
                  colors = color[[3]],
                  labels = label[[3]], ## legend labels (only min and max)
                  opacity = 0.6,      ## transparency again
                  title = title[[3]])
    }

    else if (input$CF == "Prob"){
        
    }
    
    else if (input$CF == "Heatmap"){
      
    }
  
    ## TOP5 ##    
    if (input$top15count == TRUE){
    pic1<-pic1 %>%
    addMarkers(~top5count$center_lng, ~top5count$center_lat, icon = greenLeafIcon)
    }

    else{
      pic1
    }
    
    if (input$top15FPD == TRUE){
      pic1<-pic1 %>%
        addMarkers(~top5FPD$center_lng, ~top5FPD$center_lat, icon = redLeafIcon)
    }

    else{
      pic1
    }

    ## subway analysis ##
    if (input$subway == 1){
      pic1
    }
    else if (input$subway == 2) {
      pic1<-pic1 %>% addMarkers(data = subway1, ~Latitude, ~Longitude, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))  
    }
    else if (input$subway == 3){
    pic1<-pic1 %>% addMarkers(data = subway2, ~L1, ~L2, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))  
      
    }
    

  })
  
    ## Probability Heat Map ##
    observeEvent(input$map_click, {
    click <- input$map_click
    
    output$pos <- renderText({"Coordinates of Click"})
    output$lng <- renderText({paste0(" Longitude: ", click$lng)})
    output$lat <- renderText({paste0(" Latitude: " , click$lat)})
    
    
    if(input$type=="Heatmap"&input$CF=="Prob"){  
      map_new <- leafletProxy("map") %>%
        clearShapes() %>% clearMarkers() %>% 
        addMarkers(lng = click$lng, lat = click$lat) %>% 
        addCircles(lng = click$lng, lat = click$lat, radius = input$radius_pos, 
                   fillOpacity = 0.2, stroke = FALSE)
      
      
      dat_drop_inside <- dynamicdata[abs(dropoff_longitude - click$lng)*111320 <  input$radius_pos & 
                                       abs(dropoff_latitude - click$lat)*111190 < input$radius_pos]
      dat_pick_inside <- dynamicdata[abs(pickup_longitude - click$lng)*111320 <  input$radius_pos & 
                                       abs(pickup_latitude - click$lat)*111190 < input$radius_pos]
      if(nrow(dat_drop_inside) > 0 & input$pd == "Drop off"){
      
        map_new <- map_new %>% clearWebGLHeatmap() %>%
          addWebGLHeatmap(lng=dat_drop_inside$dropoff_longitude, lat = dat_drop_inside$dropoff_latitude,opacity = 0.7, size = 30)
      }
      if(nrow(dat_pick_inside) > 0 & input$pd == "Pick up"){
        
        map_new <- map_new %>% clearWebGLHeatmap() %>%
          addWebGLHeatmap(lng=dat_pick_inside$pickup_longitude, lat = dat_pick_inside$pickup_latitude, opacity = 0.7, size=30, alphaRange = 0.5)
      }
      
      map_new
    }
    })
  
  ## Recommand Locations ##
  getdistance <- function(destination, ori){
    lon1 = ori[2]*pi/180
    lat1 = ori[1]*pi/180
    lon2 = destination[2]*pi/180
    lat2 = destination[1]*pi/180
    deltaLat = lat2 - lat1
    deltaLon = lon2 - lon1
    a = sin(deltaLat/2)^2 + cos(lat1) * cos(lat2) * sin(deltaLon/2)^2
    c = 2 * asin(sqrt(a))
    EARTH_RADIUS = 6371
    return (c * EARTH_RADIUS * 1000)
    }
    
    observe({
      origin <- c(40.756197, -73.97644)
      event <- input$map_click
      #if(is.null(event)){
      #  warn <- "Please select a valid postion"
      #  leafletProxy("map") %>%
      #    addPopups(lat = origin[1], lng = origin[2], popup = warn, 
      #              options = popupOptions(closeButton = TRUE))
      #  return()
      #}
      #else{
        origin <- c(event$lat, event$lng)
      #}
      radius <- input$radius_pos
      Time <- input$time
      if(input$type=="Points"&input$CF=="Prob"){
      if(input$pd2 == "Pick up") {
        if(input$hours != 0){
          dat <- filter(dynamicdata, pickup_hour <= input$hours + 1 & pickup_hour >= input$hours - 1)
          dat <- dat[,c(2,1)] #may change dataset
        }
        else{
          dat <- filter(dynamicdata, pickup_hour == 23 | pickup_hour <= 1)
          dat <- dat[,c(2,1)] #may change dataset
        }
      }
      else{
        if(input$hours != 0){
          dat <- filter(dynamicdata, dropoff_hour <= input$hours + 1 & dropoff_hour >= input$hours - 1)
          dat <- dat[,c(6,5)] #may change dataset
        }
        else{
          dat <- filter(dynamicdata, dropoff_hour == 23 | dropoff_hour <= 1)
          dat <- dat[,c(6,5)] #may change dataset
        }
      }
      
      dis <- function(destination){
        return(getdistance(destination, origin))
      }
      
      
      in_radius.log <- apply(dat, MARGIN = 1, FUN = dis) <= radius
      in_radius.dat <- dat[in_radius.log, ]
      
      colnames(in_radius.dat) <- c("latitude","longitude")
      
      if(nrow(in_radius.dat) >= 5){
        cluster <- kmeans(in_radius.dat, 5)
        out <- data.frame(cluster$centers)
      }
      else{
        warn <- "Please select a valid postion(not efficient data here)"
        leafletProxy("map") %>%
          clearPopups() %>%
          addPopups(lat = origin[1], lng = origin[2], popup = warn, 
                    options = popupOptions(closeButton = TRUE))
        out <- data.frame(in_radius.dat)
      }
      
      leafletProxy("map", data = out) %>%
        clearMarkers() %>% clearShapes() %>% clearWebGLHeatmap() %>%
        addMarkers(~longitude, ~latitude) %>%
        addCircleMarkers(lng = origin[2], lat = origin[1], radius = 5, color = "red") %>%
        clearShapes() %>%
        addCircles(lng = origin[2], lat = origin[1], radius = input$radius_pos)
      }
    })

    ## Direction Heatmap ##
    observe({
      amount2 <- reactive({amount2 <- input$Amount2})
      pickuphour <- reactive({pickuphour <- input$pickuphour})
      #amount2 <- inout$Amount2
      #pickuphour <- input$pickuphour
      if(input$CF=="Heatmap") {
        map_new2<-leafletProxy("map")
      if(input$direction=='south'){
        directiondata<-filter(dynamicdata,dynamicdata$dropoff_latitude<dynamicdata$pickup_latitude, dynamicdata$pickup_hour==pickuphour())
        directiondata<-directiondata[sample(1:dim(directiondata)[1],amount2(),replace = T),]
        #output$map <- renderLeaflet({
        #map_new2<-leaflet(directiondata[sample(1:dim(directiondata)[1],amount2(),replace = T),]) %>%
        #    clearMarkers() %>% clearShapes() %>% clearWebGLHeatmap() %>%
        #    setView(lat=40.7128, lng=-74.0059, zoom=11) %>%
            #map_new2<-leafletProxy("map")%>%
        #    addProviderTiles(providers$CartoDB.DarkMatter) %>%
        #    addWebGLHeatmap(lng=~pickup_longitude, lat=~pickup_latitude, size = 300, unit="m",alphaRange = 0.5)
        #})
      }
      if(input$direction=='north'){
        directiondata<-filter(dynamicdata,dynamicdata$dropoff_latitude>dynamicdata$pickup_latitude, dynamicdata$pickup_hour==pickuphour())
        directiondata<-directiondata[sample(1:dim(directiondata)[1],amount2(),replace = T),]
      }
      #output$map <- renderLeaflet({
      map_new2<-map_new2%>%
        clearMarkers() %>% clearShapes() %>% clearWebGLHeatmap() %>%
        setView(lat=40.7128, lng=-74.0059, zoom=11) %>%
        #map_new2<-leafletProxy("map")%>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        addWebGLHeatmap(lng=directiondata$pickup_longitude, lat=directiondata$pickup_latitude, size = 300, unit="m",alphaRange = 0.5)
        #})
    map_new2
      }
    })
  
  ## Dynamic Map ################################################################

  observe({
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    dattest = data.frame(Longitude = event$lng, Latitude = event$lat)
    coordinates(dattest) <- ~ Longitude + Latitude
    proj4string(dattest) <- CRS("+proj=longlat")
    dattest <- spTransform(dattest, proj4string(myShape1))
    rtest = over(dattest, myShape1)
    
    output$districttimeplot <- renderPlot({
      if (nrow(rtest) == 0) {
        return(NULL)
      }
      if (input$days == "All Day"){
        count_resultNTA = count_result[which(rownames(count_result) == rtest$NTACode),,]
        count_resultNTA = apply(count_resultNTA, 1, sum)
        index <- c(0:23)
        dfcount_resultNTA <- data.frame(index, count_resultNTA)
        ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
          labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
      }
      else if (input$days == "Business Day"){
        count_resultNTA = count_result[which(rownames(count_result) == rtest$NTACode),,1]
        index <- c(0:23)
        dfcount_resultNTA <- data.frame(index, count_resultNTA)
        ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
          labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
      }
      else if (input$days == "Not Business Day") {
        count_resultNTA = count_result[which(rownames(count_result) == rtest$NTACode),,2]
        index <- c(0:23)
        dfcount_resultNTA <- data.frame(index, count_resultNTA)
        ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
          labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
      }
      
    })
  })
    
    
    ######
    
    output$map2 <- renderLeaflet({
      leaflet() %>%
        setView(lat=40.7128, lng=-74.0059, zoom=11) %>%
      # Base groups
      addTiles(group = "default") %>%
        addProviderTiles(providers$Stamen.Toner, group = "blackwhite") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "grey")%>%
      # Layers control
      addLayersControl(
        baseGroups = c("default", "blackwhite", "grey"),
        overlayGroups = c("points", "region"),
        options = layersControlOptions(collapsed = FALSE)
      )
    })
    
    drawvalue <- reactive({
      if (input$pd == 'pick up'){
        t <- filter(dynamicdata, pickup_hour == input$hours, pickup_date == "1/1/2015")
        return(t)
      }
      else{
        t <- filter(dynamicdata, dropoff_hour == input$hours, dropoff_date == "1/1/2015")
        return(t)
      }
    })
  
    observe({
      
      radius <-  100
      if (input$pd == 'Pick up')  {
        t <- filter(dynamicdata, pickup_hour == input$hours, pickup_date == input$`choose date`)
        longitudepmax <- max(t$pickup_longitude)
        latitudepmax <- max(t$pickup_latitude)
        longitudepmin <- min(t$pickup_longitude)
        latitudepmin <- min(t$pickup_latitude)
        leafletProxy("map2", data = t) %>%
          clearShapes() %>%  
          addCircles(~pickup_longitude, ~pickup_latitude,radius = radius, 
                     stroke=FALSE, fillOpacity=0.8,fillColor = "green",
                     popup = as.character(paste0('<strong>longitude </strong>&nbsp&nbsp',
                                                 '<strong>latitude </strong><br>',
                                                 round(t$pickup_longitude,5),
                                                 '&nbsp&nbsp&nbsp',
                                                 round(t$pickup_latitude,5))),
                     group='points') %>%
          addRectangles(
            lng1=longitudepmax, lat1=latitudepmax,
            lng2=longitudepmin, lat2=latitudepmin,
            fillColor = "green",group='region')
        }
      else if (input$pd == 'Drop off')  {
        t <- filter(dynamicdata, dropoff_hour == input$hours, dropoff_date == input$`choose date`)
        longitudedmax <- max(t$dropoff_longitude)
        latitudedmax <- max(t$dropoff_latitude)
        longitudedmin <- min(t$dropoff_longitude)
        latitudedmin <- min(t$dropoff_latitude)
        leafletProxy("map2", data = t) %>%
          clearShapes() %>%
          addCircles(~dropoff_longitude, ~dropoff_latitude, radius=radius,
                     stroke=FALSE, fillOpacity=0.8,fillColor = "red",
                     popup = as.character(paste0('<strong>longitude </strong>&nbsp&nbsp',
                                                 '<strong>latitude </strong><br>',
                                                 round(t$dropoff_longitude,5),
                                                 '&nbsp&nbsp&nbsp',
                                                 round(t$dropoff_latitude,5))),
                     group = 'points') %>%
          addRectangles(
            lng1=longitudedmax, lat1=latitudedmax,
            lng2=longitudedmin, lat2=latitudedmin,
            fillColor = "red",
            color = "red",group = 'region'
          ) 
      }
      else if (input$pd == "All"){
        t <- filter(dynamicdata, dropoff_hour == input$hours | pickup_hour == input$hours, 
                    dropoff_date == input$`choose date` | pickup_date == input$`choose date`)
        longitudepmax <- max(t$pickup_longitude)
        latitudepmax <- max(t$pickup_latitude)
        longitudepmin <- min(t$pickup_longitude)
        latitudepmin <- min(t$pickup_latitude)
        longitudedmax <- max(t$dropoff_longitude)
        latitudedmax <- max(t$dropoff_latitude)
        longitudedmin <- min(t$dropoff_longitude)
        latitudedmin <- min(t$dropoff_latitude)
        
        leafletProxy("map2", data = t) %>%
          clearShapes() %>%
          addCircles(~dropoff_longitude, ~dropoff_latitude, radius=radius,
                     stroke=FALSE, fillOpacity=0.8,fillColor = "red",
                     popup = as.character(paste0('<strong>longitude </strong>&nbsp&nbsp',
                                                 '<strong>latitude </strong><br>',
                                                 round(t$dropoff_longitude,5),
                                                 '&nbsp&nbsp&nbsp',
                                                 round(t$dropoff_latitude,5))),
                     group='points') %>%
          addCircles(~pickup_longitude, ~pickup_latitude, radius=radius,
                     stroke=FALSE, fillOpacity=0.8,fillColor = "green",
                     popup = as.character(paste0('<strong>longitude </strong>&nbsp&nbsp',
                                                 '<strong>latitude </strong><br>',
                                                 round(t$pickup_longitude,5),
                                                 '&nbsp&nbsp&nbsp',
                                                 round(t$pickup_latitude,5))),
                     group = 'points') %>%
          addRectangles(
            lng1=longitudepmax, lat1=latitudepmax,
            lng2=longitudepmin, lat2=latitudepmin,
            fillColor = "green",group='region'
          ) %>%
          addRectangles(
            lng1=longitudedmax, lat1=latitudedmax,
            lng2=longitudedmin, lat2=latitudedmin,
            fillColor = "red",
            color = "red",group='region'
          )
      }
      
    })
    
  ## Data Explorer ################################################################

  ## Histogram ##
    
  observe({
    type2 <- reactive({type2 <- input$type2})
    if(input$type2=='pickup'){
      output$payment_type <- renderPlotly({
        plot_ly(x = dataa$pickup_hour,type = 'histogram')
      })
    } 
    if(input$type2=='dropoff'){
      output$payment_type<-renderPlotly({
        plot_ly(x = dataa$dropoff_hour,type = 'histogram')
      })
    }
  })
  
  dataa$time<-ifelse(dataa$pickup_hour>=6&dataa$pickup_hour<=18,'day','night')
    output$plot2 <- renderPlotly({
      plot_ly(data = filter(dataa,dataa$payment_type==ifelse(input$method=='cash',1,0)),
              x = ~trip_distance,y = ~fare_amount,color = ~time)})
    
    
    output$rawtable <- DT::renderDataTable({
      DT::datatable(dataa)
    })
    output$dynamicdata <- DT::renderDataTable({
      DT::datatable(dynamicdata)
    })
})



