
#Server
server = function (input, output, session){
  
  #-----------------------------------------------
  # Overview Tab
  #-----------------------------------------------
  
  
  
  
  
  #-----------------------------------------------
  # Dashboard Tab
  #-----------------------------------------------
  
  # Crime Frequency Tab
  #-----------------------------------------------
  
  #Value boxes
  output$totalcrimes <- renderValueBox({
    
    crime.total <- crime[crime$Month %in% input$Months] %>% 
      group_by(`Primary Type`) %>% 
      summarise(counts = n())
    
    valueBox(value = sum(crime.total$counts),
             subtitle = "Total Number of Crimes",
             icon = icon("exclamation-circle"),
             color = "orange")
  })
  
  
  
  
  output$montharrest <- renderValueBox({
    
    crime.arrest <- crime[crime$Month %in% input$Months] %>% 
      group_by(Arrest) %>% 
      summarise(counts = n()) %>%
      mutate(per = round(counts/ sum(counts)*100, 2))
    
    valueBox(value = paste0(as.character(crime.arrest$per[crime.arrest$Arrest == "TRUE"]),
                            "%"),
             subtitle = "Arrests were made",
             icon = icon("user-lock"),
             color = "orange")
  })
  
  # Bar graph for crime type                     
  output$BG_month.crimetype  <- renderPlot({
    
    crime.month <- crime[crime$Month %in% input$Months]    
    
    crime.month.df <- crime.month %>% group_by(`Primary Type`) %>% summarise(counts = n())
    
    ggplot(data = crime.month.df, aes(x= reorder(`Primary Type`, -counts), y=counts, fill = counts)) +
      geom_col() +
      scale_fill_gradient2(low = "tan1", mid = "orange", high = "red2")+
      labs(y = "Number of Crimes",
           x = "Crime type") +
      geom_text(aes(label = counts), size = 4, color = "black",
                position = position_dodge(width = 0.9), vjust = -0.5) +
      ylim(c(0,max(crime.month.df$counts)+500))+
      theme(legend.position = "none",
            axis.title = element_text(face="bold", size = 16),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 9.5, angle = 90, vjust = 0.5, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) 
  })
  
  
  
  
  
  # Locations Tab
  #-----------------------------------------------
  
  # location map of crime
  output$totalcrimesbydate <- renderValueBox({
    
    crime.total <- crime[as.Date(crime$Date) >= input$date[1] &
                           as.Date(crime$Date) <= input$date[2] &
                           crime$`Crime Category` %in% input$crime.type] %>%
      group_by(`Crime Category`) %>%
      summarise(counts = n())
    
    valueBox(value = sum(crime.total$counts),
             subtitle = "Total Number of Crimes",
             icon = icon("exclamation-circle"),
             color = "orange")
    
  })
  
  output$crimemap <- renderLeaflet({
    
    crime$crime.labels <- paste0('Date:   ',crime$Date,'<br>',
                                 'Crime Type:   ',crime$`Primary Type`,'<br>',
                                 'Crime Category:',crime$`Crime Category`,'<br>',
                                 'Ward:   ',crime$Ward) %>% 
      lapply(htmltools::HTML)
    
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>% 
      setView( lng = -87.6, lat = 41.83, zoom = 10.8 ) %>%
      addCircles(data = crime[as.Date(crime$Date) >= input$date[1] &
                                as.Date(crime$Date) <= input$date[2] &
                                crime$`Crime Category` %in% input$crime.type ],
                 radius = 30,
                 lat = crime$Latitude[as.Date(crime$Date) >= input$date[1] & 
                                        as.Date(crime$Date) <= input$date[2] &
                                        crime$`Crime Category` %in% input$crime.type],
                 lng = crime$Longitude[as.Date(crime$Date) >= input$date[1] & 
                                         as.Date(crime$Date) <= input$date[2] &
                                         crime$`Crime Category` %in% input$crime.type],
                 fillColor = "#FFAD00",
                 fillOpacity = 0.5,
                 color = "#FFAD00",
                 weight = 2,
                 stroke = T,
                 highlightOptions = highlightOptions(color = "red",
                                                     opacity = 1.0,
                                                     weight = 2,
                                                     fillOpacity = 1,
                                                     bringToFront = TRUE),
                 label = ~crime.labels
                 
      ) #End Add Circles
    
  })
  
  
  
  
  # Heat map  Tab
  #-----------------------------------------------
  
  # Heat map of crime type to hour of day
  output$heatmap <- renderPlotly({
    
    heatmap.df <- crime %>% 
      group_by(Hour,`Primary Type`) %>% 
      summarise(counts = n()) %>% 
      spread(`Primary Type`, value = counts) %>%
      remove_rownames %>% 
      column_to_rownames(var = "Hour")
    
    heatmap.df[is.na(heatmap.df)] <- 0
    heatmap.matrix <- as.matrix(heatmap.df)
    
    
    plot_ly(x = colnames(heatmap.matrix), y = rownames(heatmap.matrix), 
            z = heatmap.matrix, type = "heatmap",
            showlegend = FALSE,
            colors = colorRamp(c('tan1', 'red', "red4"))) %>%
      layout(margin = list(l=120),
             xaxis = list(title = "<b>Crime Type</b>", tickfont = list(size = 10)),
             yaxis = list(title = "<b>Time of the Day</b>",range = c(0, 23), 
                          tickfont = list(size = 10)),
             rangebreaks = list(dticks = 1)) %>%
      hide_colorbar()%>% 
      add_trace(text = heatmap.matrix[],
                hovertemplate = paste(" Hour of Day: %{y:.0f}<br>",
                                      "Crime Type: %{x}<br>",
                                      "Crimes: %{text}</br>"))
  })   
  
  
  
  
  # Cluster map of crime type to hour of day
  output$percent.crime <- renderValueBox({
    
    crime.per.total <- crime[crime$`Primary Type` %in% input$crime.type.time &
                               crime$Hour >= input$TOD[1] & crime$Hour <= input$TOD[2] &
                               crime$Weekday %in% input$week] %>%
      group_by(Hour) %>%
      summarise(per.counts = n())
    
    valueBox(value = paste0((round((sum(crime.per.total$per.counts)/nrow(crime)*100),2)),"%"),
             subtitle = "Percentage of total recorded crimes",
             icon = icon("chart-pie"),
             color = "orange")
    
  })
  
  output$heatmap.time <- renderLeaflet({
    
    crime$crime.labels.time <- paste0('Hour of the day:   ',crime$Hour,'<br>',
                                      'Crime Type:   ',crime$`Primary Type`,'<br>',
                                      'Crime Category:',crime$`Crime Category`,'<br>',
                                      'Weekday:   ',crime$Weekday) %>% 
      lapply(htmltools::HTML)
    
    
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>% 
      setView( lng = -87.6, lat = 41.83, zoom = 10.8) %>%
      addCircleMarkers(data = crime[crime$`Primary Type` %in% input$crime.type.time &
                                      crime$Hour >= input$TOD[1] & crime$Hour <= input$TOD[2] &
                                      crime$Weekday %in% input$week ],
                       color = "#FFAD00",
                       weight = 2,
                       opacity = 0.8,
                       fill = TRUE,
                       fillOpacity = 0.8,
                       radius = 5,
                       clusterOptions = markerClusterOptions(),
                       label = ~crime.labels.time
                       
      ) #End Add Circles Markers
    
  })
  
  
  
  
  # Crime Caution Tab
  #-----------------------------------------------
  # value box
  output$high.caution.com <- renderValueBox({
    
    shp.filter1 <- shp.freq.community %>% 
      filter(`Primary Type` %in% input$crime.caution.type) %>%
      filter(Hour >= input$crime.caution.TOD[1] &  Hour <= input$crime.caution.TOD[2]) %>%
      filter(Weekday.end %in% input$crime.caution.week)
    
    
    if(nrow(shp.filter1)>0)
    {
      shp.filter <- shp.filter1 %>%
        group_by(COMMUNITY) %>%
        summarise(shp.freq = n())
      shp.df <- merge(area.shp, shp.filter, by = "COMMUNITY", all.x = TRUE)
      shp.df$shp.freq[is.na(shp.df$shp.freq)] <- 0
      shp.df$`CAUTION LEVEL`[shp.df$shp.freq <= quantile(shp.df$shp.freq, 0.3)] <- "Low"
      shp.df$`CAUTION LEVEL`[shp.df$shp.freq > quantile(shp.df$shp.freq, 0.3) &
                               shp.df$shp.freq <= quantile(shp.df$shp.freq, 0.6)] <- "Medium"
      shp.df$`CAUTION LEVEL`[shp.df$shp.freq > quantile(shp.df$shp.freq, 0.6)] <- "High"
      
      number.high <- ifelse(nrow(shp.df[shp.df$`CAUTION LEVEL` == "High",])>0,nrow(shp.df[shp.df$`CAUTION LEVEL` == "High",]),0)       
      
      valueBox(value = number.high,
               subtitle = "Number of High Caution Communities",
               icon = icon("slack-hash"),
               color = "orange")
      
    }
    else{
      
      valueBox(value = 0,
               subtitle = "Number of High Caution Communities",
               icon = icon("slack-hash"),
               color = "orange")
      
    }
    
    
  })
  
  # Map
  output$cautionmap <- renderLeaflet({
    
    shp.filter1 <- shp.freq.community %>% 
      filter(`Primary Type` %in% input$crime.caution.type) %>%
      filter(Hour >= input$crime.caution.TOD[1] &  Hour <= input$crime.caution.TOD[2]) %>%
      filter(Weekday.end %in% input$crime.caution.week)
    
    
    if(nrow(shp.filter1)>0)
    {
      shp.filter <- shp.filter1 %>%
        group_by(COMMUNITY) %>%
        summarise(shp.freq = n())
      shp.df <- merge(area.shp, shp.filter, by = "COMMUNITY", all.x = TRUE)
      shp.df$shp.freq[is.na(shp.df$shp.freq)] <- 0
      shp.df$`CAUTION LEVEL`[shp.df$shp.freq <= quantile(shp.df$shp.freq, 0.3)] <- "Low"
      shp.df$`CAUTION LEVEL`[shp.df$shp.freq > quantile(shp.df$shp.freq, 0.3) &
                               shp.df$shp.freq <= quantile(shp.df$shp.freq, 0.6)] <- "Medium"
      shp.df$`CAUTION LEVEL`[shp.df$shp.freq > quantile(shp.df$shp.freq, 0.6)] <- "High"
      nc_pal <- colorFactor(c('orange', 'red', "red4"), 
                            domain = shp.df$`CAUTION LEVEL`, 
                            levels = c("Low","Medium","High"))
    }
    else{
      shp.df <- area.shp
      shp.df$`CAUTION LEVEL` <- "No Records Selected"
      nc_pal <- colorFactor(c('grey'), domain = shp.df$`CAUTION LEVEL`, levels = c("No Records Selected"))
      
    }
    
    leaflet(shp.df) %>%
      addProviderTiles("CartoDB.DarkMatter") %>% 
      setView( lng = -87.6, lat = 41.83, zoom = 10.48) %>%
      addPolygons(data = shp.df,
                  fillColor = ~nc_pal(`CAUTION LEVEL`),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.5,
                  highlight = highlightOptions(weight = 3,
                                               color = "white",
                                               dashArray = "",
                                               fillOpacity = 0.9,
                                               bringToFront = TRUE),
                  label = sprintf(shp.df$COMMUNITY) %>% 
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(direction = "auto")) %>%
      addLegend("bottomleft", 
                colors = c("#850101","red","orange"), 
                labels = c("High", "Medium", "Low"), 
                title = "Caution Level",
                opacity = 0.7)
    
    
    
  })
  
  
  
  #-----------------------------------------------
  # Data Tab
  #-----------------------------------------------
  
  
  output$col.details <- renderDataTable({return(col.details)},
                                        options = list(autoWidth = TRUE,
                                                       columnDefs = (list(list(width = '600px', 
                                                                               targets ="Column Name"), 
                                                                          list(width = '800px', 
                                                                               targets ="Description")))
                                        ))
  
  
  output$rawtable <- renderDataTable(options = list(pageLength = 10),
                                     {return(crime)})
  
  
  output$downloadCsv <- downloadHandler(filename = "chicago_crime2020.csv",
                                        content = function(file) {
                                          write.csv(crime, file)},
                                        contentType = "text/csv")    
  
}


