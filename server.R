#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
library(RColorBrewer)
library(gplots)
library(rgdal)
library(rgeos)

aft.colors = rich.colors(17)
aft.names.fromzero <- c( "Ext_AF", "IA", "Int_AF", "Int_Fa", "IP", "MF", "Min_man", "Mix_Fa", "Mix_For", "Mix_P", "Multifun", "P-Ur", "UL", "UMF", "Ur", "VEP", "EP")
aft.pal <- colorFactor(rich.colors(17),levels = 1:17, na.color = "transparent") # "#0C2C84", "#41B6C4", "#FFFFCC"), # , bins = 17) 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #r <- projectRaster( raster(paste0("Data/Maps/Baseline-0-0-EU-Cell-2016_LL.tif"), 16), crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E5)

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  rnew <- eventReactive(input$Year, {
    
    r_changed= raster(paste0("Data/Maps/Baseline-0-0-EU-Cell-", input$Year, "_LL.tif"), 16)
     projectRaster(r_changed, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E4)
    # return(r_changed)
    # r <-   raster(paste0("Data/Maps/Baseline-0-0-EU-Cell-", input$year, "_LL.tif"), 16)
  })
  
 
   
  # observeEvent(input$Year, {
  #    leafletProxy('mymap') %>%
  #        addRasterImage(r2)})
  
  # output$distPlot <- renderPlot({
  # 
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = 11)
  # 
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # 
  # })
  


  
  
  # points <- eventReactive(input$year, {
  # cbind(rnorm(1) * 2 + 13, rnorm(1) + 48)
  # }, ignoreNULL = FALSE)
  # points <- eventReactive(input$year, {
  # cbind(rnorm(1) * 2 + 13, rnorm(1) + 48)
  # }, ignoreNULL = FALSE)
  # 
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      
      
      #addProviderTiles(providers$Stamen.TonerLite,
      #                 options = providerTileOptions(noWrap = TRUE)
      #) %>%
       # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
    
      addRasterImage(r, project = FALSE, colors = pal, opacity = 0.8, maxBytes = 8 * 1024 * 1024) %>%
    #  addLegend( pal = aft.pal, values = 1:17, labels = aft.names.fromzero, title = "AFT")
      addLegend(colors = col2hex(aft.colors), labels = aft.names.fromzero, title = "AFT")
    
    #%>%
    # addMarkers(data = points())
  })
  
  
  # should be managed in its own observer.
  observe({
    proxy <- leafletProxy("mymap",data =rnew())
      proxy %>%
      clearImages() %>%
      addRasterImage(rnew(), project = FALSE, colors = pal, opacity = 0.8, maxBytes = 8 * 1024 * 1024)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  # observe({
  #   pal <- colorpal()
  # 
  #   leafletProxy("mymap", data = filteredData()) %>%
  #     clearShapes() %>%
  #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
  #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
  #     )
  # })
  #

  #
  # # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("mymap", data = quakes)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = pal, values = ~mag
  #     )
  #   }
  # })
  # # 
  
  
  
  
})
