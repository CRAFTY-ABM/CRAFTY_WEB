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

source("Functions_CRAFTY_WEB.R")





# r.default <- projectRaster( raster(paste0("Data/Maps/", scenarioname.default, "-0-0-EU-Cell-2016_LL.tif"), 16), crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E4)
# [1] "Tick"                           "X"                              "Y"                             
# [4] "Service.Meat"                   "Service.Crops"                  "Service.Diversity"             
# [7] "Service.Timber"                 "Service.Carbon"                 "Service.Urban"                 
# [10] "Service.Recreation"             "Capital.Crop.productivity"      "Capital.Forest.productivity"   
# [13] "Capital.Grassland.productivity" "Capital.Financial.capital"      "Capital.Human.capital"         
# [16] "Capital.Social.capital"         "Capital.Manufactured.capital"   "Capital.Urban.capital"         
# [19] "LandUse"                        "LandUseIndex"                   "Agent"                         
# [22] "Competitiveness"                "lon"                            "lat"         


accessDropbox()


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  # Reactive expression for the data subsetted to what the user selected
  # filteredData <- reactive({
  #   quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  # })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  # scenarioname <- reactive({
  #   ret = (input$scenario)
  #   # str(ret)
  #   # print(ret)
  #   return(ret)
  # })
  # 
  rnew <- reactive({
     
    runid = which(scenario.names == input$scenario) - 1 
    
    fname_changed =  paste0("Data/", input$paramset, "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$Year, ".csv")
    spdf_changed = getSPDF(fname_changed)
    rs_changed <- stack(spdf_changed)[[4:22]]
    
    # r_changed= raster(paste0("Data/Maps/", input$scenario, "-0-0-EU-Cell-", input$Year, "_LL.tif"), 16)
    indicator_idx = which (input$indicator == indicator.names)
    r_changed = rs_changed[[indicator_idx]] 
    
    projectRaster(r_changed, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E4)
    # return(r_changed)
    # r <-   raster(paste0("Data/Maps/Baseline-0-0-EU-Cell-", input$year, "_LL.tif"), 16)
  })
  
  
  # points <- eventReactive(input$year, {
  # cbind(rnorm(1) * 2 + 13, rnorm(1) + 48)
  # }, ignoreNULL = FALSE)
  # points <- eventReactive(input$year, {
  # cbind(rnorm(1) * 2 + 13, rnorm(1) + 48)
  # }, ignoreNULL = FALSE)
  # 
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      #addTiles() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      
      addRasterImage(r.default, project = FALSE, colors = aft.pal, opacity = input$alpha, maxBytes = 4 * 1024 * 1024) %>%
      #  addLegend( pal = aft.pal, values = 1:17, labels = aft.names.fromzero, title = "AFT")
      addLegend(colors = col2hex(aft.colors), labels = aft.names.fromzero, title = "AFT")
    
    #%>%
    # addMarkers(data = points())
  })
  
  
  # should be managed in its own observer.
  observe({
    dt = rnew()
    proxy <- leafletProxy("mymap",data =rnew())
    proxy %>%
      clearImages() %>%
      addRasterImage(dt, project = FALSE, colors =pal.list[[which (input$indicator == indicator.names)]]
                     , opacity = input$alpha, maxBytes = 4 * 1024 * 1024) %>%
    clearControls()
      
    if (input$indicator %in% c("LandUse", "LandUseIndex", "Agent")) { 
      proxy %>%
        addLegend(colors = (aft.colors), labels = aft.names.fromzero, title = input$indicator)
    } else {
      
      proxy %>%
        addLegend(pal = pal.list[[which (input$indicator == indicator.names)]], values = quantile(dt, probs=seq(0, 1, 0.05)) 
                  , title = input$indicator)
    }
    
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
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("mymap")
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    
    if (input$legend) {
      #proxy %>% addLegend(colors = col2hex(aft.colors), labels = aft.names.fromzero, title = "AFT")
      
      if (input$indicator %in% c("LandUse", "LandUseIndex", "Agent")) { 
        proxy %>%
          addLegend(colors = col2hex(aft.colors), labels = aft.names.fromzero, title = input$indicator)
      } else {
        
        # proxy %>%
        #   addLegend(pal = pal.list[[which (input$indicator == indicator.names)]], values = quantile(dt, probs=seq(0, 1, 0.05)) 
        #             , title = input$indicator)
      }
      
    }
  })
  # # 
  
  
  
  
  
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
  
  
  
})
