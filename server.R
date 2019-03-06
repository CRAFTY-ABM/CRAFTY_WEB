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
  
  runinfo <- reactive({
    p.idx = which(input$paramset == paramsets)
    
    paste0("Simulated ", input$outputlayer, " in ", input$year, " with the ", paramsets.fullanems[p.idx], " parameters (", input$paramset, ") and ",  input$scenario, " scenario." )
  })
  
 
  output$PaneRuninfo <- renderText({
    runinfo()
  })

  output$PaneRuninfo2 <- renderText({
    runinfo()
  })
  
  rnew <- reactive({
    
    runid = which(scenario.names == input$scenario) - 1 
    indicator_idx = which (input$outputlayer == indicator.names)
    
    fname_changed =  paste0("Data/", input$paramset, "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    # spdf_changed = getSPDF(fname_changed)
    # rs_changed = stack(spdf_changed)[[4:22]]
    # 
    # # r_changed= raster(paste0("Data/Maps/", input$scenario, "-0-0-EU-Cell-", input$year, "_LL.tif"), 16)
    # r_changed = rs_changed[[indicator_idx]] 
    # print(indicator_idx)
    #  
    r_changed = getRaster(fname_changed, band.idx = indicator_idx)
    
    projectRaster(r_changed, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E4)
    # return(r_changed)
    # r <-   raster(paste0("Data/Maps/Baseline-0-0-EU-Cell-", input$year, "_LL.tif"), 16)
  })
  
  providernew <- reactive({
    input$background
  })
    
  
  # points <- eventReactive(input$year, {
  # cbind(rnorm(1) * 2 + 13, rnorm(1) + 48)
  # }, ignoreNULL = FALSE)
  # points <- eventReactive(input$year, {
  # cbind(rnorm(1) * 2 + 13, rnorm(1) + 48)
  # }, ignoreNULL = FALSE)
  # 
  
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    
    runid = which(scenario.names == input$scenario) - 1 
    fname_changed =  paste0("Data/", input$paramset, "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    spdf_changed = getSPDF(fname_changed)
    
    target_val = spdf_changed[4:22]
    
    # hist(as.numeric(target_val$Service.Meat))
    return(target_val)
  })
  
  
  output$PlotPane <- renderPlot({
    
    runid = which(scenario.names == input$scenario) - 1 
    fname_changed =  paste0("Data/", input$paramset, "/", input$scenario, "/", input$scenario, "-",runid, "-",seedid,"-EU-Cell-", input$year, ".csv")
    spdf_changed = getSPDF(fname_changed)
    target_val = spdf_changed[4:22]
    
    par(mar = c(5.1, 4.1, 4, 1))
    plot(target_val@data[, input$outputlayer], main=input$outputlayer)
  })
  
  
  output$Tab1_SubplotPane <- renderPlot({
    
    runid = which(scenario.names == input$scenario) - 1 
    # csvname_changed = "Data/Paramset1/Baseline/Baseline-0-99-EU-AggregateServiceDemand.csv"
    
    aft_csvname_changed =  paste0("Data/", input$paramset, "/", input$scenario, "/", input$scenario, "-",runid, "-", seedid, "-EU-AggregateAFTComposition.csv") 
    aftcomp_dt = getCSV(aft_csvname_changed)
    aftcomp_m = t(as.matrix(sapply(aftcomp_dt[, -c(1,2)] , FUN = function(x) as.numeric(as.character(x)))))
    
    
    demand_csvname_changed =  paste0("Data/", input$paramset, "/", input$scenario, "/", input$scenario, "-",runid, "-", seedid, "-EU-AggregateServiceDemand.csv") 
    demand_dt = getCSV(demand_csvname_changed)
    demand_m = t(as.matrix(sapply(demand_dt[, -c(15,16)] , FUN = function(x) as.numeric(as.character(x)))))
    
    
    serviceNames <- c("Meat","Crops", "Diversity", "Timber", "Carbon", "Urban", "Recreation")
    serviceColours <- c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity" = "red", "Timber" = "tan4", "Carbon" = "darkgreen", "Urban" = "grey", "Recreation" = "orange")
    
    # demand.colors = rich.colors(7)
    # # library(ggplot2)
    # # ggplot(aftcomp_dt)
    
    par(mfrow=c(2,2), mar = c(5.1, 4.1, 4, 1))
    # AFT changes
    barplot(height = aftcomp_m/colSums(aftcomp_m) * 100, ylab="%", col = (aft.colors.fromzero), main = "AFT composition", names= target_years)
    barplot(height = demand_m[1:7,], beside=T, ylab="Service Supply", col = serviceColours, main = "Service Supply", names= demand_dt$Tick)
    legend("topright", legend = serviceNames, fill=serviceColours, cex=0.7, bty="n")
    
    barplot(height = demand_m[8:14,], beside=T, ylab="Demand", col = serviceColours, main = "Service Demand", names= demand_dt$Tick)
    barplot(height = (demand_m[8:14,] - demand_m[1:7,]) , beside=T, ylab="Demand - Supply", col = serviceColours, main = "S/D gap", names= demand_dt$Tick)
    legend("topright", legend = serviceNames, fill=serviceColours, cex=0.7, bty="n")
    
  })
  
  # observe({
  #   # dt.plot = selectedData()
  #   input$
  #   par(mar = c(5.1, 4.1, 4, 1))
  #   plot(selectedData()@data[, input$outputlayer], main=input$indicator)
  #   
  # })
  
  
  output$MapPane <- renderLeaflet({
    
    leaflet() %>%
      #addTiles() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, # Esri.WorldImagery
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      
      addRasterImage(r.default, project = FALSE, colors = aft.pal, maxBytes = 4 * 1024 * 1024) %>%
      #  addLegend( pal = aft.pal, values = 1:17, labels = aft.names.fromzero, title = "AFT")
      addLegend(colors = col2hex(as.character(aft.colors.fromzero)), labels = aft.names.fromzero, title = indicator.names[17])
    
    #%>%
    # addMarkers(data = points())
  })
  
  
  observe({
    # dt = providernew()
    proxy <- leafletProxy("MapPane", data = providernew())
    proxy %>% clearTiles() %>% addProviderTiles(input$background, options = providerTileOptions(noWrap = TRUE))  
    
  })
  
  
  # should be managed in its own observer.
  observe({
    dt = rnew()
    proxy <- leafletProxy("MapPane", data =rnew())
    proxy %>% clearImages() %>% clearControls() 
    
    # touches
    input$background
    
    # Layers control
    proxy %>% addLayersControl(
      # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("OutputLayer"),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    if (input$outputlayer %in% c("LandUse", "LandUseIndex", "Agent")) {
      proxy %>%  
        addRasterImage(dt, project = FALSE, colors =aft.colors.fromzero, group = "OutputLayer"
                       , opacity = input$alpha, maxBytes = 4 * 1024 * 1024) %>%
        addLegend(colors = col2hex(as.character(aft.colors.fromzero)), labels = aft.names.fromzero, title = input$outputlayer)
      
      # addLegend(colors = (aft.colors.fromzero), labels = aft.names.fromzero, title = input$indicator)
    } else {
      # print(which (input$indicator == indicator.names))
      # print(pal.list)
      dt.v = getValues(dt)
      dt.rng = range(dt.v, na.rm = T)
      print(dt.rng)
      pal = colorNumeric(input$colors,reverse = T, domain = dt.rng, na.color = "transparent")
      
      proxy %>%
        addRasterImage(dt, project = FALSE, colors =pal, group = "OutputLayer"
                       , opacity = input$alpha, maxBytes = 4 * 1024 * 1024) %>% 
        addLegend(pal = pal, values = quantile(dt.v, probs=seq(1, 0, -0.05), na.rm=T), 
                  , title = input$outputlayer, labFormat = labelFormat(transform = function(x) sort(quantile(dt.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)))
    }
    

    
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  # observe({
  #   pal <- colorpal()
  # 
  #   leafletProxy("MapPane", data = filteredData()) %>%
  #     clearShapes() %>%
  #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
  #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
  #     )
  # })
  #
  
  #
  # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("MapPane")
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   
  #   if (input$legend) {
  #     #proxy %>% addLegend(colors = col2hex(aft.colors.fromzero), labels = aft.names.fromzero, title = "AFT")
  #     
  #     if (input$outputlayer %in% c("LandUse", "LandUseIndex", "Agent")) { 
  #       proxy %>%
  #         addLegend(colors = col2hex(aft.colors.fromzero), labels = aft.names.fromzero, title = input$indicator)
  #     } else {
  #       dt.v = getValues(dt)
  #       dt.rng = range(dt.v, na.rm = T)
  #        pal = colorNumeric("RdYlBu", domain = dt.rng, na.color = "transparent")
  #       addLegend(pal = pal , values = quantile(dt.v, probs=seq(0, 1, 0.05), na.rm=T) 
  #                 , title = input$indicator)
  #       
  #     }
  #     
  #   }
  # })
  # # 
  
  output$downloadData <- downloadHandler(
    filename = function() {
      
      runid = which(scenario.names == input$scenario) - 1 
      indicator_idx = which (input$outputlayer == indicator.names)
      
      fname_changed =  paste0("CRAFTY-EU_", input$paramset, "_", input$scenario, "_", input$year, "_", input$outputlayer, ".tif")
      
      # fname_changed      
    },
    content = function(file) {
      runid = which(scenario.names == input$scenario) - 1 
      indicator_idx = which (input$outputlayer == indicator.names)
      
      fname_changed =  paste0("Data/", input$paramset, "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
      print(fname_changed)
      data = projectRaster(getRaster(fname_changed, band.idx = indicator_idx), crs = proj4.LL)
      
      writeRaster(data, file)
    }
  )
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
  # output$summary <- renderPrint({
  #   summary(cars)
  # })
  # 
  # output$table <- DT::renderDataTable({
  #   DT::datatable(cars)
  # })
  
})

# function(input, output, session) {
#   output$plot <- renderPlot({
#     plot(cars, type=input$plotType)
#   })
#   
#   output$summary <- renderPrint({
#     summary(cars)
#   })
#   

# }