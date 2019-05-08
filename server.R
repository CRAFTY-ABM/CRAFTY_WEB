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
# library(rgdal)
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
    p.idx = which(input$paramset_full == paramsets.fullnames)
    
    paste0("Simulated ", input$outputlayer, " in ", input$year, " with the ", input$paramset_full, " parameters and ",  input$scenario, " scenario." )
  })
  
  
  runinfo2 <- reactive({
    p.idx = which(input$paramset_full == paramsets.fullnames)
    
    paste0("Simulated ", input$outputlayer," with the ", input$paramset_full, " parameters and ",  input$scenario, " scenario." )
  })
  
  
  
  observeEvent(input$deleteCache, {
    # session$sendCustomMessage(type = 'testmessage',
    # message = 'Thank you for clicking')
    unlink(path.rastertmp)
    unlink(path.droptmp)
    
  })
  
  
  output$PaneRuninfo <- renderText({
    runinfo()
  })
  
  output$PaneRuninfo2 <- renderText({
    runinfo2()
  })
  
  
  
  
  
  rnew <- reactive({
    
    runid = which(scenario.names == input$scenario) - 1
    # runid = 0
    p.idx = which(input$paramset_full == paramsets.fullnames)
    
    fname_changed =  paste0("Data/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    # spdf_changed = getSPDF(fname_changed)
    # rs_changed = stack(spdf_changed)[[4:22]]
    # 
    # # r_changed= raster(paste0("Data/Maps/", input$scenario, "-0-0-EU-Cell-", input$year, "_LL.tif"), 16)
    # r_changed = rs_changed[[indicator_idx]] 
    # print(indicator_idx)
    
    
    indicator_idx = which (input$outputlayer == indicator.names)
    r_changed = getRaster(fname_changed, band.idx = indicator_idx)
    
    # r_changed_projected = projectRaster(r_changed, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E4)
    
    return(r_changed)
    # r <-   raster(paste0("Data/Maps/Baseline-0-0-EU-Cell-", input$year, "_LL.tif"), 16)
  })
  
  rnew_input <- reactive({
    
    runid = which(scenario.names == input$scenario) - 1
    # runid = 0 
    
    p.idx = which(input$paramset_full == paramsets.fullnames)
    
    
    fname_changed =  paste0("Data/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    
    indicator_idx = which (input$inputlayer == indicator.names)
    r_changed = getRaster(fname_changed, band.idx = indicator_idx)
    
    return(r_changed)
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
    p.idx = which(input$paramset_full == paramsets.fullnames)
    
    fname_changed =  paste0("Data/",  paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    spdf_changed = getSPDF(fname_changed)
    
    target_val = spdf_changed[4:22]
    
    # hist(as.numeric(target_val$Service.Meat))
    return(target_val)
  })
  
  
  output$Tab1_SubPlotPane <- renderPlot({
    
    target_data = rnew()
    
    # runid = which(scenario.names == input$scenario) - 1 
    # p.idx = which(input$paramset_full == paramsets.fullnames)
    # 
    # fname_changed =  paste0("Data/",  paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-",seedid,"-EU-Cell-", input$year, ".csv")
    # spdf_changed = getSPDF(fname_changed)
    # target_val = spdf_changed[4:22]
    # 
    # str(getValues(target_data))
    
    par(mar = c(5.1, 4.1, 4, 1), mfrow=c(1,2))
    hist(getValues(target_data), main="Histogram", xlab= input$outputlayer)
    # plot(target_val@data[, input$inputlayer], main=input$inputlayer)
    
    
    # target_val = selectedData()
    # rs.LL = stack(target_val)[[17]]
    # classstat.res.LL = ClassStat(rs.LL, cellsize = 15000, bkgd = NA, latlon = T)
    # boxplot(classstat.res.LL, las=2, ylim=c(-10, 10))
    # boxplot(classstat.res.LL$mean.frac.dim.index, classstat.res.LL$patch.cohesion.index, classstat.res.LL$aggregation.index)
    
  })
  # observe({
  #   # dt.plot = selectedData()
  #   input$
  #   par(mar = c(5.1, 4.1, 4, 1))
  #   plot(selectedData()@data[, input$outputlayer], main=input$indicator)
  #   
  # })
  
  output$Tab2_SummaryPlotPane <- renderPlot(height = 600, res = 96, {
    
    runid = which(scenario.names == input$scenario) - 1
    # runid = 0
    
    # csvname_changed = "Data/Paramset1/Baseline/Baseline-0-99-EU-AggregateServiceDemand.csv"
    p.idx = which(input$paramset_full == paramsets.fullnames)
    
    aft_csvname_changed =  paste0("Data/",  paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-", seedid.v[p.idx], "-EU-AggregateAFTComposition.csv") 
    aftcomp_dt = getCSV(aft_csvname_changed)
    aftcomp_m = t(as.matrix(sapply(aftcomp_dt[, -c(1,2)] , FUN = function(x) as.numeric(as.character(x)))))
    
    # 8 classes 
    aftcomp_8classes_by = by(aftcomp_m, INDICES = aft.lookup.17to8[-1, 2], FUN = colSums)
    
    aftcomp_8classes_m = t(sapply(aftcomp_8classes_by, c))[aft.fullnames.8classes, ]
    
    
    
    
    demand_csvname_changed =  paste0("Data/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-", seedid.v[p.idx], "-EU-AggregateServiceDemand.csv") 
    demand_dt = getCSV(demand_csvname_changed)
    demand_m = t(as.matrix(sapply(demand_dt[, -c(15,16)] , FUN = function(x) as.numeric(as.character(x)))))
    
    
    shortfall_m = ((demand_m[8:14,] - demand_m[1:7,]) / demand_m[8:14,] ) * 100 
    
    # demand.colors = rich.colors(7)
    # # library(ggplot2)
    # # ggplot(aftcomp_dt)
    
    par(mfrow=c(2,2), mar = c(5.1, 4.1, 4, 1))
    
    # plot(target_years, aftcomp_perc_m[1,], type="l", ylab="%", col = aft.colors.fromzero[1], ylim=c(0, max(aftcomp_perc_m, na.rm = T) * 1.1), main = "AFT (17) composition changes")
    # 
    # for (a.idx in 2:8) { 
    #   lines(target_years, aftcomp_perc_m[a.idx,],   col = aft.colors.fromzero[a.idx])
    # }
    # legend("topright", aft.shortnames.fromzero, col = aft.colors.fromzero, lty=1, cex=0.7, bty="n")
    # 
    
    
    aftcomp_8classes_perc_m = aftcomp_8classes_m/colSums(aftcomp_8classes_m) * 100
    # barplot(height = aftcomp_8classes_perc_m, ylab="%", col = aft.colors.8classes, main = "AFT composition", names= target_years)
    
    plot(target_years, aftcomp_8classes_perc_m[1,], type="l", xlab= "Year", ylab="EU-28 proportion (%)", col = aft.colors.8classes[1], ylim=c(0, max(aftcomp_8classes_perc_m, na.rm = T) * 1.1), main = "AFT (8) composition changes")
    
    for (a.idx in 2:8) { 
      lines(target_years, aftcomp_8classes_perc_m[a.idx,],   col = aft.colors.8classes[a.idx])
    }
    legend("topright", aft.fullnames.8classes, col = aft.colors.8classes, lty=1, cex=0.7, bty="n")
    
    # barplot(height = demand_m[1:7,], beside=T, ylab="Service Supply", col = serviceColours, main = "Service Supply", names= demand_dt$Tick)
    # legend("topright", legend = serviceNames, fill=serviceColours, cex=0.7, bty="n")
    # 
    # barplot(height = demand_m[8:14,], beside=T, ylab="Demand", col = serviceColours, main = "Service Demand", names= demand_dt$Tick)
    # barplot(height = (demand_m[8:14,] - demand_m[1:7,]) , beside=T, ylab="Demand - Supply", col = serviceColours, main = "S/D gap", names= demand_dt$Tick)
    
    
    plot(demand_dt$Tick, shortfall_m[1,], type="l", col = serviceColours[1], ylim=c(-200,200), xlab="Year", ylab="Production shortfall (%)",  main = "Production shortfall")
    
    for (a.idx in 2:7) { 
      lines(demand_dt$Tick, shortfall_m[a.idx,],   col = serviceColours[a.idx])
    }
    
    legend("topright", legend = serviceNames, col=serviceColours, lty = 1, cex=0.7, bty="n")
    
    
    # AFT changes
    aftcomp_perc_m =  aftcomp_m/colSums(aftcomp_m) * 100
    
    plot(target_years, aftcomp_perc_m[1,], type="l", xlab= "Year", ylab="EU-28 proportion (%)", col = aft.colors.fromzero[1], ylim=c(0, max(aftcomp_perc_m, na.rm = T) * 1.1), main = "AFT (17) composition changes")
    
    for (a.idx in 2:nrow(aftcomp_perc_m)) { 
      lines(target_years, aftcomp_perc_m[a.idx,],   col = aft.colors.fromzero[a.idx])
    }
    legend("topright", aft.shortnames.fromzero, col = aft.colors.fromzero, lty=1, cex=0.7, bty="n")
    
    # barplot(height = demand_m[1:7,], beside=T, ylab="Service Supply", col = serviceColours, main = "Service Supply", names= demand_dt$Tick)
    # legend("topright", legend = serviceNames, fill=serviceColours, cex=0.7, bty="n")
    # 
    # barplot(height = demand_m[8:14,], beside=T, ylab="Demand", col = serviceColours, main = "Service Demand", names= demand_dt$Tick)
    # barplot(height = (demand_m[8:14,] - demand_m[1:7,]) , beside=T, ylab="Demand - Supply", col = serviceColours, main = "S/D gap", names= demand_dt$Tick)
    
  })
  
  
  
  output$MapPane <- renderLeaflet({
    r_dummy  = raster(extent(r.default))
    r_dummy = setValues(r_dummy, values = 0)
    proj4string(r_dummy) = proj4string(r.default)
    
    leaflet() %>%
      clearImages() %>% clearControls() %>%
      #addTiles()
      addProviderTiles(providers$OpenStreetMap.Mapnik, # Esri.WorldImagery
                       options = providerTileOptions(noWrap = TRUE), group = "TileLayer"
      ) %>%
      # %>%
      # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
      addRasterImage(r_dummy, project = FALSE, group="OutputLayer") # , colors = aft.pal, maxBytes = 4 * 1024 * 1024) %>%
    #  addLegend( pal = aft.pal, values = 1:17, labels = aft.names.fromzero, title = "AFT")
    # addLegend(colors = col2hex(as.character(aft.colors.fromzero)), labels = aft.names.fromzero, title = indicator.names[17])
    #%>%
    # addMarkers(data = points())
  })
  
  
  observe({
    # dt = providernew()
    proxy <- leafletProxy("MapPane", data = providernew())
    proxy %>% clearTiles() %>% addProviderTiles(input$background, options = providerTileOptions(noWrap = TRUE), group = "TileLayer")
    # proxy %>% clearControls() 
    
  })
  
  
  # should be managed in its own observer.
  observe({
    dt = rnew()
    # print(which (input$indicator == indicator.names))
    
    
    proxy <- leafletProxy("MapPane", data =rnew())
    proxy %>% clearImages() %>% clearControls() 
    
    # touches
    input$background
    
    # Layers control
    proxy %>% addLayersControl(
      # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      baseGroups = c("ModelOutput", "ModelInput"),  # , "OutputLayer"), 
      # overlayGroups = c(  "TileLayer"), 
      options = layersControlOptions(collapsed = FALSE)
    )
    
    
    # Add output layer 
    if (input$outputlayer %in% indicators_categorical) {
      
      if (input$outputlayer  == indicator.names[20]) {
        proxy %>%  
          addRasterImage(dt, project = FALSE, colors =aft.colors.8classes, group = "ModelOutput"
                         , opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
        proxy %>% addLegend(colors = col2hex(as.character(aft.colors.8classes)), labels = aft.names.8classes , title = paste0("Output: ", input$outputlayer),group = "ModelOutput")
      } else {
        proxy %>%  
          addRasterImage(dt, project = FALSE, colors =aft.colors.fromzero, group = "ModelOutput"
                         , opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
        proxy %>% addLegend(colors = col2hex(as.character(aft.colors.fromzero)), labels = aft.names.fromzero, title = paste0("Output: ", input$outputlayer),group = "ModelOutput")
      }
    } else {
      dt.v = getValues(dt)
      dt.rng = range(dt.v, na.rm = T)
      print(dt.rng)
      pal = colorNumeric(input$colors,reverse = T, domain = dt.rng,  na.color = "transparent")
      
      proxy %>%
        addRasterImage(dt, project = FALSE, colors =pal, group = "ModelOutput", method = "bilinear"
                       , opacity = input$alpha, maxBytes = 4 * 1024 * 1024) %>% 
        addLegend(pal = pal, values = quantile(dt.v, probs=seq(1, 0, -0.05), na.rm=T), 
                  , title = paste0("Output ", input$outputlayer), labFormat = labelFormat(transform = function(x) sort(quantile(dt.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)), group = "ModelOutput")
    }
    
    # Add input layer 
    dt_input = rnew_input()
    dt_input.v = getValues(dt_input)
    dt_input.rng = range(dt_input.v, na.rm = T)
    print(dt_input.rng)
    
    pal_input = colorNumeric(input$colors, reverse = T, domain = dt_input.rng, na.color = "transparent")
    
    proxy %>%
      addRasterImage(dt_input, project = FALSE, colors =pal_input, method = "bilinear", group = "ModelInput"
                     , opacity = input$alpha, maxBytes = 4 * 1024 * 1024) %>% 
      addLegend(pal = pal_input, values = quantile(dt_input.v, probs=seq(1, 0, -0.05), na.rm=T), 
                , title = paste0("Input ", input$inputlayer), labFormat = labelFormat(transform = function(x) sort(quantile(dt_input.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)), group = "ModelInput")
    
    
    
    
    
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
      # runid = 0 
      runid = which(scenario.names == input$scenario) - 1
      
       indicator_idx = which (input$outputlayer == indicator.names)
      p.idx = which(input$paramset_full == paramsets.fullnames)
      
      fname_changed =  paste0("CRAFTY-EU_",paramsets[p.idx], "_", input$scenario, "_", input$year, "_", input$outputlayer, ".tif")
      
      # fname_changed      
    },
    content = function(file) {
      # runid = 0 
      
      runid = which(scenario.names == input$scenario) - 1
      indicator_idx = which (input$outputlayer == indicator.names)
      p.idx = which(input$paramset_full == paramsets.fullnames)
      
      fname_changed =  paste0("Data/",paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
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