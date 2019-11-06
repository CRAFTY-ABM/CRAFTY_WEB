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
library(grid)
library(DT)
source("RScripts/Functions_CRAFTY_WEB.R")

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
  
  
  runinfo_ts <- reactive({
    p.idx = which(input$paramset_full_ts == paramsets.fullnames)
    
    paste0("Simulation with the ", input$paramset_full_ts, " parameters and ",  input$scenario_ts, " scenario." )
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
  
  output$PaneRuninfo_ts <- renderText({
   # runinfo_ts()
  })
  
  
  output$ReferenceToParameters <- renderText({
    "<br/>Behavioural parameter set 1 is the default from which main results are derived; in this setup agents respond directly to benefit values with no additional individual or typological behaviour. In parameter set 2, giving-up and giving-in thresholds are altered to introduce abandonmentof land when benefit values fall below the giving-up threshold value, and resistance to change unless a competing land use has an additional benefit value of at least the giving-in threshold. Intensive land use agents are parameterised to be less tolerantof low benefit values, and more willing to switch to a land use with higher benefit values. In parameter set 3, individual agents differ from one another in terms of their abilities to produce different ecosystem services, and their giving-up and giving-in thresholds. Parameter sets 4 and 5 replicate parameter sets 2 and 3 respectively, but with larger values for thresholds and variations.<p/>

Please refer to the following paper to understand the behavioural model used in CRAFTY:

<p/>
<i>Brown, C., Murray-Rust, D., Van Vliet, J., Alam, S. J., Verburg, P. H., & Rounsevell, M. D. (2014). Experiments in globalisation, food security and land use decision making. PLoS ONE, 9(12), 1–24. </i> <a href='https://doi.org/10.1371/journal.pone.0114213'>https://doi.org/10.1371/journal.pone.0114213</a>

<p/>
Please see the further details of the parameters in Table A4 of the following paper: <p/>
<i>Brown, C., Seo, B., & Rounsevell, M. (2019). Societal breakdown as an emergent property of large-scale behavioural models of land use change. Earth System Dynamics, (accepted), 1–49.</i> <a href='https://doi.org/10.5194/esd-2019-24'>https://doi.org/10.5194/esd-2019-24</a>"
      })
  
  
  
  output$ReferenceToAFT <- renderText({
    "<br/>Please see the detailed decription of the scenarios in Table 2 (pp. 27-28) of the following paper: <p/>
<i><small>Brown, C., Seo, B., & Rounsevell, M. (2019). Societal breakdown as an emergent property of large-scale behavioural models of land use change. Earth System Dynamics Discussions, (May), 1–49.</i> <a href='https://doi.org/10.5194/esd-2019-24'>https://doi.org/10.5194/esd-2019-24</a></small>"
  })
  
  output$ReferenceToScenarios <- renderText({
    "<br/>Please see the detailed decription of the scenarios in Table 2 (pp. 27-28) of the following paper: <p/>
<i><small>Brown, C., Seo, B., & Rounsevell, M. (2019). Societal breakdown as an emergent property of large-scale behavioural models of land use change. Earth System Dynamics Discussions, (May), 1–49.</i> <a href='https://doi.org/10.5194/esd-2019-24'>https://doi.org/10.5194/esd-2019-24</a></small>"
  })
  
  rnew <- reactive({
    
    runid = which(scenario.names == input$scenario) - 1
    # runid = 0
    p.idx = which(input$paramset_full == paramsets.fullnames)
    
    # fname_changed =  paste0("Data/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    # if (input$food != "Normal") { 
      
      fname_changed =   paste0("Data/", input$foodprice, "/",  input$fooddemand, "/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    # }
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
    
    
    # fname_changed =  paste0("Data/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    # if (input$food != "Normal") { 
      fname_changed =   paste0("Data/",  input$foodprice, "/",  input$fooddemand, "/",paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    # }
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
    
    fname_changed =  paste0("Data/",  input$foodprice, "/",  input$fooddemand, "/",  paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
    spdf_changed = getSPDF(fname_changed)
    
    target_val = spdf_changed[4:22]
    
    # hist(as.numeric(target_val$Service.Meat))
    return(target_val)
  })
  
  
  output$Tab1_StatisticsPane <- renderPlot({
    
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
  
  
  
  output$Tab1_BehaviouralTablePane <- renderDataTable(
    {
      
      
      runid = which(scenario.names == input$scenario) - 1
      p.idx = which(input$paramset_full == paramsets.fullnames)
      
      p_name = paste0("Tables/Paramset", p.idx, ".csv")
      
      p_tb = read.csv(p_name)
      
      
      # fname_changed =  paste0("Data/",  paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-",seedid,"-EU-Cell-", input$year, ".csv")
      # spdf_changed = getSPDF(fname_changed)
      # target_val = spdf_changed[4:22]
      # 
      # str(getValues(target_data))
      # tb1 =     table(getValues(target_data))
      # print(tb1)
      
          DT::datatable(p_tb, options= list(paging = FALSE),  editable = F )
    })
  
  
  
  # observe({
  #   # dt.plot = selectedData()
  #   input$
  #   par(mar = c(5.1, 4.1, 4, 1))
  #   plot(selectedData()@data[, input$outputlayer], main=input$indicator)
  #   
  # })
  
  output$Tab2_TimeseriesPlotPane <- renderPlot(height = PLOT_HEIGHT, res = 96, {
    
    runid = which(scenario.names == input$scenario_ts ) - 1 
    # csvname_changed = "Data/Paramset1/Baseline/Baseline-0-99-EU-AggregateServiceDemand.csv"
    p.idx = which(input$paramset_full_ts == paramsets.fullnames)
    
    aft_csvname_changed =  paste0("Data/",  input$foodprice_ts, "/",  input$fooddemand_ts, "/", paramsets[p.idx], "/", input$scenario_ts, "/", input$scenario_ts, "-",runid, "-", seedid, "-EU-AggregateAFTComposition.csv") 
    aftcomp_dt = getCSV(aft_csvname_changed)
    aftcomp_m = t(as.matrix(sapply(aftcomp_dt[, -c(1,2)] , FUN = function(x) as.numeric(as.character(x)))))
    
    # 8 classes 
    aftcomp_8classes_by = by(aftcomp_m, INDICES = aft.lookup.17to8[-1, 2], FUN = colSums)
    
    aftcomp_8classes_m = t(sapply(aftcomp_8classes_by, c))[aft.fullnames.8classes, ]
    
    
     
    demand_csvname_changed =  paste0("Data/",  input$foodprice_ts, "/",  input$fooddemand_ts, "/", paramsets[p.idx], "/", input$scenario_ts, "/", input$scenario_ts, "-",runid, "-", seedid, "-EU-AggregateServiceDemand.csv") 
    demand_dt = getCSV(demand_csvname_changed)
    demand_m = t(as.matrix(sapply(demand_dt[, -c(15,16)] , FUN = function(x) as.numeric(as.character(x)))))
    
    
    shortfall_m = ((demand_m[8:14,] - demand_m[1:7,]) / demand_m[8:14,] ) * 100 
    
    # demand.colors = rich.colors(7)
    # # library(ggplot2)
    # # ggplot(aftcomp_dt)
    
    
    LEGEND_MAR = -0.2 
    LEGEND_CEX = 1
    par(mfrow=c(4,2), mar = c(5.1, 4.1, 4, 1)  + c(0,0,0,8), oma=c(1,1,1,1))
    
    # par(mfrow=c(4,2), xpd = T, mar = par()$mar + c(0,0,0,7))
    
    aftcomp_8classes_perc_m = aftcomp_8classes_m/colSums(aftcomp_8classes_m) * 100
    # barplot(height = aftcomp_8classes_perc_m, ylab="%", col = aft.colors.8classes, main = "AFT composition", names= target_years_aggcsv)
    
    par( mar = c(5.1, 4.1, 4, 1)  + c(0,0,0,10))
    
    plot(target_years_other[-9], aftcomp_8classes_perc_m[1,], type="l", xlab= "Year", ylab="EU-28 proportion (%)", col = aft.colors.8classes[1], ylim=c(0, max(aftcomp_8classes_perc_m, na.rm = T) * 1.1), main = "Land use composition changes", xaxt="n")
    axis(side=1, at = target_years_other[-9], labels = target_years_other[-9])
    
    for (a.idx in 2:8) { 
      lines(target_years_other[-9], aftcomp_8classes_perc_m[a.idx,],   col = aft.colors.8classes[a.idx])
    }
    legend("topright", aft.fullnames.8classes, col = aft.colors.8classes, lty=1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR-0.2,0))
    
    
    
    plotting17AFT = FALSE # summerschool version
    if (plotting17AFT) { 
      # AFT changes
      aftcomp_perc_m =  aftcomp_m/colSums(aftcomp_m) * 100
      
      plot(target_years_other, aftcomp_perc_m[1,], type="l", xlab= "Year", ylab="EU-28 proportion (%)", col = aft.colors.fromzero[1], ylim=c(0, max(aftcomp_perc_m, na.rm = T) * 1.1), main = "AFT (17) composition changes", xaxt="n")
      
      for (a.idx in 2:nrow(aftcomp_perc_m)) { 
        lines(target_years_other, aftcomp_perc_m[a.idx,],   col = aft.colors.fromzero[a.idx])
      }
      legend("topright", aft.shortnames.fromzero, col = aft.colors.fromzero, lty=1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))
      
    }
    par( mar = c(5.1, 4.1, 4, 1)  + c(0,0,0,8))
    
    ### Plotting number of changed pixels
    print("changed pixels")
    cnp_path =  paste0("Tables/ChangedPixelNo/",input$foodprice_ts, "/", input$fooddemand_ts, "/",paramsets[p.idx], "/", input$scenario_ts, "_ChangedPixelNo.xlsx")
    
    cnp_dt = readxl::read_excel(cnp_path, sheet = 1)
    cnp_v = as.numeric(unlist(cnp_dt))
    cnp_max = max(c(500, cnp_v), na.rm = T)
    
    
    
    plot(target_years_other[c(2:8)],cnp_v, ylim=c(0, cnp_max), type="l", xlab= "Year", ylab="Number of pixels changed", col = "blue", main = "Magnitude of land use change", xaxt ="n")
    
    axis(side=1, at = target_years_other[-1], labels = target_years_other[-1])
    
    
    
    
   
     
    
    ### Plotting service supply and demand 
    

    supply_m_norm = (demand_m[1:7,] / demand_m[1:7,1] - 1) * 100 
    supdem_range = range(supply_m_norm)
    y_lim_max = max(10, max(abs(supdem_range)) * 1.2)
    y_lim = c(-y_lim_max, y_lim_max)
    barplot(height = supply_m_norm, beside=T, ylab= "Relative to 2016's supply (%)", ylim= y_lim, col = serviceColours, main = "Service Supply", names= demand_dt$Tick)
    legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))
    
    
    demand_m_norm = (demand_m[8:14,]/ demand_m[1:7,1]-1) * 100
    barplot(height = demand_m_norm, beside=T, ylab="Relative to 2016's supply (%)", col = serviceColours, main = "Service Demand", names= demand_dt$Tick, ylim=y_lim)
    legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))
    
    sdgap  = (demand_m[8:14,] - demand_m[1:7,]) 
    # sdgap = (sdgap /demand_m[1:7,1] -1 ) * 100 
    sdgap_range = range(sdgap, na.rm=T)
    y_lim_max = max(abs(sdgap_range)) *2
    y_lim = c(-y_lim_max, y_lim_max)
    
    
    barplot(height =  sdgap, beside=T, ylab="Demand - Supply", col = serviceColours, main = "S/D gap", names= demand_dt$Tick, ylim = y_lim)
    legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))
    
    
    shortfall_range = range(shortfall_m[1,], na.rm = T)
    shortfall_max = max(abs(shortfall_range)) * 2
    shortfall_intv = floor(shortfall_max / 100) * 10 
    shortfall_intv = max(1,shortfall_intv)
    # print(shortfall_intv)
    plot(demand_dt$Tick, shortfall_m[1,], type="l", col = serviceColours[1], ylim=c(-shortfall_max,shortfall_max), xlab="Year", ylab="Production shortfall (%)",  main = "Production shortfall", las=1, xaxt="n" )
    axis(side=1, at = target_years_other, labels = target_years_other)
    # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    abline(h = 0, lty=2)
    
    for (a.idx in c(2:5, 7)) { 
      lines(demand_dt$Tick, shortfall_m[a.idx,],   col = serviceColours[a.idx])
    }
    
    legend("topright", legend = serviceNames[-6], col=serviceColours[-6], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))
    
    
    
    print("fragmentation statistics")
    
    
    ### fragmentation statistics
    frac_path =  paste0("Tables/FragStats/",input$foodprice_ts, "/", input$fooddemand_ts, "/",paramsets[p.idx], "/", input$scenario_ts, "_FragStats.xlsx")
    frac_dim = as.matrix( readxl::read_excel(frac_path, sheet = 1))
    
    
    frac_dim_norm =  frac_dim / frac_dim[,1] * 100  - 100
    
    frac_v = as.numeric(colMeans(frac_dim_norm))
    frac_max = max(c(abs(frac_v)), na.rm = T) * 1.2
    # barplot(t(frac_dim), beside=T)
    plot(x=target_years_other, y = frac_v, ylim=c(-frac_max, frac_max), type="l",xlab="Year", ylab="Relative to 2016 (%)", col= "blue", main = "Avg. Fractal Dimension")
    abline(h = 0, lty=2)
    
    
  })
  
  
  output$Tab3_TransitionPlotPane <- renderPlot(height = PLOT_HEIGHT, res = 96, {
    
    
    runid_from = which(scenario.names == input$scenario_from ) - 1 
    runid_to = which(scenario.names == input$scenario_to ) - 1 
    
    p_from.idx = which(input$paramset_full_from == paramsets.fullnames)
    p_to.idx = which(input$paramset_full_to  == paramsets.fullnames)
    
    indicator_trans_idx = which (input$outputlayer_transition == indicator.names)
    # indicator_trans_idx = 20
    # fname_from =  paste0("Data/", paramsets[p_from.idx], "/", input$scenario_from  , "/", input$scenario_from  , "-",runid_from, "-99-EU-Cell-", input$year_from, ".csv")
    # fname_to =  paste0("Data/", paramsets[p_to.idx], "/", input$scenario_to  , "/", input$scenario_to   , "-",runid_to, "-99-EU-Cell-", input$year_to, ".csv")
    
    
    # if (input$food != "Normal") { 
      fname_from =  paste0("Data/", input$foodprice_from, "/",  input$fooddemand_from, "/",paramsets[p_from.idx], "/", input$scenario_from  , "/", input$scenario_from  , "-",runid_from, "-99-EU-Cell-", input$year_from, ".csv")
      fname_to =  paste0("Data/", input$foodprice_to, "/",  input$fooddemand_to, "/",paramsets[p_to.idx], "/", input$scenario_to  , "/", input$scenario_to   , "-",runid_to, "-99-EU-Cell-", input$year_to, ".csv")
      
    # }
    
    
    # demand.colors = rich.colors(7)
    # # library(ggplot2)
    # # ggplot(aftcomp_dt)
    
    par(mfrow=c(1,1), mar = c(5.1, 4.1, 4, 1))
    
    # plot(target_years_aggcsv, aftcomp_perc_m[1,], type="l", ylab="%", col = aft.colors.fromzero[1], ylim=c(0, max(aftcomp_perc_m, na.rm = T) * 1.1), main = "AFT (17) composition changes")
    # 
    # for (a.idx in 2:8) { 
    #   lines(target_years_aggcsv, aftcomp_perc_m[a.idx,],   col = aft.colors.fromzero[a.idx])
    # }
    # legend("topright", aft.shortnames.fromzero, col = aft.colors.fromzero, lty=1, cex=0.7, bty="n")
    # 
    
    
    #### Transsition matrix 
    # @todo simply the processing by eliminating raster processing.. 
    # spdf.from = getSPDF(fname_from)
    # rs.from.LL <- stack(spdf.from)[[4:22]]
    # print(rs.from.LL)
    
    
    rs.from.LL = getRaster(fname_from, indicator_trans_idx)
    
    print(indicator_trans_idx)
    r.from = projectRaster(rs.from.LL[[1]], crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 2.5E4)
    
    # spdf.to = getSPDF(fname_to)
    # rs.to.LL <- stack(spdf.to)[[4:22]]
    
    rs.to.LL = getRaster(fname_to, indicator_trans_idx)
    
    r.to = projectRaster(rs.to.LL[[1]], crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 2.5E4)
    
    
    aft.old = getValues(r.from)
    aft.new = getValues(r.to)
    
    aft.tr.df = cbind(aft.old, aft.new)
    
    aft.tr.df = aft.tr.df[!is.na(rowSums(aft.tr.df)),]
    aft.tr.df = data.frame(cbind(1:nrow(aft.tr.df), aft.tr.df))
    colnames(aft.tr.df)[1] = "rowid"
    # Create the transition matrix that 
    # is the basis for the transition plot
    
    # print( table(aft.old))
    # print(table(aft.new))
    
    aft_tb_oldandnew = table(aft.old, aft.new)
    
    # non rectangular tables.. 
    if (dim(aft_tb_oldandnew)[1] != dim(aft_tb_oldandnew)[2]) {
      # if (nrow(aft_tb_oldandnew)!= nrow(aft_tb_oldandnew)){
      # print(aft_tb_oldandnew)
      print(dim(aft_tb_oldandnew)) 
      
      if (dim(aft_tb_oldandnew)[1] > dim(aft_tb_oldandnew)[2]) {
        
        missing_class = setdiff(rownames(aft_tb_oldandnew), colnames(aft_tb_oldandnew))
        
        aft_tb_oldandnew= cbind(rep(0, nrow(aft_tb_oldandnew)), aft_tb_oldandnew)
        
        
        colnames(aft_tb_oldandnew)[1] = missing_class
        
      } else {
        
        missing_class = setdiff(colnames(aft_tb_oldandnew), rownames(aft_tb_oldandnew))
        
        aft_tb_oldandnew= rbind(rep(0, ncol(aft_tb_oldandnew)), aft_tb_oldandnew)
        
        
        rownames(aft_tb_oldandnew)[1] = missing_class
      }
      
    }
    
    trn_mtrx <- with(aft.tr.df, aft_tb_oldandnew)
    # str(aft_tb_oldandnew)
    
    if (nrow(aft_tb_oldandnew)> 17 ) { 
      tr.colors = c("grey30", aft.colors.fromzero)
      tr_names = c("LazyFR", aft.names.fromzero)
    } else if (nrow(aft_tb_oldandnew)== 17){
      tr.colors =  aft.colors.fromzero 
      tr_names = c( aft.names.fromzero)
      
    } else {
      tr.colors =  aft.colors.8classes  
      tr_names = c(aft.names.8classes)
      
    }
    
    
    aft_old_prop = paste0( round(table(aft.old) / sum(aft.old, na.rm = T) * 100, 2  ), "%")
    aft.new_prop = paste0(round(table(aft.new) / sum(aft.new, na.rm = T)* 100, 2   ), "%")
 
    
    
    # Setup proportions
    box_prop <- cbind(aft_old_prop, aft.new_prop)
    # str(box_prop)
    par(mfrow=c(2,1))
    plot.new()
    
    transitionPlot(trn_mtrx,new_page=T,   fill_start_box =  tr.colors, arrow_clr =tr.colors, cex=1, color_bar = T, txt_start_clr = "black", txt_end_clr = "black", type_of_arrow = "simple", box_txt = box_prop,  overlap_add_width = 1, tot_spacing = 0.07, box_label = c(input$year_from, input$year_to)) # , min_lwd = unit(0.05, "mm"), max_lwd = unit(30, "mm"))
    
    plot.new()
    
     
    legend("center", tr_names, col = tr.colors, pch=15, cex=1)
    
    # aftcomp_8classes_perc_m = aftcomp_8classes_m/colSums(aftcomp_8classes_m) * 100
    # # barplot(height = aftcomp_8classes_perc_m, ylab="%", col = aft.colors.8classes, main = "AFT composition", names= target_years_aggcsv)
    # 
    # plot(target_years_aggcsv, aftcomp_8classes_perc_m[1,], type="l", xlab= "Year", ylab="EU-28 proportion (%)", col = aft.colors.8classes[1], ylim=c(0, max(aftcomp_8classes_perc_m, na.rm = T) * 1.1), main = "AFT (8) composition changes")
    # 
    # for (a.idx in 2:8) { 
    #   lines(target_years_aggcsv, aftcomp_8classes_perc_m[a.idx,],   col = aft.colors.8classes[a.idx])
    # }
    # legend("topright", aft.fullnames.8classes, col = aft.colors.8classes, lty=1, cex=0.7, bty="n")
    # 
    # barplot(height = demand_m[1:7,], beside=T, ylab="Service Supply", col = serviceColours, main = "Service Supply", names= demand_dt$Tick)
    # legend("topright", legend = serviceNames, fill=serviceColours, cex=0.7, bty="n")
    # 
    # barplot(height = demand_m[8:14,], beside=T, ylab="Demand", col = serviceColours, main = "Service Demand", names= demand_dt$Tick)
    # barplot(height = (demand_m[8:14,] - demand_m[1:7,]) , beside=T, ylab="Demand - Supply", col = serviceColours, main = "S/D gap", names= demand_dt$Tick)
    
    
  })
  
  output$Tab1_MapPane <- renderLeaflet({
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
    proxy <- leafletProxy("Tab1_MapPane", data = providernew())
    proxy %>% clearTiles() %>% addProviderTiles(input$background, options = providerTileOptions(noWrap = TRUE), group = "TileLayer")
    # proxy %>% clearControls() 
    
  })
  
  
  # should be managed in its own observer.
  observe({
    dt = rnew()
    # print(which (input$indicator == indicator.names))
    
    
    proxy <- leafletProxy("Tab1_MapPane", data =rnew())
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
  #   leafletProxy("Tab1_MapPane", data = filteredData()) %>%
  #     clearShapes() %>%
  #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
  #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
  #     )
  # })
  #
  
  #
  # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("Tab1_MapPane")
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
      
      fname_changed =  paste0("CRAFTY-EU_", paramsets[p.idx], "_", input$scenario, "_", "FoodPrice_", input$foodprice, "_", "MeatDemand_", input$fooddemand, "_", input$year, "_", input$outputlayer, ".tif")
      
      # fname_changed      
    },
    content = function(file) {
      # runid = 0 
      
      runid = which(scenario.names == input$scenario) - 1
      indicator_idx = which (input$outputlayer == indicator.names)
      p.idx = which(input$paramset_full == paramsets.fullnames)
      
      # fname_changed =  paste0("Data/",paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
      
      # if (input$food != "Normal") { 
        
        fname_changed =   paste0("Data/", input$foodprice, "/",  input$fooddemand, "/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
        
      # }
      
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