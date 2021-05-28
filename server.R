#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.


library(shiny)
source("RScripts/Functions_CRAFTY_WEB.R")

accessDropbox()


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
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
    p.idx = which(input$paramset_full == paramsets_fullnames)
    
    paste0("Simulated ", input$outputlayer, " in ", input$year, " with the ", input$paramset_full, " parameters and ",  input$scenario, " climate scenario." )
  })
  
  
  # runinfo_ts <- reactive({
  #   p.idx = which(input$paramset_full_ts == paramsets_fullnames)
  #   
  #   paste0("Simulation with the ", input$paramset_full_ts, " parameters and ",  input$scenario_ts, " climate scenario." )
  # })
  # 
  
  
  observeEvent(input$deleteCache, {
    session$sendCustomMessage(type = 'message',
                              message = 'deleteCache')
    print("delete cache")
    unlink(paste0(path_rastercache, "/*"), recursive = T)
    unlink(paste0(path_filecache, "/*"), recursive = T)
    
  })
  
  
  output$PaneRuninfo <- renderText({
    runinfo()
  })
  
  # output$PaneRuninfo_ts <- renderText({
  #   # runinfo_ts()
  # })
  # 
  
  output$ReferenceToParameters <- renderText({
    "<br/>Behavioural parameter set 1 is the default from which main results are derived; in this setup agents respond directly to benefit values with no additional individual or typological behaviour. In parameter set 2, giving-up and giving-in thresholds are altered to introduce abandonment of land when benefit values fall below the giving-up threshold value, and resistance to change unless a competing land use has an additional benefit value of at least the giving-in threshold. Intensive land use agents are parameterised to be less tolerantof low benefit values, and more willing to switch to a land use with higher benefit values. <p/>

Please refer to the following paper to understand the behavioural model used in CRAFTY:

<p/>
<i>Brown, C., Murray-Rust, D., Van Vliet, J., Alam, S. J., Verburg, P. H., & Rounsevell, M. D. (2014). Experiments in globalisation, food security and land use decision making. PLoS ONE, 9(12), 1–24. </i> <a href='https://doi.org/10.1371/journal.pone.0114213'>https://doi.org/10.1371/journal.pone.0114213</a>

<p/>
Please see the further details of the parameters in Table A4 of the following paper: <p/>
<i>Brown, C., Seo, B., & Rounsevell, M. (2019). Societal breakdown as an emergent property of large-scale behavioural models of land use change. Earth System Dynamics, (accepted), 1–49.</i> <a href='https://doi.org/10.5194/esd-2019-24'>https://doi.org/10.5194/esd-2019-24</a>"
  })
  # In parameter set 3, individual agents differ from one another in terms of their abilities to produce different ecosystem services, and their giving-up and giving-in thresholds. Parameter sets 4 and 5 replicate parameter sets 2 and 3 respectively, but with larger values for thresholds and variations.
  
  
  output$ReferenceToAFT <- renderText({
    "<br/>Please see the detailed decription of the scenarios in Table 2 (pp. 27-28) of the following paper: <p/>
<i><small>Brown, C., Seo, B., & Rounsevell, M. (2019). Societal breakdown as an emergent property of large-scale behavioural models of land use change. Earth System Dynamics Discussions, (May), 1–49.</i> <a href='https://doi.org/10.5194/esd-2019-24'>https://doi.org/10.5194/esd-2019-24</a></small>"
  })
  
  output$ReferenceToScenarios <- renderText({
    "<br/>Please see the detailed decription of the scenarios in Table 2 (pp. 27-28) of the following paper: <p/>
<i><small>Brown, C., Seo, B., & Rounsevell, M. (2019). Societal breakdown as an emergent property of large-scale behavioural models of land use change. Earth System Dynamics Discussions, (May), 1–49.</i> <a href='https://doi.org/10.5194/esd-2019-24'>https://doi.org/10.5194/esd-2019-24</a></small>"
  })
  
  
  
  rnew_input <- reactive({
    print("rnew_input called")
    
    
    
    p.idx = which(input$paramset_full == paramsets_fullnames)
    
    fname_changed = getFname(input$version,  paramsets[p.idx], input$scenario, input$year)
    
    r_changed = getRaster(fname_changed, band.name = input$inputlayer, resolution = RESOLUTION_WEB, location = location_UK)
    
    return(r_changed)
  } )
  
  providernew <- reactive({
    print("providernew called")
    
    input$background
  })
  # providernew_sn <- reactive({
  #   input$background_sn
  # })
  # 
  # points <- eventReactive(input$year, {
  # cbind(rnorm(1) * 2 + 13, rnorm(1) + 48)
  # }, ignoreNULL = FALSE)
  
  
  # Combine the selected variables into a new data frame
  # selectedData <- reactive({
  #   
  #   p.idx = which(input$paramset_full == paramsets_fullnames)
  #   
  #   fname_changed =  paste0("Data/",  input$foodprice, "/", paramsets[p.idx], "/",  input$scenario, "/", input$fooddemand, "/",  input$scenario, "-",runid, "-99-UK-Cell-", input$year, ".csv")
  #   spdf_changed = getSPDF_UK(fname_changed)
  #   
  #   target_val = spdf_changed[4:22]
  #   
  #   # hist(as.numeric(target_val$Service.Meat))
  #   return(target_val)
  # })
  
  
  output$Tab1_StatisticsPane <- renderPlot({
    print("draw stat pane")
    
    target_data = rnew()
    
    # runid = which(scenario_names == input$scenario) - 1
    # p.idx = which(input$paramset_full == paramsets_fullnames)
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
  
  
  
  output$Tab1_AFTTablePane <- renderDataTable(
    {
      print("draw AFT pane")
      
      
      # p.idx = which(input$paramset_full == paramsets_fullnames)
      # 
      # # foldername_tmp = ("Tables/agents/BehaviouralBaseline/Baseline")
      # foldername_tmp = paste0("Tables/agents/", paramsets[p.idx], "/", input$scenario)
      # 
      # aftparams_df = sapply(aft_shortnames_fromzero[-length(aft_shortnames_fromzero)], FUN = function(x) read.csv(paste0(foldername_tmp, "/AftParams_", x, ".csv"))) %>% t
      # 
      # aftparams_df = data.frame(aftparams_df)
      # aftparams_df$productionCsvFile = NULL
      # p_name = paste0("Tables/Paramset", p.idx, ".csv")
      # 
      
      AFT_tb = read.csv("Tables/AFT_Names_UK.csv")
      # 
      
      # fname_changed =  paste0("Data/",  paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-",seedid,"-EU-Cell-", input$year, ".csv")
      
      # spdf_changed = getSPDF(fname_changed)
      # target_val = spdf_changed[4:22]
      # 
      # str(getValues(target_data))
      # tb1 =     table(getValues(target_data))
      # print(tb1)
      
      DT::datatable(AFT_tb[,c("Name", "Description", "Group", "Type")], options= list(paging = FALSE),  editable = F) 
      # %>%  DT::formatStyle(columns = colnames(.), fontSize = '50%')
      # %>% formatStyle(
      #   'Name',
      #   backgroundColor =  aft_colors_fromzero_17
      # )
    })
  
  output$Tab1_BehaviouralTablePane <- renderDataTable(
    {
      print("draw behavioural pane")
      
      
      
      p.idx = which(input$paramset_full == paramsets_fullnames)
      
      # foldername_tmp = ("Tables/agents/BehaviouralBaseline/Baseline")
      foldername_tmp = paste0("Tables/agents/", paramsets[p.idx], "/", input$scenario)
      
      aftparams_df = sapply(aft_shortnames_fromzero[-length(aft_shortnames_fromzero)], FUN = function(x) read.csv(paste0(foldername_tmp, "/AftParams_", x, ".csv"))) %>% t
      
      aftparams_df = data.frame(aftparams_df)
      aftparams_df$productionCsvFile = NULL
      # p_name = paste0("Tables/Paramset", p.idx, ".csv")
      # 
      # p_tb = read.csv(p_name)
      # 
      
      # fname_changed =  paste0("Data/",  paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-",seedid,"-EU-Cell-", input$year, ".csv")
      
      # spdf_changed = getSPDF(fname_changed)
      # target_val = spdf_changed[4:22]
      # 
      # str(getValues(target_data))
      # tb1 =     table(getValues(target_data))
      # print(tb1)
      
      DT::datatable(aftparams_df, options= list(paging = FALSE),  editable = F) 
      # %>%  DT::formatStyle(columns = colnames(.), fontSize = '50%')
    })
  
  output$Tab1_ProductionTablePane <- renderDataTable(
    {
      print("draw production pane")
      
      p.idx = which(input$paramset_full == paramsets_fullnames)
      
      foldername_tmp = ("Tables/production/Baseline")
      foldername_tmp = paste0("Tables/production/", input$scenario)
      
      productionparams_l = lapply(aft_shortnames_fromzero[-length(aft_shortnames_fromzero)], FUN = function(x) read.csv(paste0(foldername_tmp, "/", x, ".csv"))) 
      
      a_idx = 1 
      x = productionparams_l[[a_idx]]
      
      
      
      
      colnames(x)[1] = "Service"
      DT::datatable(x, options= list(paging = F),  editable = F, rownames = F, caption = aft_names_fromzero[a_idx]) 
      # %>%  DT::formatStyle(columns = colnames(.), fontSize = '50%')
    })
  
  
  # observe({
  #   # dt.plot = selectedData()
  #   input$
  #   par(mar = c(5.1, 4.1, 4, 1))
  #   plot(selectedData()@data[, input$outputlayer], main=input$indicator)
  #   
  # })
  
  output$Tab2_TimeseriesPlotPane <- renderPlot(height = "auto", width = 1000, res = 96, {
    print("draw timeseries pane")
    
    p.idx = which(input$paramset_full == paramsets_fullnames)
    
    
    scenario_tmp = "RCP6_0-SSP3"
    # scenario_tmp = "RCP4_5-SSP4"
    # scenario_tmp = "Baseline"
    paramset_tmp = "Thresholds"
    
    scenario_tmp = input$scenario
    paramset_tmp = paramsets[p.idx]
    
    # File names
    
    
    if (!str_detect(scenario_tmp, "SSP3")) {
      # aft composition
      aft_csvname_changed = fs::path_expand(paste0(version_prefix[match(input$version, version_names)], "/",paramset_tmp , "/", scenario_tmp, "/",  scenario_tmp, "-", runid, "-99-UK-AggregateAFTComposition.csv"))
      
      # supply and demand files
      demand_csvname_changed = fs::path_expand(paste0(version_prefix[match(input$version, version_names)], "/", paramset_tmp, "/", scenario_tmp, "/", scenario_tmp, "-", runid, "-99-UK-AggregateServiceDemand.csv"))
      
      aftcomp_dt = getCSV(aft_csvname_changed, location = location_UK)
      demand_dt = getCSV(demand_csvname_changed, location = location_UK)
      
    } else { 
      
      # aft composition
      aft_csvname_changed_v = fs::path_expand(paste0(version_prefix[match(input$version, version_names)], "/", paramset_tmp, "/", scenario_tmp, "/",  scenario_tmp, "-", runid, "-99-", region_names, "-AggregateAFTComposition.csv"))
      
      aftcomp_dt_l = lapply(aft_csvname_changed_v, FUN = function(x) getCSV(x, location = location_UK))
      
      aftcomp_dt = cbind(aftcomp_dt_l[[1]][,c("Tick", "Region")],  Reduce("+", lapply(aftcomp_dt_l, FUN = function(x) x[,-c(1:2)])))
      
      
      
      # supply and demand files
      demand_csvname_changed_v = fs::path_expand(paste0(version_prefix[match(input$version, version_names)], "/", paramset_tmp, "/", scenario_tmp, "/", scenario_tmp, "-", runid, "-99-", region_names, "-AggregateServiceDemand.csv"))
      demand_dt_l = lapply(demand_csvname_changed_v, FUN = function(x) getCSV(x, location = location_UK))
      
      rem_col_idx = match(c("Tick", "Region"), colnames(demand_dt_l[[1]]))
      
      demand_dt = cbind( Reduce("+", lapply(demand_dt_l, FUN = function(x) x[,-rem_col_idx ])), Region= "UK", Tick = demand_dt_l[[1]][,c("Tick")])
      
      
    }
    
    
    
    # mean capital level
    capital_csvname_changed = fs::path_expand(paste0(scenario_tmp, "-", runid, "-", seedid, "-UK-AggregateCapital.csv"))
    
    #@todo read csv files
    
    capital_scene_tmp = read.csv(paste0("Tables/Summary/", capital_csvname_changed))
    

    aftcomp_dt_org = aftcomp_dt
    # reclassify
    aftcomp_dt[,"AFT.IAfood"] = aftcomp_dt[,"AFT.IAfood"] + aftcomp_dt[,"AFT.IAfodder"]
    aftcomp_dt[,"AFT.IAfodder"] = NULL
    
    aftcomp_dt[,"AFT.MW"] = aftcomp_dt[,"AFT.PNB"] + aftcomp_dt[,"AFT.PNC"] + aftcomp_dt[,"AFT.PNNB"] + aftcomp_dt[,"AFT.PNNC"]
    aftcomp_dt[, c("AFT.PNB", "AFT.PNC","AFT.PNNB","AFT.PNNC")] = NULL
    
    colnames(aftcomp_dt)[  colnames(aftcomp_dt) == "AFT.IAfood"] = "AFT.IA"
    colnames(aftcomp_dt)[  colnames(aftcomp_dt) == "AFT.MW"] = "AFT.PW"
    
    
    aftcomp_m = t(as.matrix(sapply(aftcomp_dt[, -c(1,2)] , FUN = function(x) as.numeric(as.character(x)))))
    

    
    # process csv files
    demand_m = t(as.matrix(sapply(demand_dt[, -c(ncol(demand_dt) - 1:0)] , FUN = function(x) as.numeric(as.character(x)))))
    
    
    str(demand_m)
    ncold = nrow(demand_m)
    idx_dem_st = ((ncold/2)+1) : ncold
    idx_sup_st = 1:(ncold/2) 
    
    shortfall_m = ((demand_m[idx_dem_st,] - demand_m[idx_sup_st,]) / demand_m[idx_dem_st,] ) * 100
    shortfall_m[!is.finite(shortfall_m)] = NA
    # demand.colors = rich.colors(7)
    # # library(ggplot2)
    # # ggplot(aftcomp_dt)
    
    
    
    
    par(mfrow=c(4,2), mar = c(5.1, 5.1, 2, 0)  + c(0,0,0,10), oma=c(0,0,0,0))
    
    # par(mfrow=c(4,2), xpd = T, mar = par()$mar + c(0,0,0,7))
    # par( mar = c(5.1, 4.1, 4, 0)  + c(0,0,0,8))
    
    
    # AFT changes
    n_cell_unmanaged = n_cell_total - colSums(aftcomp_m) 
    
    aftcomp_m = rbind(aftcomp_m, n_cell_unmanaged)
    
    rownames(aftcomp_m)
    
    aftcomp_perc_m =  aftcomp_m/n_cell_total * 100
    
    # str(aftcomp_perc_m)
    
    
    plot(aftcomp_dt$Tick, aftcomp_perc_m[1,], type="l", xlab= "Year", ylab="Proportion (%)", col = aft_group_colors[1], ylim=c(0, max(aftcomp_perc_m, na.rm = T) * 1.1), main = "AFT composition changes", xaxt="n", lty= aft_lty_ts)
    axis(side=1, at = target_years_other, labels = target_years_other)
    
    for (a.idx in 2:nrow(aftcomp_perc_m)) {
      lines(aftcomp_dt$Tick, aftcomp_perc_m[a.idx,], col = aft_group_colors[a.idx], lty=aft_lty_ts[a.idx])
    }
    legend("topright", aft_group_shortnames, col = aft_group_colors, lty=aft_lty_ts, cex=LEGEND_CEX, bty="n", xpd = TRUE, inset=c(LEGEND_MAR,0), lwd=1.5)
    
    
    # par( mar = c(5.1, 4.1, 4, 1)  + c(0,0,0,8))
    # 
    # ### Plotting number of changed pixels
    # print("changed pixels")
    # cnp_path =  paste0("Tables/ChangedPixelNo/",input$foodprice_ts, "/", input$fooddemand_ts, "/",paramsets[p.idx], "/", input$scenario_ts, "_ChangedPixelNo.xlsx")
    # 
    # cnp_dt = readxl::read_excel(cnp_path, sheet = 1)
    # cnp_v = as.numeric(unlist(cnp_dt))
    # cnp_max = max(c(500, cnp_v), na.rm = T)
    # 
    # 
    # 
    # plot(target_years_other[c(2:8)],cnp_v, ylim=c(0, cnp_max), type="l", xlab= "Year", ylab="Number of pixels changed", col = "blue", main = "Magnitude of land use change", xaxt ="n")
    # 
    # axis(side=1, at = target_years_other[-1], labels = target_years_other[-1])
    # 
    
    
    
    ### Plotting service supply and demand
    
    
    supply_m_norm = (demand_m[idx_sup_st,] / demand_m[idx_sup_st,1] - 1) * 100
    demand_m_norm = (demand_m[idx_dem_st,] / demand_m[idx_dem_st,1]-1) * 100
    
    supdem_range = range(cbind(supply_m_norm))
    print(supdem_range)
    
    y_lim_max = max(3, max(abs(supdem_range)) * 1.2)
    y_lim_v = c(-y_lim_max, y_lim_max)
    # barplot(height = supply_m_norm, beside=T, ylab= "Relative to 2020's supply (%)", ylim= y_lim_v, col = serviceColours, main = "Service Supply", names= demand_dt$Tick, border=NA)
    # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
    
    plot(demand_dt$Tick, supply_m_norm[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Relative to 2020's supply (%)",  main = "Service Supply", las=1, xaxt="n" )
    axis(side=1, at = target_years_other, labels = target_years_other)
    # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    abline(h = 0, lty=2)
    
    for (a.idx in c(1:nrow(supply_m_norm))) {
      lines(demand_dt$Tick, supply_m_norm[a.idx,],   col = serviceColours[a.idx])
    }
    
    legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    
    
    
    
    dem_range = range(cbind(demand_m_norm))
    print(dem_range)
    
    y_lim_max = max(5, max(abs(dem_range)) * 1.2)
    y_lim_v = c(max(-100, -y_lim_max), y_lim_max)
    
    # barplot(height = demand_m_norm, beside=T, ylab="Relative to 2020's supply (%)", col = serviceColours, main = "Service Demand", names= demand_dt$Tick, ylim=y_lim_v, border=NA)
    
    plot(demand_dt$Tick, demand_m_norm[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Relative to 2020's demand (%)",  main = "Service Demand", las=1, xaxt="n" )
    axis(side=1, at = target_years_other, labels = target_years_other)
    # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    abline(h = 0, lty=2)
    abline(h = -100, lty=2)
    
    for (a.idx in c(1:nrow(demand_m_norm))) {
      lines(demand_dt$Tick, demand_m_norm[a.idx,],   col = serviceColours[a.idx])
    }
    
    # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
    legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    
    
    #### SDGAP
    
    sdgap  = (demand_m[idx_dem_st,] - demand_m[idx_sup_st,])
    # sdgap = (sdgap /demand_m[idx_sup_st,1]  ) * 100
    
    
    sdgap_range = range(sdgap, na.rm=T)
    y_lim_max = max(1, max(abs(sdgap_range)) * 1.2,  demand_m[idx_sup_st,1]*0.3) 
    
    y_lim_v = c(-y_lim_max, y_lim_max)
    
    
    # barplot(height =  sdgap, beside=T, ylab="Demand - Supply", col = serviceColours, main = "S/D gap", names= demand_dt$Tick, ylim = y_lim_v, border=NA)
    # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
    
    plot(demand_dt$Tick, sdgap[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Demand - Supply (original unit)",  main = "S/D gap (=D-S)", las=1, xaxt="n", mgp=c(4,1,0))
    axis(side=1, at = target_years_other, labels = target_years_other)
    # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    abline(h = 0, lty=2)
    
    for (a.idx in c(1:nrow(sdgap))) {
      lines(demand_dt$Tick, sdgap[a.idx,],   col = serviceColours[a.idx])
    }
    legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    
    
    
    
    ###### PRODUCTION SHORTFALL 
    
    shortfall_range = range(shortfall_m[,], na.rm = T)
    range(shortfall_m)
    shortfall_max = max(5, max(abs(shortfall_range)) * 1.2)
    shortfall_intv = floor(shortfall_max / 100) * 10
    shortfall_intv = max(1, shortfall_intv)
    
    
    # print(shortfall_intv)
    plot(demand_dt$Tick, shortfall_m[1,], type="l", col = serviceColours[1], ylim=c(-shortfall_max,shortfall_max), xlab="Year", ylab="",  main = "Production shortfall", las=1, xaxt="n" )
    # title(ylab="Production shortfall (%)", mgp=c(3, 1, 0))
    mtext(side = 2, text ="Production shortfall (%)", line = 4, cex=0.8)
    
    axis(side=1, at = target_years_other, labels = target_years_other)
    # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    abline(h = 0, lty=2)
    
    for (a.idx in c(1:nrow(shortfall_m))) {
      lines(demand_dt$Tick, shortfall_m[a.idx,],   col = serviceColours[a.idx])
    }
    
    legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    
    
    ########## Mean capital levels
    
    
    
    # str(capital_scene_tmp)
    if (nrow(capital_scene_tmp) > 1) { 
      capital_scene_tmp[,-1] = sapply(1:length(baseline_capital_tmp[-1]), FUN = function(x) capital_scene_tmp[,x+1] /  baseline_capital_tmp[x+1])
      
      
      ylim_cap = max(sapply(capital_scene_tmp[,-1], max, na.rm=T))
      
      plot(capital_scene_tmp$Tick, capital_scene_tmp[,2] * 100, type="l", col = capital_colours[1], ylim=c(0, ylim_cap * 100), xlab="Year", ylab="Relative to 2020 (%)",  main = "Mean input capitals changes", las=1, xaxt="n", lwd=1.5)
      # title(ylab="Production shortfall (%)", mgp=c(3, 1, 0))
      # mtext(side = 2, text ="Production shortfall (%)", line = 4, cex=0.8)
      
      axis(side=1, at = target_years_other, labels = target_years_other)
      # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
      abline(h = 0, lty=2)
      
      for (a.idx in c(3:ncol(capital_scene_tmp))) {
        lines(capital_scene_tmp$Tick, capital_scene_tmp[,a.idx] * 100,   col = capital_colours[a.idx-1])
      }
      
      legend("topright", legend = capital_names$Capital[], col=capital_colours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
      
      
    } else {   # baseline 
      plot.new()
      capital_scene_tmp$X = 2020
      colnames(capital_scene_tmp) = c("Tick", capital_names$Capital)
      capital_scene_tmp[-1] = capital_scene_tmp[-1] / capital_scene_tmp[-1] #all 1 
    }
    
    
    ######### FOOD PRODUCTION 
    
    # Food as a sum of food crops, fodder crops, GF milk & GF meat, as a percentage of the baseline total? Maybe we can convert them to real quantities later...
    
    # food_names = C("Food.crops", "Fodder.crops", "GF.redMeat", "GF.milk")  
    
    food_production = colSums(demand_m[c(1,2,3,13),])
    
    food_production = ((food_production / food_production[1]) -1) * 100  
    
    food_range = range(food_production)
    print(food_range)
    
    y_lim_max = max(3, max(abs(food_range)) * 1.2)
    y_lim_v = c(max(-100, -y_lim_max), y_lim_max)
    
    # barplot(height = demand_m_norm, beside=T, ylab="Relative to 2020's supply (%)", col = serviceColours, main = "Service Demand", names= demand_dt$Tick, ylim=y_lim_v, border=NA)
    
    plot(demand_dt$Tick, food_production, type="l", col = "black", ylim=y_lim_v, xlab="Year", ylab="Relative to 2020's production (%)",  main = "Food production", las=1, xaxt="n" )
    axis(side=1, at = target_years_other, labels = target_years_other)
    # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    abline(h = 0, lty=2)
    abline(h = -100, lty=2)
 
    
    # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
    # legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    
    
    
    ######### INTENSITY 
 
    aftcomp_org_m = t(as.matrix(sapply(aftcomp_dt_org[, -c(1,2)] , FUN = function(x) as.numeric(as.character(x)))))
    
    print(rownames(aftcomp_org_m))
    impact_idx = match(rownames(aftcomp_org_m), paste0("AFT.", impact_coeff$AFT))
    str(impact_idx)
    
    intensity_v = rowSums(sapply(1:nrow(aftcomp_org_m), FUN = function(x) aftcomp_org_m[x,] * as.numeric(impact_coeff$Impact[impact_idx[x]])), na.rm=F)
    
    print(intensity_v)
    str(intensity_v)
    
    intensity_rel_v = intensity_v / intensity_v[1] * 100 
    
     
    intensity_range = range(intensity_rel_v, na.rm = T)
    print(intensity_range)
    
    y_lim_max = max(3, max(abs(intensity_range)) * 1.1)
    y_lim_min = min(intensity_range) * 0.9 
    y_lim_v = c(y_lim_min, y_lim_max)
    
    # barplot(height = demand_m_norm, beside=T, ylab="Relative to 2020's supply (%)", col = serviceColours, main = "Service Demand", names= demand_dt$Tick, ylim=y_lim_v, border=NA)
    
    plot(demand_dt$Tick, intensity_rel_v, type="l", col = "black", ylim=y_lim_v,   xlab="Year", ylab="Relative to 2020's intensity (%)",  main = "Land Use Intensity", las=1, xaxt="n" )
    axis(side=1, at = target_years_other, labels = target_years_other)
    # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    abline(h = 0, lty=2)
    abline(h = 100, lty=2)
    
     
    
    
    
    
    # print("fragmentation statistics")
    # 
    # 
    # ### fragmentation statistics
    # frac_path =  paste0("Tables/FragStats/",input$foodprice_ts, "/", input$fooddemand_ts, "/",paramsets[p.idx], "/", input$scenario_ts, "_FragStats.xlsx")
    # frac_dim = as.matrix( readxl::read_excel(frac_path, sheet = 1))
    # 
    # 
    # frac_dim_norm =  frac_dim / frac_dim[,1] * 100  - 100
    # 
    # frac_v = as.numeric(colMeans(frac_dim_norm))
    # frac_max = max(c(abs(frac_v)), na.rm = T) * 1.2
    # # barplot(t(frac_dim), beside=T)
    # plot(x=target_years_other, y = frac_v, ylim=c(-frac_max, frac_max), type="l",xlab="Year", ylab="Relative to 2016 (%)", col= "blue", main = "Avg. Fractal Dimension")
    # abline(h = 0, lty=2)
    
    
  })
  
  
  output$Tab3_TransitionPlotPane <- renderPlot(height = PLOT_HEIGHT, res = 96, {
    
    
    p_from.idx = which(input$paramset_full_from == paramsets_fullnames)
    p_to.idx = which(input$paramset_full_to  == paramsets_fullnames)
    
    # fname_from = paste0(input$version_tr, "/BehaviouralBaseline/Baseline/Baseline-0-99-UK-Cell-2020.csv")
    # fname_to =  paste0(input$version_tr, "/BehaviouralBaseline/RCP4_5-SSP4/RCP4_5-SSP4-0-99-UK-Cell-2080.csv")
    
    fname_from =  getFname(input$version_from, paramsets[p_from.idx], input$scenario_from, year =  input$year_from)
    fname_to   =  getFname(input$version_to, paramsets[p_to.idx], input$scenario_to, year =  input$year_to)
    
    
    #### Transition matrix
 
    csv_from = getCSV(fname_from, location = location_UK)
    csv_to = getCSV(fname_to, location = location_UK)
    
    # already 1 km grid in projected space (for the UK model)
    aft_old = csv_from$LandUseIndex
    aft_new = csv_to$LandUseIndex
    
    
    # deal with -1 
    aft_old[aft_old==-1] = 16
    aft_new[aft_new==-1] = 16 
    
    # deal with zero
    aft_old = aft_old + 1
    aft_new = aft_new + 1
    
    # reclassify
    aft_old[aft_old==6] = 5
    aft_old[aft_old %in% c(8, 10:13)] = 8
    
    aft_new[aft_new==6] = 5
    aft_new[aft_new %in% c(8, 10:13)] = 8
    
    

     
    
    
    aft_tr.df = cbind(aft_old, aft_new)
    
    aft_tr.df = aft_tr.df[!is.na(rowSums(aft_tr.df)),]
    aft_tr.df = data.frame(cbind(1:nrow(aft_tr.df), aft_tr.df))
    colnames(aft_tr.df)[1] = "rowid"
    # Create the transition matrix that
    # is the basis for the transition plot
    
    # print( table(aft_old))
    # print(table(aft_new))
    
    
    
    aft_old_f = factor(aft_old, levels = c(1:5, 7:9, 14:17))
    aft_new_f = factor(aft_new, levels = c(1:5, 7:9, 14:17))
    
    aft_tb_oldandnew = table(aft_old_f, aft_new_f)
   
    trn_mtrx <- with(aft_tr.df, aft_tb_oldandnew)
    str(aft_tb_oldandnew)
    
    # reduce 
    tr.colors =  aft_group_colors
    tr_names =  aft_group_names
    
    
    aft_old_tb = rowSums(trn_mtrx)
    aft_new_tb = colSums(trn_mtrx)
    
     
    aft_old_prop = paste0(round(aft_old_tb / sum(aft_old_tb, na.rm = T) * 100, 3  ), "%")
    aft_new_prop = paste0(round(aft_new_tb / sum(aft_new_tb, na.rm = T)* 100, 3   ), "%")
    
    
    
    # Setup proportions
    box_prop <- cbind(aft_old_prop, aft_new_prop)
    # str(box_prop)
    # par(mfrow=c(1,1), mar = c(5.1, 4.1, 4, 1))
    par(mfrow=c(1,1))
    plot.new()
    
    transitionPlot(trn_mtrx,new_page=T, fill_start_box =  tr.colors, arrow_clr =tr.colors, cex=1, color_bar = T, txt_start_clr = "black", txt_end_clr = "black", type_of_arrow = "simple", box_txt = box_prop, overlap_add_width = 1, tot_spacing = 0.07, min_lwd = unit(0.005, "mm"), max_lwd = unit(10, "mm"),  box_label = c(input$year_from, input$year_to),)
     
    
    legend("center", tr_names, col = tr.colors, pch=15, cex=0.9)
    
    # aftcomp_8classes_perc_m = aftcomp_8classes_m/colSums(aftcomp_8classes_m) * 100
    # # barplot(height = aftcomp_8classes_perc_m, ylab="%", col = aft_colors.8classes, main = "AFT composition", names= target_years_aggcsv)
    #
    # plot(target_years_aggcsv, aftcomp_8classes_perc_m[1,], type="l", xlab= "Year", ylab="EU-28 proportion (%)", col = aft_colors.8classes[1], ylim=c(0, max(aftcomp_8classes_perc_m, na.rm = T) * 1.1), main = "AFT (8) composition changes")
    #
    # for (a.idx in 2:8) {
    #   lines(target_years_aggcsv, aftcomp_8classes_perc_m[a.idx,],   col = aft_colors.8classes[a.idx])
    # }
    # legend("topright", aft_fullnames_8classes, col = aft_colors.8classes, lty=1, cex=0.7, bty="n")
    #
    # barplot(height = demand_m[1:7,], beside=T, ylab="Service Supply", col = serviceColours, main = "Service Supply", names= demand_dt$Tick)
    # legend("topright", legend = serviceNames, fill=serviceColours, cex=0.7, bty="n")
    #
    # barplot(height = demand_m[8:14,], beside=T, ylab="Demand", col = serviceColours, main = "Service Demand", names= demand_dt$Tick)
    # barplot(height = (demand_m[8:14,] - demand_m[1:7,]) , beside=T, ylab="Demand - Supply", col = serviceColours, main = "S/D gap", names= demand_dt$Tick)
    
    
  })
  
  # output$Tab1_MapPane <- renderLeaflet({
  #   r_dummy  = raster(extent(r.default))
  #   r_dummy = setValues(r_dummy, values = 0)
  #   proj4string(r_dummy) = proj4string(r.default)
  #   
  #   leaflet() %>%
  #     clearImages() %>% clearControls() %>%
  #     #addTiles()
  #     addProviderTiles(providers$OpenStreetMap.Mapnik, # Esri.WorldImagery
  #                      options = providerTileOptions(noWrap = TRUE), group = "TileLayer"
  #     ) %>%
  #     # %>%
  #     # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  #     addRasterImage(r_dummy, project = FALSE, group="OutputLayer") # , colors = aft_pal, maxBytes = 4 * 1024 * 1024) %>%
  #   #  addLegend( pal = aft_pal, values = 1:17, labels = aft_names_fromzero, title = "AFT")
  #   # addLegend(colors = col2hex(as.character(aft_colors.fromzero)), labels = aft_names_fromzero, title = indicator_names[17])
  #   #%>%
  #   # addMarkers(data = points())
  # })
  
  output$Tab1_MapPane <- renderLeaflet({
    print("draw mappane 1")
    
    leaflet() %>%
      clearImages() %>% clearControls() %>%
      #addTiles()
      addProviderTiles(providers$OpenStreetMap.Mapnik, # Esri.WorldImagery
                       options = providerTileOptions(noWrap = TRUE), group = "TileLayer"
      ) %>%   
      fitBounds(ext[1], ext[3], ext[2], ext[4] )
    # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    # addRasterImage(r_dummy, project = FALSE, group="OutputLayer", opacity=0, colors = aft_pal, maxBytes = 4 * 1024 * 1024) %>%
    # addLegend(pal = aft_pal, values = 1:n_aft, labels = aft_names_fromzero, title = "AFT")
    # addLegend(colors = col2hex(as.character(aft_colors.fromzero)), labels = aft_names_fromzero, title = indicator_names[17])
    #%>%
    # addMarkers(data = points())
  })
  # 
  # 
  # 
  # 
  observe({
    print("draw tile layer")
    
    # dt = providernew()
    proxy <- leafletProxy("Tab1_MapPane", data = providernew())
    proxy %>% clearTiles() %>% addProviderTiles(input$background, options = providerTileOptions(noWrap = TRUE), group = "TileLayer")
    # proxy %>% clearControls()
  })
  # 
  
  # should be managed in its own observer.
  observe({
    print("redraw output layer")
    dt = rnew()
    # print(which (input$indicator == indicator_names))
    
    proxy <- leafletProxy("Tab1_MapPane", data =dt)
    proxy %>% clearImages() %>% clearControls()
    
    # touches
    input$background
    
    # Layers control
    proxy %>% addLayersControl(
      # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      baseGroups = c("ModelResult",  "Basemap"),
      # overlayGroups = c("TileLayer"),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    
    # proxy %>% hideGroup("ModelInput")
    
    # print(dt)
    
    if (input$outputGroup == "print_out") { 
      
      # Add output layer
      
      if (input$outputlayer == "LandUseIndex") {  # land use index
        
        proxy %>% addRasterImage(dt, project = FALSE, colors = aft_pal, group = "ModelResult"
                                 , opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
        if (input$legend) {
          
          proxy %>% addLegend(colors = col2hex(as.character(aft_colors_fromzero)), labels = aft_shortnames_fromzero, title = paste0("Output: ", input$outputlayer),group = "ModelResult", opacity = input$alpha)
        }
      } else {
        dt.v = getValues(dt)
        dt.rng = range(dt.v, na.rm = T)
        print(dt.rng)
        pal = colorNumeric(input$colors,reverse = input$InvertColour, domain = dt.rng,  na.color = "transparent")
        
        proxy %>%
          addRasterImage(dt, project = FALSE, colors =pal, group = "ModelResult", method = "bilinear"
                         , opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
        if (input$legend) { proxy %>%
            addLegend(pal = pal, values = quantile(dt.v, probs=seq(1, 0, -0.05), na.rm=T),
                      , title = paste0("Output ", input$outputlayer), labFormat = labelFormat(transform = function(x) sort(quantile(dt.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)), group = "ModelResult", opacity=input$alpha)
        }
      }
    }
    
    if (input$outputGroup == "print_in") { 
      # Add input layer
      dt_input = rnew_input()
      dt_input.v = getValues(dt_input)
      dt_input.rng = range(dt_input.v, na.rm = T)
      # print(dt_input.rng)
      
      pal_input = colorNumeric(input$colors, reverse = input$InvertColour, domain = dt_input.rng, na.color = "transparent")
      
      proxy %>%
        addRasterImage(dt_input, project = FALSE, colors =pal_input, method = "bilinear", group = "ModelResult"
                       , opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
      if (input$legend) { proxy %>%
          addLegend(pal = pal_input, values = quantile(dt_input.v, probs=seq(1, 0, -0.05), na.rm=T),
                    , title = paste0("Input ", input$inputlayer), labFormat = labelFormat(transform = function(x) sort(quantile(dt_input.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)), group = "ModelResult")
      }
    }
    
    
    # add empty layer 
    
    proxy %>% addRasterImage(r_dummy, project = FALSE, group = "Basemap", opacity = 0) %>% addMiniMap(position = "bottomleft", zoomAnimation = T, toggleDisplay = TRUE)  %>% addMeasure()
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
  #     #proxy %>% addLegend(colors = col2hex(aft_colors_fromzero), labels = aft_names_fromzero, title = "AFT")
  #     
  #     if (input$outputlayer %in% c("LandUseIndex")) {
  #       proxy %>%
  #         addLegend(colors = col2hex(aft_colors_fromzero), labels = aft_shortnames_fromzero, title = input$indicator)
  #     } else {
  #       dt.v = getValues(dt)
  #       dt.rng = range(dt.v, na.rm = T)
  #       pal = colorNumeric("RdYlBu", domain = dt.rng, na.color = "transparent")
  #       addLegend(pal = pal , values = quantile(dt.v, probs=seq(0, 1, 0.05), na.rm=T)
  #                 , title = input$indicator)
  #       
  #     }
  #     
  #   }
  # })
  #
  
  # output$downloadData <- downloadHandler(
  #   
  #   filename = function() {
  #     
  #     indicator_idx = which (input$outputlayer == indicator_names)
  #     p.idx = which(input$paramset_full == paramsets_fullnames)
  #     
  #     fname_changed =  paste0("CRAFTY-EU_", paramsets[p.idx], "_", input$scenario, "_", "FoodPrice_", input$foodprice, "_", "MeatDemand_", input$fooddemand, "_", input$year, "_", input$outputlayer, ".tif")
  #     
  #     # fname_changed      
  #   },
  #   content = function(file) {
  #     
  #     indicator_idx = which (input$outputlayer == indicator_names)
  #     p.idx = which(input$paramset_full == paramsets_fullnames)
  #     
  #     # fname_changed =  paste0("Data/",paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
  #     
  #     # if (input$food != "Normal") { 
  #     
  #     fname_changed =   paste0("Data/", input$foodprice, "/",  input$fooddemand, "/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
  #     
  #     # }
  #     
  #     fname_changed =   paste0("Data/", input$foodprice, "/",  input$fooddemand, "/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
  #     
  #     # }
  #     
  #     print(fname_changed)
  #     data = projectRaster(getRaster(fname_changed, band.idx = indicator_idx), crs = proj4.LL)
  #     
  #     writeRaster(data, file)
  #   }
  # )
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
  
  
  
  # # reactiveValues to create a list of defaults, and
  # values <- reactiveValues(type = "Int. arable")
  # # observeEvent for the input$bins, updating the defaults
  # observeEvent(input$type_sn, {
  #   values$type <- input$type_sn
  # })
  
  
  
  
  
  
  # rnew <- reactive({
  #   
  
  #   p.idx = which(input$paramset_full == paramsets_fullnames)
  #   
  #   # fname_changed =  paste0("Data/", paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-99-EU-Cell-", input$year, ".csv")
  #   # if (input$food != "Normal") { 
  #   
  #   fname_changed =   paste0( input$foodprice, "/", paramsets[p.idx], "/", input$scenario, "/", input$fooddemand, "/",  input$scenario, "-", runid, "-99-UK-Cell-", input$year, ".csv")
  #   # }
  #   # spdf_changed = getSPDF(fname_changed)
  #   # rs_changed = stack(spdf_changed)[[4:22]]
  #   # 
  #   # # r_changed= raster(paste0("Data/Maps/", input$scenario, "-0-0-EU-Cell-", input$year, "_LL.tif"), 16)
  #   # r_changed = rs_changed[[indicator_idx]] 
  #   # print(indicator_idx)
  #   
  #   
  #   indicator_idx = which (input$outputlayer == indicator_names)
  #   r_changed = getRaster(fname_changed, band.idx = indicator_idx)
  #   
  #   # r_changed_projected = projectRaster(r_changed, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E4)
  #   
  #   # return(r_changed)
  #   
  #   # r <-   raster(paste0("Data/Maps/Baseline-0-0-EU-Cell-", input$year, "_LL.tif"), 16)
  # })
  
  
  
  rnew <- reactive( {
    print("Rnew called")
    
    # input$background # touch
    
    p.idx = which(input$paramset_full == paramsets_fullnames)
    
    fname_changed =getFname(input$version, paramsets[p.idx], input$scenario,input$year)   
    
    r_changed = getRaster(fname_changed, band.name = input$outputlayer, resolution = RESOLUTION_WEB, location = location_UK)
    
    return(r_changed)
  })
  
  
  # 
  # rnew_UK_density <- reactive( {
  #   
  #   input$background_sn # touch
  #   
  #   p.idx = which(input$paramset_sn == paramsets_fullnames)
  #   
  #   fname_changed = paste0("Data/", input$foodprice_sn, "/",  input$fooddemand_sn, "/", paramsets[p.idx], "/", input$scenario_sn, "/", input$scenario_sn, "-",runid, "-99-EU-Cell-", input$year_sn, ".csv")
  #   
  # 
  #     indicator_idx = which ("Land Use (17 AFTs)" == indicator_names)
  # 
  #   r_changed = getRaster(fname_changed, band.idx = indicator_idx, resolution = RESOLUTION_SN)
  #   
  #   # r_changed_projected = projectRaster(r_changed, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E4)
  #   
  #   return(r_changed)
  #   # r <-   raster(paste0("Data/Maps/Baseline-0-0-EU-Cell-", input$year, "_LL.tif"), 16)
  # })
  # 
  
  
  # 
  # 
  # # should be managed in its own observer.
  # observe({
  #   
  #   dt = rnew_UK()
  #   dt_density = rnew_UK_density()
  #   # print(which (input$indicator == indicator_names))
  #   
  #   proxy <- leafletProxy("TabUK_MapPane", data =rnew_UK())
  #   p.idx = which(input$paramset_sn == paramsets_fullnames)
  #   
  #   
  #   # touches
  #   input$background_sn
  #   
  #   # Layers control
  #   proxy %>% addLayersControl(
  #     # baseGroups = c( "AFT","AFTdensity","GuMean", "GiMean"),
  #     # baseGroups = c( "AFT"),
  #     # overlayGroups = c( "AFTdensity", "GuMean", "GiMean"),
  #     overlayGroups = c("AFT", "Social.capital"),
  #     options = layersControlOptions(collapsed = FALSE)
  #   )
  #    
  #   proxy %>% clearImages() %>% clearControls() 
  #   
  #   
  #   aft_selected = match(values$type, aft_names_fromzero)
  #   
  #   sp_1 = SpatialPoints(rasterToPoints(dt, fun=function(x){x==aft_selected}))
  #   proj4string(sp_1) = proj4string(dt)
  #   
  #   
  #   # sp_2 <- rescale(as.ppp(sp_1), 1000, unitname = "km")
  #   if (FALSE) { 
  #     sp_2 <- as.ppp(sp_1)
  #     
  #     dt_1 = density(sp_2, eps=input$socialnet_width * 1000 ) # sigma Kernel Smoothed Intensity of Point Pattern. sigma  means Standard deviation of isotropic smoothing kernel.  eps
  #     
  #     dt_1_r = raster(dt_1)
  #     proj4string(dt_1_r) = proj4string(dt)
  #     
  #     dt_1_r=  mask(projectRaster(dt_1_r, dt), dt)
  #   }
  #   
  #   sn_width = as.integer(round(input$socialnet_width * 1E3 / RESOLUTION_SN)) # km to pixel num
  #   
  #   sn_width = ifelse(sn_width %% 2 == 0, yes = sn_width +1 , no = sn_width)  # make it a odd number
  #   
  #   sn_width = max(3, sn_width) # at least the adjacent 8 pixels
  #   print(paste0("focal ", sn_width, " pixels"))
  #   
  #   r_in = dt_density==aft_selected
  #   
  #   
  #   wm <-focalWeight(r_in, d=input$socialnet_width * 1E3 ,type="circle") # original weight matrix
  #   w<-wm
  #   w[w>0]<-1
  #   
  #   # dt_1_r = focal(r_in, w = matrix(data = 1, nrow =sn_width, ncol = sn_width ), pad = T, padValue=0, fun = mean, na.rm=T)
  #   dt_1_r = focal(r_in, w = w, pad = T, padValue=0, fun = mean, na.rm=T)
  #   
  #   
  #   
  #   # dt_1_r[dt_1_r< 0] = 0
  #   # dt_1_r[is.na(dt_1_r)] = 0
  #   # 
  #   dt_1_r=  mask(projectRaster(dt_1_r, dt), dt)
  #   
  #   
  #   if (input$outputlayer_sn == "AFT density") { 
  # 
  # 
  #     # proxy %>% hideGroup("GuMean") %>% hideGroup("GiMean")%>% hideGroup("AFTdensity")
  #     # Add AFT density layer 
  #     pal = colorNumeric(input$colors_sn, domain = c(0,1), na.color = "transparent")
  #     proxy %>%  
  #       addRasterImage(dt_1_r, colors = pal, project = FALSE, group = "AFTdensity", method="ngb", 
  #                      , maxBytes = 4 * 1024 * 1024, opacity = input$alpha_sn)
  #     proxy %>% addLegend(pal = pal, values = seq(1, 0, -0.05), group = "AFTdensity", title="AFT density (0-1)")
  #     
  #      
  #     
  #     # } else if (input$outputlayer_sn == "Giving-up mean") { 
  #     #    
  #     #   
  #     #   Gu =  gugi_info$givingUpDistributionMean
  #     #   
  #     #   gu_new =  Gu + dt_1_r * alpha + beta 
  #     #   
  #     #   dt_gu_new  = getValues(gu_new)
  #     #   dt_gu_new_rng = range(dt_gu_new, na.rm = T)
  #     #   
  #     # 
  #     #   print(dt_gu_new_rng)
  #     #   
  #     #   pal_gu = colorNumeric(input$colors_sn, reverse = T, domain = dt_gu_new_rng, na.color = "transparent")
  #     #   
  #     #   proxy %>%
  #     #     addRasterImage(gu_new, project = FALSE, colors =pal_gu, method = "ngb", group = "GuMean'"
  #     #                    , opacity = input$alpha_sn, maxBytes = 4 * 1024 * 1024)
  #     #   proxy %>%
  #     #     addLegend(pal = pal_gu, values = quantile(dt_gu_new, probs=seq(1, 0, -0.05), na.rm=T), layerId = "GuMean_Legend",  group = "GuMean", title = paste0("Gu Mean"))
  #     #   
  #     #   
  #     
  #     # Add AFT layer
  #     proxy %>%
  #       addRasterImage(dt == aft_selected, project = FALSE, colors = c("transparent", "black"), group = "AFT"
  #                      , opacity = input$alpha_sn, maxBytes = 4 * 1024 * 1024, , method="ngb")
  #     
  #     
  #   } else if (input$outputlayer_sn == "Social.capital") { 
  #      
  #     sc_r = dt
  #     
  #     # SN model 
  #     alpha = input$sn_alpha
  #     beta = input$sn_beta
  #     print(dt_1_r)
  #     sc_new =  sc_r + sc_r * (1 + dt_1_r) * alpha + beta 
  #     
  #     # sc_new = sc_new / max(getValues(sc_new), na.rm=T) *  max(getValues(sc_r), na.rm=T)
  # 
  #     dt_input.v = getValues(sc_new)
  #     dt_input.rng = range(dt_input.v, na.rm = T)
  #     print(dt_input.rng)
  #     
  #     pal_sc = colorNumeric(input$colors_sn, reverse = T, domain = dt_input.rng, na.color = "transparent")
  #     
  #     proxy %>%
  #       addRasterImage(sc_new, project = FALSE, colors =pal_sc, method = "bilinear", group = "Social.capital"
  #                      , opacity = input$alpha_sn, maxBytes = 4 * 1024 * 1024) %>% 
  #       addLegend(pal = pal_sc, values = quantile(dt_input.v, probs=seq(1, 0, -0.05), na.rm=T), 
  #                 , title = paste0("Social.capital"), labFormat = labelFormat(transform = function(x) sort(quantile(dt_input.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)), group = "Social.capital")
  #     
  # 
  #   }
  # })
  
  # 
  #   observe({
  #     # dt = providernew()
  #     proxy <- leafletProxy("TabUK_MapPane", data = providernew_sn())
  #     proxy %>% clearTiles() %>% addProviderTiles(input$background_sn, options = providerTileOptions(noWrap = TRUE), group = "TileLayer")
  #     # proxy %>% clearControls() 
  #     
  #   })
  #   
  
  # output$PaneSNinfo <- renderText({
  #   paste0("", input$type_sn, " in ", input$year_sn, " with the ", input$paramset_sn, " parameters and ",  input$scenario_sn, " scenario. Default AFT params: ")# line break
  # })
  # 
  # output$PaneSNinfoGugi <- renderText({
  #   p.idx = which(input$paramset_sn == paramsets_fullnames)
  #   
  #   aft_selected = match(values$type, aft_names_fromzero)
  #   gugi_info = aft_params_df_l[[p.idx]][aft_selected, ]
  #   gugi_info_txt = paste0(names(gugi_info), "=",  as.character(gugi_info), collapse = "  \t")
  #   paste0(  gugi_info_txt)# line break
  #   
  #   
  # })
  
  # function(input, output, session) {
  #   output$plot <- renderPlot({
  #     plot(cars, type=input$plotType)
  #   })
  #   
  #   output$summary <- renderPrint({
  #     summary(cars)
  # })
  #   
  
})
