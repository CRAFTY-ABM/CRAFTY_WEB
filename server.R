# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
source("RScripts/Functions_CRAFTY_WEB.R")


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
  
  # # synchronise selected scenario options
  observeEvent(input$scenario,{
    updateSelectInput(session,"scenario_ts", selected = input$scenario)
    updateSelectInput(session,"scenario_from", selected = input$scenario)
  }
  )
  #  
  # observeEvent(input$scenario_ts,{  
  #   updateSelectInput(session,"scenario", selected = input$scenario_ts)
  #   updateSelectInput(session,"scenario_from", selected = input$scenario_ts)
  # }
  # )
  # 
  # observeEvent(input$scenario_from,{  
  #   updateSelectInput(session,"scenario", selected = input$scenario_from)
  #   updateSelectInput(session,"scenario_ts", selected = input$scenario_from)
  # }
  # )
  # 
  # synchronise selected time slice
  observeEvent(input$year,{
    updateSelectInput(session,"year_from", selected = input$year)
  }
  )
  # 
  # observeEvent(input$year_from,{  
  #   updateSelectInput(session,"year", selected = input$year_from)
  # }
  # )
  
  
  
  runinfo <- reactive({
    # p.idx = which(input$paramset_full == paramsets_fullnames)
    
    output_idx = match(input$outputlayer, indicator_names_full)
    output_name_tmp = indicator_names[output_idx]
    
    input_idx = match(input$inputlayer, indicator_names_full)
    input_name_tmp = indicator_names[input_idx]
    
    
    if (input$outputGroup == "print_out") { 
      out_name = output_name_tmp
    }  else {
      out_name = input_name_tmp
    }
    
    
    
    s_idx = match(input$scenario, MODEL_INFO$scenario_names_full)
    
    paste0("Simulated ", out_name, " in ", input$year, " with the ",
           #input$paramset_full, " parameters and ",
           MODEL_INFO$scenario_names[s_idx], " climate scenario." )
  })
  
  
  observeEvent(input$deleteCache, {
    session$sendCustomMessage(type = 'message',
                              message = 'deleteCache')
    print("delete cache")
    unlink(paste0(path_rastercache, "/*"), recursive = T)
    unlink(paste0(path_filecache, "/*"), recursive = T)
    
  })
  
  observeEvent(input$createCache, {
    session$sendCustomMessage(type = 'message',
                              message = 'createCache')
    print("create cache")
    createTempFiles()
    print("create cache done")
    
  })
  
  
  output$PaneRuninfo <- renderText({
    runinfo()
  })
  
  # output$PaneRuninfo_ts <- renderText({
  #   # runinfo_ts()
  # })
  # 
  
  output$Footer <- renderText({
    
    '<hr/><font size="-9">
     <b>Disclaimer:</b> The content of the interface and any derived analysis may only be used for non-commercial purposes, and on condition that the source reference pursuant to section ‘Required citation’ is included. Any reproductions of content must include a hyperlink to the page (https://landchange.earth/CRAFTY). Any wholesale duplication, translation, reworking, processing, arrangement, transformation of the data for commercial or non-commercial purposes is not permitted without our explicit approval. With respect to information available from this website, we provide no warranty, expressed or implied, including warranties of merchantability and fitness for a particular purpose, nor do we assume any legal liability or responsibility for the accuracy, completeness, or usefulness of any information, product, or process disclosed. Users are recommended to read the info on the webpage and the paper before results are used.
    <br/><b>Required citation:</b> <i>Brown, C., Seo, B., Alexander, P., Burton, V., Chacón-Montalván, E.A., Dunford, R., Merkle, M., Harrison, P.A., Prestele, R., Robinson, E.L., Rounsevell, M., Alternative futures need alternative models: agent-based modelling of the UK\'s land use system. In review.</i>
<br/>
<b>Contact:</b> Queries can be sent to Bumsuk Seo (bumsuk.seo@kit.edu) or Calum Brown (calum.brown@kit.edu).</font>'
  })
  
  
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
    ""
    # "<br/>Please see the detailed decription of the scenarios in Table 2 (pp. 27-28) of the following paper: <p/>
    # <i><small>Brown, C., Seo, B., & Rounsevell, M. (2019). Societal breakdown as an emergent property of large-scale behavioural models of land use change. Earth System Dynamics Discussions, (May), 1–49.</i> <a href='https://doi.org/10.5194/esd-2019-24'>https://doi.org/10.5194/esd-2019-24</a></small>"
  })
  
  output$ReferenceToScenarios <- renderText({
    ""
    #"<br/>Please see the detailed decription of the scenarios in Table 2 (pp. 27-28) of the following paper: <p/>
    # <i><small>Brown, C., Seo, B., & Rounsevell, M. (2019). Societal breakdown as an emergent property of large-scale behavioural models of land use change. Earth System Dynamics Discussions, (May), 1–49.</i> <a href='https://doi.org/10.5194/esd-2019-24'>https://doi.org/10.5194/esd-2019-24</a></small>"
  })
  
  
  # Reactive to output layer
  rnew_output <- reactive({
    print("rnew_output called")
    
    output_idx = match(input$outputlayer, indicator_names_full)
    output_name_tmp = indicator_names[output_idx]
    
    
    # input$background # touch
    
    # p_idx = which(input$paramset_full == paramsets_fullnames)
    p_idx = p_idx_default
    s_idx = match(input$scenario, MODEL_INFO$scenario_names_full)
    
    fname_changed =getFname(default_version_byscenario[s_idx],  MODEL_INFO$paramsets[p_idx], MODEL_INFO$scenario_names[s_idx],input$year)   
    
    r_changed = getRaster(fname_changed, band_name = output_name_tmp, resolution_in =  MODEL_INFO$RESOLUTION_UI, location = MODEL_INFO$location)
    
    print("Return changed raster")
    return(r_changed)
  })
  
  
  # Reactive to input layer
  
  rnew_input <- reactive({
    print("rnew_input called")
    
    input_idx = match(input$inputlayer, indicator_names_full)
    input_name_tmp = indicator_names[input_idx]
    print(input_name_tmp)
    
    # p.idx = which(input$paramset_full == paramsets_fullnames)
    p_idx = p_idx_default
    s_idx = match(input$scenario, MODEL_INFO$scenario_names_full)
    
    
    fname_changed = getFname(default_version_byscenario[s_idx],   MODEL_INFO$paramsets[p_idx], MODEL_INFO$scenario_names[s_idx], input$year)
    
    r_changed = getRaster(fname_changed, band_name = input_name_tmp, resolution_in = MODEL_INFO$RESOLUTION_UI, location = MODEL_INFO$location)
    
    print("Return changed raster")
    
    return(r_changed)
  } )
  
  providernew <- reactive({
    print("providernew called")
    
    input$background # @todo necessary?
  })
  
  
  output$Tab1_StatisticsPane <- renderPlot({
    print("draw stat pane")
    
    target_data = rnew_output()
    
    # runid = which(MODEL_INFO$scenario_names == input$scenario) - 1
    # p.idx = which(input$paramset_full == paramsets_fullnames)
    #
    # fname_changed =  paste0("Data/",  paramsets[p.idx], "/", input$scenario, "/", input$scenario, "-",runid, "-",seedid,"-EU-Cell-", input$year, ".csv")
    # spdf_changed = getSPDF(fname_changed)
    # target_val = spdf_changed[4:22]
    #
    # str(getValues(target_data))
    
    
    output_idx = match(input$outputlayer, indicator_names_full)
    output_name_tmp = indicator_names[output_idx]
    
    input_idx = match(input$inputlayer, indicator_names_full)
    input_name_tmp = indicator_names[input_idx]
    
    
    par(mar = c(5.1, 4.1, 4, 1), mfrow=c(1,2))
    
    
    hist(getValues(target_data), main="Histogram", xlab= output_name_tmp)
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
      
      
      
      DT::datatable(MODEL_INFO$AFT_TB[,c("Name", "Description", "Group", "Type")], options= list(paging = FALSE),  editable = F) 
      # %>%  DT::formatStyle(columns = colnames(.), fontSize = '50%')
      # %>% formatStyle(
      #   'Name',
      #   backgroundColor =  aft_colors_fromzero_17
      # )
    })
  
  
  output$Tab1_ServiceTablePane <- renderDataTable(
    {
      print("draw service pane")
      
      
      Service_tb = read.csv("Tables/Services.csv")
      
      
      Service_tb = Service_tb # AFT_tb[,c("Name", "Description", "Group", "Type")
      DT::datatable(Service_tb, options= list(paging = FALSE),  editable = F) 
      # %>%  DT::formatStyle(columns = colnames(.), fontSize = '50%')
      # %>% formatStyle(
      #   'Name',
      #   backgroundColor =  aft_colors_fromzero_17
      # )
    })
  
  
  output$Tab1_CapTablePane <- renderDataTable(
    {
      print("draw capital pane")
      
      
      Cap_tb = read.csv("Tables/Capitals.csv")
      
      
      Cap_tb = Cap_tb # AFT_tb[,c("Name", "Description", "Group", "Type")
      DT::datatable(Cap_tb, options= list(paging = FALSE),  editable = F) 
      # %>%  DT::formatStyle(columns = colnames(.), fontSize = '50%')
      # %>% formatStyle(
      #   'Name',
      #   backgroundColor =  aft_colors_fromzero_17
      # )
    })
  
  output$Tab1_BehaviouralTablePane <- renderDataTable(
    {
      print("draw behavioural pane")
      
      # p_idx = which(input$paramset_full == paramsets_fullnames)
      p_idx = 1 # fix 
      s_idx = match(input$scenario, MODEL_INFO$scenario_names_full)
      
      
      # foldername_tmp = ("Tables/agents/BehaviouralBaseline/Baseline")
      foldername_tmp = paste0("Tables/agents/",  MODEL_INFO$paramsets[p_idx], "/", MODEL_INFO$scenario_names[s_idx])
      
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
      
      # p_idx = which(input$paramset_full == paramsets_fullnames)
      p_idx = 1
      
      s_idx = match(input$scenario, MODEL_INFO$scenario_names_full)
      
      
      foldername_tmp = ("Tables/production/Baseline")
      foldername_tmp = paste0("Tables/production/", MODEL_INFO$scenario_names[s_idx])
      
      productionparams_l = lapply(aft_shortnames_fromzero[-length(aft_shortnames_fromzero)], FUN = function(x) read.csv(paste0(foldername_tmp, "/", x, ".csv"))) 
      
      a_idx = 1 
      x = productionparams_l[[a_idx]]
      
      
      
      colnames(x)[1] = "Service"
      DT::datatable(x, options= list(paging = F),  editable = F, rownames = F, caption = aft_names_fromzero[a_idx]) 
      # %>%  DT::formatStyle(columns = colnames(.), fontSize = '50%')
    })
  
  
  
  output$Tab2_TimeseriesPlotPane <- renderPlot(height = "auto", width = 1000, res = 96, {
    
    # 
    # print("draw timeseries pane")
    # 
    # p_idx = p_idx_default
    # 
    # scenario_tmp = "RCP6_0-SSP3"
    # # scenario_tmp = "RCP4_5-SSP4"
    # # scenario_tmp = "Baseline"
    # paramset_tmp = "Thresholds"
    # 
    # 
    # # File names
    # s_idx = match(input$scenario_ts, MODEL_INFO$scenario_names_full)
    # 
    # scenario_tmp = MODEL_INFO$scenario_names[s_idx]
    # 
    # paramset_tmp = paramsets[p_idx]
    # 
    # 
    # 
    # if (!str_detect(scenario_tmp, "SSP3")) {
    #   # aft composition
    #   aft_csvname_changed = fs::path_expand(paste0(version_prefix[match(default_version_byscenario[s_idx], version_names)], "/",paramset_tmp , "/", scenario_tmp, "/",  scenario_tmp, "-",MODEL_INFO$runid, "-", MODEL_INFO$seedid, "-",MODEL_INFO$PREFIX,"-AggregateAFTComposition.csv"))
    #   
    #   # supply and demand files
    #   demand_csvname_changed = fs::path_expand(paste0(version_prefix[match(default_version_byscenario[s_idx], version_names)], "/", paramset_tmp, "/", scenario_tmp, "/", scenario_tmp, "-", MODEL_INFO$runid, "-", MODEL_INFO$seedid, "-",MODEL_INFO$PREFIX,"-AggregateServiceDemand.csv"))
    #   
    #   aftcomp_dt = getCSV(aft_csvname_changed, location = MODEL_INFO$location)
    #   demand_dt = getCSV(demand_csvname_changed, location = MODEL_INFO$location)
    #   
    # } else { 
    #   
    #   # aft composition
    #   aft_csvname_changed_v = fs::path_expand(paste0(version_prefix[match(default_version_byscenario[s_idx], version_names)], "/", paramset_tmp, "/", scenario_tmp, "/",  scenario_tmp, "-", MODEL_INFO$runid, "-", MODEL_INFO$seedid, "-", MODEL_INFO$region_names, "-AggregateAFTComposition.csv"))
    #   
    #   aftcomp_dt_l = lapply(aft_csvname_changed_v, FUN = function(x) getCSV(x, location = MODEL_INFO$location))
    #   
    #   aftcomp_dt = cbind(aftcomp_dt_l[[1]][,c("Tick", "Region")],  Reduce("+", lapply(aftcomp_dt_l, FUN = function(x) x[,-c(1:2)])))
    #   
    #   
    #   
    #   # supply and demand files
    #   demand_csvname_changed_v = fs::path_expand(paste0(version_prefix[match(default_version_byscenario[s_idx], version_names)], "/", paramset_tmp, "/", scenario_tmp, "/", scenario_tmp, "-", MODEL_INFO$runid, "-", MODEL_INFO$seedid, "-", MODEL_INFO$region_names, "-AggregateServiceDemand.csv"))
    #   demand_dt_l = lapply(demand_csvname_changed_v, FUN = function(x) getCSV(x, location = MODEL_INFO$location))
    #   
    #   rem_col_idx = match(c("Tick", "Region"), colnames(demand_dt_l[[1]]))
    #   
    #   demand_dt = cbind( Reduce("+", lapply(demand_dt_l, FUN = function(x) x[,-rem_col_idx ])), Region= MODEL_INFO$REGIONNAME, Tick = demand_dt_l[[1]][,c("Tick")])
    #   
    #   
    # }
    # 
    # 
    # 
    # # mean capital level
    # capital_csvname_changed = fs::path_expand(paste0(scenario_tmp, "-", MODEL_INFO$runid, "-", MODEL_INFO$seedid, "-", MODEL_INFO$PREFIX , "-AggregateCapital.csv"))
    # 
    # 
    # capital_scene_tmp = read.csv(paste0("Tables/Summary/", capital_csvname_changed))
    # 
    # aftcomp_dt_org = aftcomp_dt
    # # reclassify
    # aftcomp_dt[,"AFT.IAfood"] = aftcomp_dt[,"AFT.IAfood"] + aftcomp_dt[,"AFT.IAfodder"]
    # aftcomp_dt[,"AFT.IAfodder"] = NULL
    # 
    # # aftcomp_dt[,"AFT.MW"] = aftcomp_dt[,"AFT.PNB"] + aftcomp_dt[,"AFT.PNC"] + aftcomp_dt[,"AFT.PNNB"] + aftcomp_dt[,"AFT.PNNC"]
    # # aftcomp_dt[, c("AFT.PNB", "AFT.PNC","AFT.PNNB","AFT.PNNC")] = NULL
    # 
    # 
    # aftcomp_dt[,"AFT.PNB"]  = aftcomp_dt[,"AFT.PNB"] + aftcomp_dt[,"AFT.PNNB"]
    # aftcomp_dt[,"AFT.PNC"] =  aftcomp_dt[,"AFT.PNC"]+ aftcomp_dt[,"AFT.PNNC"]
    # aftcomp_dt[, c("AFT.PNNB","AFT.PNNC")] = NULL
    # 
    # 
    # colnames(aftcomp_dt)[  colnames(aftcomp_dt) == "AFT.IAfood"] = "AFT.IA"
    # colnames(aftcomp_dt)[  colnames(aftcomp_dt) == "AFT.MW"] = "AFT.PW"
    # 
    # 
    # aftcomp_m = t(as.matrix(sapply(aftcomp_dt[, -c(1,2)] , FUN = function(x) as.numeric(as.character(x)))))
    # 
    # 
    # 
    # # process csv files
    # demand_m = t(as.matrix(sapply(demand_dt[, -c(ncol(demand_dt) - 1:0)] , FUN = function(x) as.numeric(as.character(x)))))
    # 
    # 
    # str(demand_m)
    # ncold = nrow(demand_m)
    # idx_dem_st = ((ncold/2)+1) : ncold
    # idx_sup_st = 1:(ncold/2) 
    # 
    # shortfall_m = ((demand_m[idx_dem_st,] - demand_m[idx_sup_st,]) / demand_m[idx_dem_st,] ) * 100
    # shortfall_m[!is.finite(shortfall_m)] = NA
    # # demand.colors = rich.colors(7)
    # # # library(ggplot2)
    # # # ggplot(aftcomp_dt)
    # 
    # 
    # 
    # 
    # par(mfrow=c(4,2), mar = c(5.1, 5.1, 2, 0)  + c(0,0,0,10), oma=c(0,0,0,0))
    # 
    # # par(mfrow=c(4,2), xpd = T, mar = par()$mar + c(0,0,0,7))
    # # par( mar = c(5.1, 4.1, 4, 0)  + c(0,0,0,8))
    # 
    # 
    # # AFT changes
    # n_cell_unmanaged = n_cell_total - colSums(aftcomp_m) 
    # 
    # aftcomp_m = rbind(aftcomp_m, n_cell_unmanaged)
    # 
    # rownames(aftcomp_m)
    # 
    # aftcomp_perc_m =  aftcomp_m/n_cell_total * 100
    # 
    # # str(aftcomp_perc_m)
    # 
    # 
    # plot(aftcomp_dt$Tick, aftcomp_perc_m[1,], type="l", xlab= "Year", ylab="Proportion (%)", col = aft_group_colors[1], ylim=c(0, max(aftcomp_perc_m, na.rm = T) * 1.1), main = "AFT composition changes", xaxt="n", lty= aft_lty_ts)
    # axis(side=1, at = target_years_other, labels = target_years_other)
    # 
    # for (a.idx in 2:nrow(aftcomp_perc_m)) {
    #   lines(aftcomp_dt$Tick, aftcomp_perc_m[a.idx,], col = aft_group_colors[a.idx], lty=aft_lty_ts[a.idx])
    # }
    # legend("topright", aft_group_shortnames, col = aft_group_colors, lty=aft_lty_ts, cex=LEGEND_CEX, bty="n", xpd = TRUE, inset=c(LEGEND_MAR,0), lwd=1.5)
    # 
    # 
    # 
    # 
    # ### Plotting service supply and demand
    # 
    # 
    # supply_m_norm = (demand_m[idx_sup_st,] / demand_m[idx_sup_st,1] - 1) * 100
    # demand_m_norm = (demand_m[idx_dem_st,] / demand_m[idx_dem_st,1]-1) * 100
    # 
    # supdem_range = range(cbind(supply_m_norm))
    # print(supdem_range)
    # 
    # y_lim_max = max(3, max(abs(supdem_range)) * 1.2)
    # y_lim_v = c(-y_lim_max, y_lim_max)
    # # barplot(height = supply_m_norm, beside=T, ylab= "Relative to 2020's supply (%)", ylim= y_lim_v, col = serviceColours, main = "Service Supply", names= demand_dt$Tick, border=NA)
    # # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
    # 
    # plot(demand_dt$Tick, supply_m_norm[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Relative to 2020's supply (%)",  main = "Service Supply", las=1, xaxt="n" )
    # axis(side=1, at = target_years_other, labels = target_years_other)
    # # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    # abline(h = 0, lty=2)
    # 
    # for (a.idx in c(1:nrow(supply_m_norm))) {
    #   lines(demand_dt$Tick, supply_m_norm[a.idx,],   col = serviceColours[a.idx])
    # }
    # 
    # legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    # 
    # 
    # 
    # 
    # dem_range = range(cbind(demand_m_norm))
    # print(dem_range)
    # 
    # y_lim_max = max(5, max(abs(dem_range)) * 1.2)
    # y_lim_v = c(max(-100, -y_lim_max), y_lim_max)
    # 
    # # barplot(height = demand_m_norm, beside=T, ylab="Relative to 2020's supply (%)", col = serviceColours, main = "Service Demand", names= demand_dt$Tick, ylim=y_lim_v, border=NA)
    # 
    # plot(demand_dt$Tick, demand_m_norm[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Relative to 2020's demand (%)",  main = "Service Demand", las=1, xaxt="n" )
    # axis(side=1, at = target_years_other, labels = target_years_other)
    # # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    # abline(h = 0, lty=2)
    # abline(h = -100, lty=2)
    # 
    # for (a.idx in c(1:nrow(demand_m_norm))) {
    #   lines(demand_dt$Tick, demand_m_norm[a.idx,],   col = serviceColours[a.idx])
    # }
    # 
    # # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
    # legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    # 
    # 
    # #### SDGAP
    # 
    # sdgap  = (demand_m[idx_dem_st,] - demand_m[idx_sup_st,])
    # # sdgap = (sdgap /demand_m[idx_sup_st,1]  ) * 100
    # 
    # 
    # sdgap_range = range(sdgap, na.rm=T)
    # y_lim_max = max(1, max(abs(sdgap_range)) * 1.2,  demand_m[idx_sup_st,1]*0.3) 
    # 
    # y_lim_v = c(-y_lim_max, y_lim_max)
    # 
    # 
    # # barplot(height =  sdgap, beside=T, ylab="Demand - Supply", col = serviceColours, main = "S/D gap", names= demand_dt$Tick, ylim = y_lim_v, border=NA)
    # # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
    # 
    # plot(demand_dt$Tick, sdgap[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Demand - Supply (original unit)",  main = "S/D gap (=D-S)", las=1, xaxt="n", mgp=c(4,1,0))
    # axis(side=1, at = target_years_other, labels = target_years_other)
    # # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    # abline(h = 0, lty=2)
    # 
    # for (a.idx in c(1:nrow(sdgap))) {
    #   lines(demand_dt$Tick, sdgap[a.idx,],   col = serviceColours[a.idx])
    # }
    # legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    # 
    # 
    # 
    # 
    # ###### PRODUCTION SHORTFALL 
    # 
    # shortfall_range = range(shortfall_m[,], na.rm = T)
    # range(shortfall_m)
    # shortfall_max = max(5, max(abs(shortfall_range)) * 1.2)
    # shortfall_intv = floor(shortfall_max / 100) * 10
    # shortfall_intv = max(1, shortfall_intv)
    # 
    # 
    # # print(shortfall_intv)
    # plot(demand_dt$Tick, shortfall_m[1,], type="l", col = serviceColours[1], ylim=c(-shortfall_max,shortfall_max), xlab="Year", ylab="",  main = "Production shortfall", las=1, xaxt="n" )
    # # title(ylab="Production shortfall (%)", mgp=c(3, 1, 0))
    # mtext(side = 2, text ="Production shortfall (%)", line = 4, cex=0.8)
    # 
    # axis(side=1, at = target_years_other, labels = target_years_other)
    # # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    # abline(h = 0, lty=2)
    # 
    # for (a.idx in c(1:nrow(shortfall_m))) {
    #   lines(demand_dt$Tick, shortfall_m[a.idx,],   col = serviceColours[a.idx])
    # }
    # 
    # legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    # 
    # 
    # ########## Mean capital levels
    # 
    # 
    # 
    # # str(capital_scene_tmp)
    # if (nrow(capital_scene_tmp) > 1) { 
    #   capital_scene_tmp[,-1] = sapply(1:length(baseline_capital_tmp[-1]), FUN = function(x) capital_scene_tmp[,x+1] /  baseline_capital_tmp[x+1])
    #   
    #   
    #   ylim_cap = max(sapply(capital_scene_tmp[,-1], max, na.rm=T))
    #   
    #   plot(capital_scene_tmp$Tick, capital_scene_tmp[,2] * 100, type="l", col = capital_colours[1], ylim=c(0, ylim_cap * 100), xlab="Year", ylab="Relative to 2020 (%)",  main = "Mean input capitals changes", las=1, xaxt="n", lwd=1.5)
    #   # title(ylab="Production shortfall (%)", mgp=c(3, 1, 0))
    #   # mtext(side = 2, text ="Production shortfall (%)", line = 4, cex=0.8)
    #   
    #   axis(side=1, at = target_years_other, labels = target_years_other)
    #   # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    #   abline(h = 0, lty=2)
    #   
    #   for (a.idx in c(3:ncol(capital_scene_tmp))) {
    #     lines(capital_scene_tmp$Tick, capital_scene_tmp[,a.idx] * 100,   col = capital_colours[a.idx-1])
    #   }
    #   
    #   legend("topright", legend = capitalNames, col=capital_colours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    #   
    #   
    # } else {   # baseline 
    #   plot.new()
    #   capital_scene_tmp$X = 2020
    #   colnames(capital_scene_tmp) = c("Tick", capitalNames)
    #   capital_scene_tmp[-1] = capital_scene_tmp[-1] / capital_scene_tmp[-1] #all 1 
    # }
    # 
    # 
    # ######### FOOD PRODUCTION 
    # 
    # # Food as a sum of food crops, fodder crops, GF milk & GF meat, as a percentage of the baseline total? Maybe we can convert them to real quantities later...
    # 
    # # food_names = C("Food.crops", "Fodder.crops", "GF.redMeat", "GF.milk")  
    # 
    # food_production = colSums(demand_m[c(1,2,3,13),])
    # 
    # food_production = ((food_production / food_production[1]) -1) * 100  
    # 
    # food_range = range(food_production)
    # print(food_range)
    # 
    # y_lim_max = max(3, max(abs(food_range)) * 1.2)
    # y_lim_v = c(max(-100, -y_lim_max), y_lim_max)
    # 
    # # barplot(height = demand_m_norm, beside=T, ylab="Relative to 2020's supply (%)", col = serviceColours, main = "Service Demand", names= demand_dt$Tick, ylim=y_lim_v, border=NA)
    # 
    # plot(demand_dt$Tick, food_production, type="l", col = "black", ylim=y_lim_v, xlab="Year", ylab="Relative to 2020's production (%)",  main = "Food production", las=1, xaxt="n" )
    # axis(side=1, at = target_years_other, labels = target_years_other)
    # # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    # abline(h = 0, lty=2)
    # abline(h = -100, lty=2)
    # 
    # 
    # # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
    # # legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    # 
    #  
    
    
    
    p_idx = 1
    
    # File names
    s_idx = match(input$scenario_ts, MODEL_INFO$scenario_names_full)
    
    scenario_tmp = MODEL_INFO$scenario_names[s_idx]
    # 
    paramset_tmp = MODEL_INFO$paramsets[p_idx]
    # 
    # 
    # 
    # if (!str_detect(scenario_tmp, "SSP3")) {
    #   # aft composition
    aft_csvname_changed = getAFTCSVname(default_version_byscenario[s_idx],  MODEL_INFO$paramsets[p_idx], MODEL_INFO$scenario_names[s_idx])
    
    # supply and demand files
    demand_csvname_changed = getServiceCSVname(default_version_byscenario[s_idx],  MODEL_INFO$paramsets[p_idx], MODEL_INFO$scenario_names[s_idx])
    
    
    
    
    
    
    aftcomp_dt = getCSV(aft_csvname_changed, location = MODEL_INFO$location)
    demand_dt = getCSV(demand_csvname_changed, location = MODEL_INFO$location)
    
    
    aftcomp_dt = aftcomp_dt[aftcomp_dt$Tick %in% target_years_aggcsv,]
    demand_dt = demand_dt[demand_dt$Tick %in% target_years_aggcsv,]
    
    aftcomp_dt$Region = NULL
    aftcomp_dt$Tick = NULL
    
    
    aftcomp_m = t(as.matrix(sapply(aftcomp_dt[, ] , FUN = function(x) as.numeric(as.character(x)))))
     
    demand_m = t(as.matrix(sapply(demand_dt[, -c(15,16)] , FUN = function(x) as.numeric(as.character(x)))))
    
    
    shortfall_m = ((demand_m[8:14,] - demand_m[1:7,]) / demand_m[8:14,] ) * 100 
    
    

    par(mfrow=c(3,2), mar = c(5.1, 5.1, 2, 0)  + c(0,0,0,8), oma=c(0,0,0,0))
     
    
    
    # 
    # # AFT changes
    aftcomp_perc_m =  aftcomp_m/colSums(aftcomp_m) * 100

    plot(target_years_other, aftcomp_perc_m[1,], type="l", xlab= "Year", ylab="EU-28 proportion (%)", col = aft_colours_default[1], ylim=c(0, max(aftcomp_perc_m, na.rm = T) * 1.1), main = paste0("AFT ", nrow(aftcomp_perc_m), " composition changes"), xaxt="n")

    for (a_idx in 2:nrow(aftcomp_perc_m)) {
      lines(target_years_other, aftcomp_perc_m[a_idx,],   col = aft_colours_default[a_idx])
    }
    legend("topright", aft_names_default, col = aft_colours_default, lty=1, ncol=1, cex=0.8, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))


    par( mar = c(5.1, 4.1, 4, 1) + c(0,0,0,10))
    
    
    ### Plotting service supply and demand 
    
    ### Plotting service supply and demand
    
 
    
    
    
    supply_m_norm = (demand_m[1:7,] / demand_m[1:7,1] - 1) * 100 
    supdem_range = range(supply_m_norm)
    y_lim_max = max(10, max(abs(supdem_range)) * 1.2)
    y_lim = c(-y_lim_max, y_lim_max)
    barplot(height = supply_m_norm, beside=T, ylab= "Relative to 2016's supply (%)", ylim= y_lim, col = serviceColours, main = "Service Supply", names= demand_dt$Tick)
    legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))
    
    
    demand_m_norm = (demand_m[8:14,]/ demand_m[1:7,1]-1) * 100
    
     
    dem_range = range(cbind(demand_m_norm))
    print(dem_range)
    
    y_lim_max = max(5, max(abs(dem_range)) * 1.2)
    y_lim_v = c(max(-100, -y_lim_max), y_lim_max)
    
    barplot(height = demand_m_norm, beside=T, ylab="Relative to 2016's supply (%)", col = serviceColours, main = "Service Demand", names= demand_dt$Tick, ylim=y_lim_v)
    legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))
    
    sdgap  = (demand_m[8:14,] - demand_m[1:7,]) 
    # sdgap = (sdgap /demand_m[1:7,1] -1 ) * 100 
    sdgap_range = range(sdgap, na.rm=T)
    y_lim_max = max(abs(sdgap_range)) *2
    y_lim = c(-y_lim_max, y_lim_max)
    barplot(height = supply_m_norm, beside=T, ylab= "Relative to 2016's supply (%)", ylim= y_lim, col = serviceColours, main = "Service Supply", names= demand_dt$Tick)
    legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))
    
    
    barplot(height =  sdgap, beside=T, ylab="Demand - Supply (original unit)", col = serviceColours, main = "S/D gap", names= demand_dt$Tick, ylim = y_lim)
    legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0))
    
    
    
    ###### PRODUCTION SHORTFALL 
    
 
    
    shortfall_range = range(shortfall_m[,], na.rm = T)
    shortfall_max = max(abs(shortfall_range)) * 2
    shortfall_intv = floor(shortfall_max / 100) * 10 
    shortfall_intv = max(1,shortfall_intv)
    
    # print(shortfall_intv)
    plot(demand_dt$Tick, shortfall_m[1,], type="l", col = serviceColours[1], ylim=c(-shortfall_max,shortfall_max), xlab="Year", ylab="Production shortfall (%)",  main = "Production shortfall", las=1, xaxt="n" )
    # title(ylab="Production shortfall (%)", mgp=c(3, 1, 0))
    # mtext(side = 2, text ="Production shortfall (%)", line = 4, cex=0.8)
    
    axis(side=1, at = target_years_other, labels = target_years_other)
    # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
    abline(h = 0, lty=2)
    
    for (a.idx in c(1:nrow(shortfall_m))) {
      lines(demand_dt$Tick, shortfall_m[a.idx,],   col = serviceColours[a.idx])
    }
    
    legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
    
    
 
 
     
    
    
    
  })
  
  
  
  
  output$Tab3_TransitionPlotPane <- renderPlot(height = PLOT_HEIGHT, res = 96, {
    
    
    p_from_idx = 1
    p_to_idx = 1
    
    s_idx_from = match(input$scenario_from, MODEL_INFO$scenario_names)
    s_idx_to = match(input$scenario_to, MODEL_INFO$scenario_names)
    
    fname_from = "//Baseline - EPAs/Baseline-0-99-EU-Cell-2026.csv"
    fname_to = "//Baseline - EPAs/Baseline-0-99-EU-Cell-2086.csv"
    
    fname_from =  getFname(default_version_byscenario[s_idx_from],  MODEL_INFO$paramsets[p_from_idx], MODEL_INFO$scenario_names[s_idx_from], year =  input$year_from)
    fname_to   =  getFname(default_version_byscenario[s_idx_to],  MODEL_INFO$paramsets[p_to_idx], MODEL_INFO$scenario_names[s_idx_to], year =  input$year_to)
    
    
    
    #### Transition matrix
    
    csv_from = getCSV(fname_from, location = MODEL_INFO$location)
    csv_to = getCSV(fname_to, location =  MODEL_INFO$location)
    
    aft_old = reclassLabels(csv_from$LandUseIndex)
    aft_new = reclassLabels(csv_to$LandUseIndex)
    
    ### factor to include missing class labels
    aft_old_f = factor(aft_old, levels = MODEL_INFO$AFT_LEVELS_TRANSITION)
    aft_new_f = factor(aft_new, levels = MODEL_INFO$AFT_LEVELS_TRANSITION)
    
    aft_tr_df = cbind(aft_old_f, aft_new_f)
    
    aft_tr_df = aft_tr_df[!is.na(rowSums(aft_tr_df)),]
    aft_tr_df = data.frame(cbind(1:nrow(aft_tr_df), aft_tr_df))
    colnames(aft_tr_df)[1] = "rowid"
    
    
    aft_tb_oldandnew = table(aft_old_f, aft_new_f)
    
    trn_mtrx <- with(aft_tr_df, aft_tb_oldandnew)
    # str(aft_tb_oldandnew)
    
    # composition
    aft_old_prop = paste0( round(table(aft_old_f) / sum(table(aft_old_f), na.rm = T) * 100, 1  ), "%")
    aft_new_prop = paste0(round(table(aft_new_f) / sum(table(aft_new_f), na.rm = T)* 100, 1   ), "%")
    
    # Setup proportions
    box_prop <- cbind(aft_old_prop, aft_new_prop)
    # str(box_prop)
    
    par(mfrow=c(1,1))
    plot.new()
    
    transitionPlot(trn_mtrx, new_page=T,   fill_start_box =  TRANSITION_COLOURS, arrow_clr =TRANSITION_COLOURS, cex=1, color_bar = T, txt_start_clr = "black", txt_end_clr = "black", type_of_arrow = "simple", box_txt = box_prop,  overlap_add_width = 1, tot_spacing = 0.07, box_label = c(input$year_from, input$year_to)) # , min_lwd = unit(0.05, "mm"), max_lwd = unit(30, "mm"))
    
    
    legend("center", TRANSITION_NAMES, col = TRANSITION_COLOURS, pch=15, cex=0.8)
    
  })
  
  
  
  output$Tab1_MapPane <- renderLeaflet({
    print("draw mappane 1")
    
    # output_idx = match(input$outputlayer, indicator_names_full)
    # output_name_tmp = indicator_names[output_idx]
    # 
    # input_idx = match(input$inputlayer, indicator_names_full)
    # input_name_tmp = indicator_names[input_idx]
    
    
    leaflet(
      #options = leafletOptions(attributionControl=F, zoomControl=F, # turn off controls
      # zoomSnap=0.01, #zoomDelta=0.01, 
      # trackResize=F, #doesn't seem to work
      # boxZoom = T
      #                )
    ) %>%
      # clearImages() %>% #clearControls() %>%
      #addTiles()
      addProviderTiles(providers$OpenStreetMap.Mapnik, # Esri.WorldImagery
                       options = providerTileOptions(noWrap = TRUE), group = "TileLayer"
      ) %>%
      addLayersControl(
        baseGroups = c("ModelResult",  "Basemap"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      fitBounds(lng1 = MODEL_INFO$extent[1],lat1 =  MODEL_INFO$extent[3], lng2 = MODEL_INFO$extent[2], lat2 = MODEL_INFO$extent[4] ) %>%
      addRasterImage(r_default, project = FALSE, colors = aft_pal_default, opacity = TRANSPARENCY_DEFAULT, maxBytes = 4 * 1024 * 1024, group="ModelResult")  %>%
      addLegend(colors = col2hex(as.character(aft_colours_default)), labels = aft_names_default, title = MAPTITLE_DEFAULT,group = "ModelResult", opacity = TRANSPARENCY_DEFAULT) %>%
      addMiniMap(position = "bottomleft", zoomAnimation = T, toggleDisplay = TRUE)  %>% addMeasure()
    
    
  })
  
  
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
    print("redraw Tab1_MapPane")
    dt = rnew_output()
    # print(which (input$indicator == indicator_names))
    
    proxy <- leafletProxy("Tab1_MapPane", data =dt)
    # proxy %>% clearImages()
    proxy %>% clearControls()
    
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
      
      output_idx = match(input$outputlayer, indicator_names_full)
      output_name_tmp = indicator_names[output_idx]
      
      if (output_name_tmp == "LandUseIndex") {  # land use index
        
        if (input$colorsGroup == "Default") { 
          
          # pal_out = aft_pal_default
          # aft_names_legend = aft_names_default
          # aft_colours_legend = aft_colours_default
          # 
          # } else if (input$colorsGroup == "Reduced (n=7)") { 
          # pal_out = aft_pal_group2
          # aft_names_legend = aft_group2_names
          # aft_colours_legend = aft_group2_colours
          
          # } else if (input$colorsGroup == "Shaded (n=14)") { 
          #   
          #   
          #   
          #   #### Choose the right intensity scaling
          #   intens.year<-subset(intens, intens$years==input$year)
          #   if(input$scenario=="Baseline") {
          #     intens.year.scen = intens[1, 3:6]
          #   } else if (input$scenario=="RCP2.6-SSP1") {
          #     intens.year.scen<-intens.year[,3:6]
          #   } else if (input$scenario=="RCP4.5-SSP2" | input$scenario=="RCP8.5-SSP2") {
          #     intens.year.scen<-intens.year[,7:10]
          #   } else if (input$scenario=="RCP6.0-SSP3") {
          #     intens.year.scen<-intens.year[,11:14]
          #   } else if (input$scenario=="RCP4.5-SSP4") {
          #     intens.year.scen<-intens.year[,15:18]
          #   } else if (input$scenario=="RCP8.5-SSP5") {
          #     intens.year.scen<-intens.year[,19:22]
          #   }
          #   
          #   
          #   
          #   IA.scale<-as.numeric(intens.year.scen[1])
          #   EA.scale<-as.numeric(intens.year.scen[2])
          #   IP.scale<-as.numeric(intens.year.scen[3])
          #   EP.scale<-as.numeric(intens.year.scen[4])
          #   
          #   IA.col<-lighten(A.col,amount=(0.9 - IA.scale))
          #   EA.col<-lighten(A.col,amount=(0.9 - EA.scale))
          #   IP.col<-lighten(P.col,amount=(0.9 - IP.scale))
          #   EP.col<-lighten(P.col,amount=(0.9 - EP.scale))
          #   VEP.col<-lighten(P.col,amount=0.9)
          #   
          #   
          #   aftColours_shaded <-c(IA.col,EA.col,"#d9abd3","#BDED50","#268c20","#215737","#0a1c01",IP.col,EP.col,VEP.col,"#28b1c9","#2432d1","#EE0F05","#fafaf7")
          #   
          #   aftColours_shaded_srt = aftColours_shaded[c(11, 12, 2, 9, 1, 1, 8, 6, 7, 4, 5, 4, 5, 3, 10, 13, 14)]
          #   
          #   pal_out = colorFactor(col2hex(as.character(aftColours_shaded_srt)),  levels = as.character(c(0:15, -1)), na.color = "transparent")
          #   
          #   
          #   aft_names_legend =   aft_group_names_extended
          #   aft_colours_legend = aft_shaded_colours_extended
          #   
        } else {
          pal_out = aft_pal_default
          
          aft_names_legend = aft_names_default
          aft_colours_legend = aft_colours_default
        }
        
        proxy %>% addRasterImage(dt, project = FALSE, colors = pal_out, group = "ModelResult", opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
        
        
        
        
        
        if (input$legend) {
          
          proxy %>% addLegend(colors = col2hex(as.character(aft_colours_legend)), labels = aft_names_legend, title = paste0(input$outputlayer),group = "ModelResult", opacity = input$alpha)
          
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
                      , title = paste0(input$outputlayer), labFormat = labelFormat(transform = function(x) sort(quantile(dt.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)), group = "ModelResult", opacity=input$alpha)
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
      
      
      input_idx = match(input$inputlayer, indicator_names_full)
      input_name_tmp = indicator_names[input_idx]
      
      
      proxy %>%
        addRasterImage(dt_input, project = FALSE, colors =pal_input, method = "bilinear", group = "ModelResult"
                       , opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
      if (input$legend) { proxy %>%
          addLegend(pal = pal_input, values = quantile(dt_input.v, probs=seq(1, 0, -0.05), na.rm=T),
                    , title = paste0(input$inputlayer), labFormat = labelFormat(transform = function(x) sort(quantile(dt_input.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)), group = "ModelResult")
      }
    }
    
    
    # add empty layer to add measure 
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
  
  
  
  
  # # reactiveValues to create a list of defaults, and
  # values <- reactiveValues(type = "Int. arable")
  # # observeEvent for the input$bins, updating the defaults
  # observeEvent(input$type_sn, {
  #   values$type <- input$type_sn
  # })
  
  
  
  
  
  
  #    
  
})
