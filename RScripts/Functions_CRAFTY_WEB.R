library(rdrop2) # Dropbox access
library(gplots) # color palette
library(RColorBrewer)

library(shiny) 

library(raster)
library(rgdal)
library(rgeos)

library(maptools)
library(spatstat) # density map

library(dplyr)    # reshaping data frame 

library(leaflet)  # leaflet.js
library(leaflet.extras)
# library(wesanderson)

library(Gmisc) # transition plot 

PLOT_HEIGHT = 1000 

SIDEBAR_WIDTH = 2
MAINPANEL_WIDTH = 12-SIDEBAR_WIDTH

# A seed used in the CRAFTY runs 
seedid = "99"

# Lon-Lat projection 
proj4.LL <- CRS("+proj=longlat +datum=WGS84")

# Proj4js.defs["EPSG:3035"] etrs89/etrs-laea
# Scope: Single CRS for all Europe. Used for statistical mapping at all scales and other purposes where true area representation is required.
# Reference: http://spatialreference.org/ref/epsg/3035/
proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";



# gugi_values 
aft_parms_df = t(sapply(aft.shortnames.fromzero, FUN = function(x) read.csv(paste0("Tables/agents_eupaper_final/AftParams_", x, ".csv"))[,-c(1, 9)]))
# aft_parms_df
# str(aft_parms_df)

# Scenarios (total 8)
scenario.names = c("Baseline", "RCP2_6-SSP1", "RCP2_6-SSP4", "RCP4_5-SSP1", "RCP4_5-SSP3", "RCP4_5-SSP4", "RCP8_5-SSP3", "RCP8_5-SSP5")

foodprice.names = c("Normal", "Tripled", "Nonupled") 
fooddemand.names = c("Normal", "LowMeatDemand")

n.paramset = 5
paramsets = paste0("Paramset", 1:n.paramset)
paramsets.fullnames = c("Behavioural baseline (P1)", "Thresholds (P2)", "Variations (P3)", "Larger Thresholds (P4)", "Larger Variations (P5)") # , "Behavioural baseline Gu=0 (P6)",  "Behavioural baseline Gu=0.2 (P7)") #,  "Behavioural baseline YearNameFalse (P8)") 

indicator.names =  c("Service.Meat","Service.Crops","Service.Diversity",
                     "Service.Timber","Service.Carbon","Service.Urban",
                     "Service.Recreation","Crop.productivity","Forest.productivity",
                     "Grassland.productivity","Financial.capital","Human.capital",
                     "Social.capital","Manufactured.capital","Urban.capital",
                     "LandUse_notuse","Land Use (17 AFTs)","Agent_notuse", "Competitiveness", "Land Use (8 AFTs)")


indicators_categorical = indicator.names[c(16:18, 20)]


serviceNames <- c("Meat","Crops", "Diversity", "Timber", "Carbon", "Urban", "Recreation")
# serviceColours <- c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity" = "red", "Timber" = "tan4", "Carbon" = "darkgreen", "Urban" = "grey", "Recreation" = "orange")
serviceColours = c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity"="turquoise", "Timber" = "tan4","Carbon"="black", "Urban" = "grey","Recreation"="dodgerblue2")


# aft.colors = rich.colors(17)
aft.shortnames.fromzero <- c( "Ext_AF", "IA", "Int_AF", "Int_Fa", "IP", "MF", "Min_man", "Mix_Fa", "Mix_For", "Mix_P", "Multifun", "P-Ur", "UL", "UMF", "Ur", "VEP", "EP")


# aftNames <- c("IA","Int_Fa","Mix_Fa","Int_AF","Ext_AF","MF", "Mix_For","UMF","IP","EP","Mix_P","VEP","Multifun","Min_man","UL","P-Ur","Ur", "Lazy FR")

aft.colors.fromzero <-  (c("Ext_AF" = "yellowgreen", "IA"  = "yellow1", "Int_AF" =  "darkolivegreen1", "Int_Fa" = "lightgoldenrod1",  "IP" = "red1", "MF" =  "green3", "Min_man" = "lightyellow3",  "Mix_Fa" = "darkgoldenrod",  "Mix_For" = "green4",   "Mix_P" = "violetred",  "Multifun" = "blueviolet", "P-Ur"="lightslategrey", "UL" = "grey", "UMF" = "darkgreen", "Ur" = "black", "VEP" = "red4", "EP" = "red3")) # , "Lazy FR" = "black")

aft.names.fromzero <- c("Ext. agro-forestry","Int. arable","Int. agro-forestry","Int. mixed farming","Int. pastoral","Managed forest","Minimal management",
                        "Ext. mixed farming","Mixed forest","Mixed pastoral","Multifunctional","Peri-Urban", "Unmanaged land","Umanaged forest","Urban", "Very ext. pastoral","Ext. pastoral")

# AFTColours <- c("Ext. agro-forestry" = "darkolivegreen3","Int. arable"="gold1","Int. agro-forestry"="darkolivegreen1","Int. mixed farming"="darkorange1",
# "Int. pastoral"="firebrick1","Managed forest"="chartreuse2","Minimal management"="darkgrey","Ext. mixed farming"="darkorange3","Mixed forest"="chartreuse4",
# "Mixed pastoral"="firebrick2", "Multifunctional"="dodgerblue3","Unmanaged land"="black","Umanaged forest"="darkgreen","Very ext. pastoral"="firebrick4","Ext. pastoral"="firebrick3")

target_years_aggcsv = seq(2020, 2090, 10)
target_years_other = seq(2016,2096,10)

aft.pal <- colorFactor(col2hex(as.character(aft.colors.fromzero)), levels = 0:17, na.color = "transparent") # "#0C2C84", "#41B6C4", "#FFFFCC"), # , bins = 17) 

# 8 aft classes used in the EU-paper
# result$Agent[result$Agent=="IA"] <- "Intensive arable"
# result$Agent[result$Agent=="IP"] <- "Intensive grassland"
# result$Agent[result$Agent=="MF"] <- "Intensive forest"
# result$Agent[result$Agent=="Int_AF" |result$Agent=="Int_Fa"] <- "Mixed intensive"
# result$Agent[result$Agent=="Mix_Fa" |result$Agent=="Ext_AF" |result$Agent=="Multifun"|result$Agent=="EP"|result$Agent=="Mix_P"] <- "Mixed extensive"
# result$Agent[result$Agent=="UMF" |result$Agent=="Mix_For"|result$Agent=="VEP"] <- "Extensive primarily forest"
# result$Agent[result$Agent=="Min_man" |result$Agent=="UL"] <- "Near-natural"
# result$Agent[result$Agent=="Ur" |result$Agent=="P-Ur" |result$Agent=="Lazy FR"] <- "Other"

aft.lookup.17to8 = matrix(ncol = 2, byrow = T, data = c(
  "Lazy FR", "Other",
  "Ext_AF", "Mixed extensive", 
  "IA", "Intensive arable",
  "Int_AF",  "Mixed intensive",
  "Int_Fa",  "Mixed intensive",
  "IP", "Intensive grassland", 
  "MF", "Intensive forest", 
  "Min_man", "Near-natural",
  "Mix_Fa", "Mixed extensive", 
  "Mix_For", "Extensive primarily forest",
  "Mix_P", "Mixed extensive", 
  "Multifun", "Mixed extensive", 
  "P-Ur","Other", 
  "UL", "Near-natural", 
  "UMF", "Extensive primarily forest",
  "Ur","Other", 
  "VEP", "Extensive primarily forest",
  "EP", "Mixed extensive"
))








aft.names.8classes <- aft.fullnames.8classes <- c("Intensive arable","Intensive grassland","Intensive forest","Mixed intensive","Mixed extensive","Extensive primarily forest","Near-natural","Other")

aft.colors.8classes <- c("Intensive arable" = "khaki2", "Intensive grassland" = "greenyellow","Intensive forest" = "olivedrab4",
                         "Mixed intensive" = "gold1","Mixed extensive" = "yellowgreen","Extensive primarily forest" = "darkgreen","Near-natural"="gray37","Other"="gray85") # other was white 


provider_names = c(
  "OpenStreetMap.Mapnik"
  # "OpenTopoMap"  
#  , "Stamen.Terrain"
#  , "Thunderforest"      
  , "Esri.WorldImagery"             
  ,"Esri.WorldPhysical"              
#  , "Esri.NatGeoWorldMap" 
# , "CartoDB"
#  , "NASAGIBS.ModisTerraTrueColorCR", "NASAGIBS.ModisTerraBands367CR"      
 # ,"NASAGIBS.ViirsEarthAtNight2012"
 # ,  "Wikimedia"      
)

# [1] "OpenStreetMap"                       "OpenStreetMap.Mapnik"                "OpenStreetMap.BlackAndWhite"        
# [4] "OpenStreetMap.DE"                    "OpenStreetMap.CH"                    "OpenStreetMap.France"               
# [7] "OpenStreetMap.HOT"                   "OpenStreetMap.BZH"                   "OpenInfraMap"                       
# [10] "OpenInfraMap.Power"                  "OpenInfraMap.Telecom"                "OpenInfraMap.Petroleum"             
# [13] "OpenInfraMap.Water"                  "OpenSeaMap"                          "OpenPtMap"                          
# [16] "OpenTopoMap"                         "OpenRailwayMap"                      "OpenFireMap"                        
# [19] "SafeCast"                            "Thunderforest"                       "Thunderforest.OpenCycleMap"         
# [22] "Thunderforest.Transport"             "Thunderforest.TransportDark"         "Thunderforest.SpinalMap"            
# [25] "Thunderforest.Landscape"             "Thunderforest.Outdoors"              "Thunderforest.Pioneer"              
# [28] "OpenMapSurfer"                       "OpenMapSurfer.Roads"                 "OpenMapSurfer.AdminBounds"          
# [31] "OpenMapSurfer.Grayscale"             "Hydda"                               "Hydda.Full"                         
# [34] "Hydda.Base"                          "Hydda.RoadsAndLabels"                "MapBox"                             
# [37] "Stamen"                              "Stamen.Toner"                        "Stamen.TonerBackground"             
# [40] "Stamen.TonerHybrid"                  "Stamen.TonerLines"                   "Stamen.TonerLabels"                 
# [43] "Stamen.TonerLite"                    "Stamen.Watercolor"                   "Stamen.Terrain"                     
# [46] "Stamen.TerrainBackground"            "Stamen.TopOSMRelief"                 "Stamen.TopOSMFeatures"              
# [49] "Esri"                                "Esri.WorldStreetMap"                 "Esri.DeLorme"                       
# [52] "Esri.WorldTopoMap"                   "Esri.WorldImagery"                   "Esri.WorldTerrain"                  
# [55] "Esri.WorldShadedRelief"              "Esri.WorldPhysical"                  "Esri.OceanBasemap"                  
# [58] "Esri.NatGeoWorldMap"                 "Esri.WorldGrayCanvas"                "OpenWeatherMap"                     
# [61] "OpenWeatherMap.Clouds"               "OpenWeatherMap.CloudsClassic"        "OpenWeatherMap.Precipitation"       
# [64] "OpenWeatherMap.PrecipitationClassic" "OpenWeatherMap.Rain"                 "OpenWeatherMap.RainClassic"         
# [67] "OpenWeatherMap.Pressure"             "OpenWeatherMap.PressureContour"      "OpenWeatherMap.Wind"                
# [70] "OpenWeatherMap.Temperature"          "OpenWeatherMap.Snow"                 "HERE"                               
# [73] "HERE.normalDay"                      "HERE.normalDayCustom"                "HERE.normalDayGrey"                 
# [76] "HERE.normalDayMobile"                "HERE.normalDayGreyMobile"            "HERE.normalDayTransit"              
# [79] "HERE.normalDayTransitMobile"         "HERE.normalNight"                    "HERE.normalNightMobile"             
# [82] "HERE.normalNightGrey"                "HERE.normalNightGreyMobile"          "HERE.basicMap"                      
# [85] "HERE.mapLabels"                      "HERE.trafficFlow"                    "HERE.carnavDayGrey"                 
# [88] "HERE.hybridDay"                      "HERE.hybridDayMobile"                "HERE.pedestrianDay"                 
# [91] "HERE.pedestrianNight"                "HERE.satelliteDay"                   "HERE.terrainDay"                    
# [94] "HERE.terrainDayMobile"               "FreeMapSK"                           "MtbMap"                             
# [97] "CartoDB"                             "CartoDB.Positron"                    "CartoDB.PositronNoLabels"           
# [100] "CartoDB.PositronOnlyLabels"          "CartoDB.DarkMatter"                  "CartoDB.DarkMatterNoLabels"         
# [103] "CartoDB.DarkMatterOnlyLabels"        "HikeBike"                            "HikeBike.HikeBike"                  
# [106] "HikeBike.HillShading"                "BasemapAT"                           "BasemapAT.basemap"                  
# [109] "BasemapAT.grau"                      "BasemapAT.overlay"                   "BasemapAT.highdpi"                  
# [112] "BasemapAT.orthofoto"                 "nlmaps"                              "nlmaps.standaard"                   
# [115] "nlmaps.pastel"                       "nlmaps.grijs"                        "nlmaps.luchtfoto"                   
# [118] "NASAGIBS"                            "NASAGIBS.ModisTerraTrueColorCR"      "NASAGIBS.ModisTerraBands367CR"      
# [121] "NASAGIBS.ViirsEarthAtNight2012"      "NASAGIBS.ModisTerraLSTDay"           "NASAGIBS.ModisTerraSnowCover"       
# [124] "NASAGIBS.ModisTerraAOD"              "NASAGIBS.ModisTerraChlorophyll"      "NLS"                                
# [127] "JusticeMap"                          "JusticeMap.income"                   "JusticeMap.americanIndian"          
# [130] "JusticeMap.asian"                    "JusticeMap.black"                    "JusticeMap.hispanic"                
# [133] "JusticeMap.multi"                    "JusticeMap.nonWhite"                 "JusticeMap.white"                   
# [136] "JusticeMap.plurality"                "Wikimedia"                      
# 
# 
# 


path.wd <- ("KIT_Modelling/CRAFTY/crafty_web/")
path.serverwd = getwd()
path.droptmp = paste0(path.serverwd, "/droptmp/")
path.rastertmp = paste0(path.serverwd, "/rastertmp/")

if(!dir.exists(path.droptmp)) { 
  dir.create(path.droptmp)
}

drop_token_name = "Authentication/droptoken.rds"

if (!file.exists(drop_token_name)) { 
  
  token <- drop_auth(cache=F)
  saveRDS(token, drop_token_name, version = 2)
} else {
  # token <- readRDS(drop_token_name)
  
  # @todo trycatch
  drop_auth(rdstoken = drop_token_name)
  
}

accessDropbox <- function() { 
  print("do nothing")
}








# result<-read.csv("C:/Users/brown-c/Documents/Work_docs/IMPRESSIONS/CRAFTY Europe IMPRESSIONS/Results/Raw results/Baseline/Baseline-0-0-EU-Cell-2096.csv")
# result<-read.csv("~/Dropbox/KIT_Modelling/CRAFTY/Calibration/ model runs_old/Baseline-0-99-EU-Cell-2016.csv")


#import data

# Cell ID and cooridnates 
# ctry.ids <- read.csv("~/Dropbox/KIT/CLIMSAVE/IAP/Cell_ID_LatLong.csv")
# saveRDS(ctry.ids, file = "GISData/ctry.ids.Rds", version = 2)
ctry.ids = readRDS("GISData/ctry.ids.Rds")
x.lat.v = sort(unique(ctry.ids$Longitude))
y.lon.v = sort(unique(ctry.ids$Latitude))


# simple caching
getCSV = function(tmp.in.name, location = "Dropbox") { 
  
  localfile_path =  paste0(path.droptmp, tmp.in.name)
  
  
  if(location == "Local") {
    stop("@todo ")
    
  } 
  
  if(!file.exists(localfile_path)) {
    localdir_path =  dirname(localfile_path)
    
    if (!dir.exists(localdir_path)) {
      dir.create(localdir_path, recursive = T)
    }
    
    res =drop_read_csv(paste0(path.wd, tmp.in.name), dest = localdir_path)
    
  } else {
    
    
    # tryCatch({
    #   file.tmp = drop_download(paste0(path.wd, tmp.in.name), overwrite = F) # local_path =  paste0(path.droptmp, tmp.in.name))
    # }, finally ={
    #   print("finally") })
    
    # drop_read_csv(paste0(path.wd, tmp.in.name)) #, overwrite=F) # dest = path.droptmp)
    res = read.csv2(localfile_path, sep = ",")
  }
  
  return(res)
}

# tmp.in.name = (paste0("Data/Paramset3/",  scenario.names[3], "/", scenario.names[3], "-0-99-EU-Cell-2016.csv"))

getSPDF <- function(tmp.in.name, location = "Dropbox") {
  
  # Target outcome
  # result.tmp <- read.csv(paste0( tmp.in.name))
  result.tmp = getCSV(tmp.in.name, location)
  result.tmp$lon = x.lat.v[result.tmp$X]
  result.tmp$lat = y.lon.v[result.tmp$Y]
  
  # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
  result.spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL), data = data.frame(result.tmp), tolerance = 0.0011)
  # plot(SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL))
  return(result.spdf)
}

scenarioname.default = "Baseline"
# fname.default = (paste0("Data/Paramset3/", scenarioname.default, "/", scenarioname.default, "-0-99-EU-Cell-2056.csv"))
fname.default = (paste0("Data/Normal/Normal/Paramset1/", scenarioname.default, "/", scenarioname.default, "-0-99-EU-Cell-2016.csv"))
spdf.default = getSPDF(fname.default)
rs.LL <- stack(spdf.default)[[4:22]]
agent.LL = rs.LL[[17]]
r.default = projectRaster(agent.LL, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 2.5E4)


# getRaster(fname.default, band.idx = 18)
# 
# fname = fname.default
# band.idx = 20





# call once on a local workstation
createChangedNumberTable <- function() { 
  
  # price = "Normal"
  # demand = "Normal"
  # paramset = "Paramset1"
  # scenario = "RCP8_5-SSP3"  
  # 
  library(doMC)
  registerDoMC()
  
  library(openxlsx)
  
  foreach(price = foodprice.names, .errorhandling ="stop") %do% {
    print(price)
    
    foreach(demand = fooddemand.names) %do% { 
      print(demand)
      
      foreach(paramset = paramsets, .errorhandling = "stop") %dopar% { 
        print(paramset)
        
        tb_localdir_path =  file.path(paste0("Tables/ChangedPixelNo/", price, "/", demand, "/",paramset, "/" ))
        
        if (!dir.exists(tb_localdir_path)) {
          dir.create(tb_localdir_path, recursive = T)
        }
        
        res1 = foreach(scenario = scenario.names, .combine = "cbind", .errorhandling="stop") %dopar% { 
          
          runid_tmp = which(scenario.names == scenario) - 1 
          
          res=  stack(lapply(target_years_other, FUN = function(year) getRaster(paste0("Data/", price, "/", demand, "/",paramset, "/", scenario  , "/", scenario  , "-",runid_tmp, "-99-EU-Cell-", year, ".csv"), 20, location = "Local")))
          
          res_m = as.matrix(res)
          res_m = res_m[!is.na(res_m[,1]),]
          
          tmp_changedno = sapply(2:ncol(res_m), FUN = function(x) length(res_m[res_m[,x]!=res_m[,x-1], x]))
          names(tmp_changedno) = target_years_other[-1]
          write.xlsx(tmp_changedno, file = paste0(tb_localdir_path, "/", scenario, "_ChangedPixelNo.xlsx"))
          
          return(NULL)
        }
        
        
        
      }
    }
  }
  
  return(TRUE)
}




# call once on a local workstation
createFragstatsTable <- function() { 
  # price = "Normal"
  # demand = "Normal"
  # paramset = "Paramset1"
  # scenario = "RCP8_5-SSP3"  
  
  library(doMC)
  registerDoMC()

  library(openxlsx) # excel 
  library(SDMTools) # fragmentation statistics
  
  foreach(price = foodprice.names, .errorhandling ="stop") %do% {
    print(price)
    
    foreach(demand = fooddemand.names) %do% { 
      print(demand)
      
      foreach(paramset = paramsets, .errorhandling = "stop") %dopar% { 
        print(paramset)
        
        tb_localdir_path =  file.path(paste0("Tables/FragStats/", price, "/", demand, "/",paramset, "/" ))
        
        if (!dir.exists(tb_localdir_path)) {
          dir.create(tb_localdir_path, recursive = T)
        }
        
        res1 = foreach(scenario = scenario.names, .combine = "cbind", .errorhandling = "stop") %dopar% { 
          
          runid_tmp = which(scenario.names == scenario) - 1 
          
             
          res_rs=  stack(lapply(target_years_other, FUN = function(year) getRaster(paste0("Data/", price, "/", demand, "/",paramset, "/", scenario  , "/", scenario  , "-",runid_tmp, "-99-EU-Cell-", year, ".csv"), 20, location = "Local")))
          
          res_rs_LL = projectRaster(res_rs, crs = proj4.LL, res = 0.1, method = "ngb")
          tmp_fragstat_m  = sapply(1:nlayers(res_rs_LL), FUN = function(x) ClassStat(res_rs_LL[[x]], cellsize = 15000, bkgd = NA, latlon = T)$mean.frac.dim.index)
          
            
          colnames(tmp_fragstat_m) = target_years_other
          rownames(tmp_fragstat_m) = aft.names.8classes
          write.xlsx(tmp_fragstat_m, file = paste0(tb_localdir_path, "/", scenario, "_FragStats.xlsx"))
          
          return(NULL)
        }
        
        
        
      }
    }
  }
  
  return(TRUE)
}











getRaster<- function(fname, band.idx, location = "Dropbox") {
  
  localfile_path = paste0("rastertmp/",fname, "_", band.idx, ".tif")
  
  
  if(!file.exists(localfile_path)) {
    
    localdir_path =  dirname(localfile_path)
    if (!dir.exists(localdir_path)) {
      dir.create(localdir_path, recursive = T)
    }
    
    if (location == "Local") { 
      
      result.tmp = read.csv2(fname, sep = ",")
      result.tmp$lon = x.lat.v[result.tmp$X]
      result.tmp$lat = y.lon.v[result.tmp$Y]
      spdf.out <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL), data = data.frame(result.tmp), tolerance = 0.0011)
      
    } else if (location == "Dropbox") {
      
      spdf.out = getSPDF(fname)
    }
    
    
    # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
    rs.LL <- stack(spdf.out)[[4:22]]
    agent_8classes.v= factor(aft.lookup.17to8[getValues(rs.LL[[17]]) + 2, 2 ], levels = aft.fullnames.8classes, labels = aft.fullnames.8classes)
    stopifnot(length(agent_8classes.v) == ncell(rs.LL))
    rs.LL[[20]] = agent_8classes.v
    
    out.reproj = projectRaster(rs.LL[[band.idx]], crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 2.5E4)
    writeRaster(out.reproj, filename = localfile_path, overwrite=T)
    
  } else {
    out.reproj = raster(localfile_path) 
  }
  
  
  return(out.reproj)
}

# [1] "Tick"                           "X"                              "Y"                             
# [4] "Service.Meat"                   "Service.Crops"                  "Service.Diversity"             
# [7] "Service.Timber"                 "Service.Carbon"                 "Service.Urban"                 
# [10] "Service.Recreation"             "Capital.Crop.productivity"      "Capital.Forest.productivity"   
# [13] "Capital.Grassland.productivity" "Capital.Financial.capital"      "Capital.Human.capital"         
# [16] "Capital.Social.capital"         "Capital.Manufactured.capital"   "Capital.Urban.capital"         
# [19] "LandUse"                        "LandUseIndex"                   "Agent"                         
# [22] "Competitiveness"                "lon"                            "lat"         



smr = sapply(1:nlayers(spdf.default[4:22]),FUN = function(x)  summary(spdf.default[4:22][[x]], na.rm=T))
smr.max = sapply(1:19, FUN = function(x) as.numeric(smr[[x]]["Max."])) * 1.2
smr.min = sapply(1:19, FUN = function(x) as.numeric(smr[[x]]["Min."])) * 0.8

smr.max[!is.finite(smr.max)] = 30
smr.min[!is.finite(smr.min)] = 0


pal.list = lapply(1:19,FUN = function(x)  colorNumeric("RdYlBu", domain = c(smr.min[x], smr.max[x]), na.color = "transparent"))

pal.list[[16]] = pal.list[[18]] =  aft.pal 





# 
# saveData <- function(data) {
#   data <- t(data)
#   
#   
#   # Create a unique file name
#   fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
#   # Write the data to a temporary file locally
#   filePath <- file.path(tempdir(), fileName)
#   write.csv(data, filePath, row.names = FALSE, quote = TRUE)
#   # Upload the file to Dropbox
#   drop_upload(filePath, path = outputDir)
# }
# 
# loadData <- function() {
#   # Read all the files into a list
#   filesInfo <- drop_dir(outputDir)
#   filePaths <- filesInfo$path
#   data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
#   # Concatenate all data together into one data.frame
#   data <- do.call(rbind, data)
#   data
# }