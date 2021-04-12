library(rdrop2) # Dropbox access
library(gplots) # color palette
library(RColorBrewer)
library(grid)
library(DT)
 
library(raster)
library(rgdal)
library(rgeos)

library(maptools)
library(spatstat) # density map

library(dplyr)    # reshaping data frame 

library(leaflet)  # leaflet.js
library(leaflet.extras)
# library(wesanderson)
library(markdown)

library(Gmisc) # transition plot 
 
RESOLUTION_WEB = 1E3 # 1.5E4
RESOLUTION_SN = 1.5E4
RESOLUTION_CRAFTY = 1.5E4

PLOT_HEIGHT = 1000 

SIDEBAR_WIDTH = 2
MAINPANEL_WIDTH = 12-SIDEBAR_WIDTH


TRANSPARENCY_DEFAULT = 1.0 

# A seed used in the CRAFTY runs 
seedid = "99"

# Lon-Lat projection 
proj4.LL <- CRS("+proj=longlat +datum=WGS84")

# Proj4js.defs["EPSG:3035"] etrs89/etrs-laea
# Scope: Single CRS for all Europe. Used for statistical mapping at all scales and other purposes where true area representation is required.
# Reference: http://spatialreference.org/ref/epsg/3035/
proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";


proj4.BNG = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.1502,0.247,0.8421,-20.4894 +units=m +no_defs"



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



path_dropbox <- ("KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/")
path_shinywd = "~/tmp"
path.droptmp = paste0(path_shinywd, "/droptmp/")
path.rastertmp = paste0(path_shinywd, "/rastertmp/")


path_data = paste0(path_dropbox, data_prefix)




if(!dir.exists(path.droptmp)) { 
  dir.create(path.droptmp, recursive = T)
  dir.create(path.rastertmp, recursive = T)
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
  warning("it does not do anything yet.")
}








# result<-read.csv("C:/Users/brown-c/Documents/Work_docs/IMPRESSIONS/CRAFTY Europe IMPRESSIONS/Results/Raw results/Baseline/Baseline-0-0-EU-Cell-2096.csv")
# result<-read.csv("~/Dropbox/KIT_Modelling/CRAFTY/Calibration/ model runs_old/Baseline-0-99-EU-Cell-2016.csv")




# simple caching
getCSV = function(tmp.in.name, location = "Dropbox") { 
  
  # @TODO IFU S3
  
  localfile_path =  paste0(path.droptmp, tmp.in.name)
  
  print(localfile_path)
  
  if(location == "Local") {
    # stop("@todo ")
     
  } 
  
  if(!file.exists(localfile_path)) {
    localdir_path =  dirname(localfile_path)
    
    if (!dir.exists(localdir_path)) {
      dir.create(localdir_path, recursive = T)
    }
    # print(tmp.in.name)
    
    res =drop_read_csv(fs::path_expand(paste0(path_data, tmp.in.name)), dest = localdir_path)
    
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

# getSPDF <- function(tmp.in.name, location = "Dropbox") {
#   
#   print(tmp.in.name)
#   
#   # Target outcome
#   # result.tmp <- read.csv(paste0( tmp.in.name))
#   result.tmp = getCSV(tmp.in.name, location)
#   result.tmp$lon = x.lat.v[result.tmp$X]
#   result.tmp$lat = y.lon.v[result.tmp$Y]
#   
#   # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
#   result.spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL), data = data.frame(result.tmp), tolerance = 0.0011)
#   # plot(SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL))
#   return(result.spdf)
# }





getSPDF_UK <- function(tmp.in.name, location = "Dropbox") {
  
  # Target outcome
  # result.tmp <- read.csv(paste0( tmp.in.name))
  result.tmp = getCSV(tmp.in.name, location)
  
  
  # print(nrow(result.tmp))
  
  
  # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
  result.spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(uk_coords$xcoord_bng, uk_coords$ycoord_bng), proj4string = crs(proj4.BNG)), data = data.frame(result.tmp[,-c(1:3)]))# , tolerance = 0.0011)
  # plot(SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL))
  return(result.spdf)
}



getRaster<- function(fname, band.idx, location = "Dropbox", resolution = RESOLUTION_WEB) {
  
  
  localtif_path = paste0(path.rastertmp, fname, "_", band.idx, ".tif")
  
  print(localtif_path)
  
  if(!file.exists(localtif_path)) {
    
    localdir_path =  dirname(localtif_path)
    if (!dir.exists(localdir_path)) {
      dir.create(localdir_path, recursive = T)
    }
    
    if (location == "Local") {
    #   
    #   # result.tmp = read.csv2(fname, sep = ",")
    #   # result.tmp$lon = x.lat.v[result.tmp$X]
    #   # result.tmp$lat = y.lon.v[result.tmp$Y]
    #   # spdf.out <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL), data = data.frame(result.tmp), tolerance = 0.0011)
    #   
    } else if (location == "Dropbox") {
      print("dropbox")
      print(fname)
      spdf.out = getSPDF_UK(fname)
    }
    
    
    # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
    rs.LL <- stack(spdf.out)[[-29]]
    print(rs.LL)
    # agent_8classes.v= factor(aft.lookup.17to8[getValues(rs.LL[[17]]) + 2, 2 ], levels = aft.fullnames.8classes, labels = aft.fullnames.8classes)
    # stopifnot(length(agent_8classes.v) == ncell(rs.LL))
    # rs.LL[[20]] = agent_8classes.v
    
     
    out.reproj = projectRaster(rs.LL[[band.idx]], crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = resolution)
    writeRaster(out.reproj, filename = localtif_path, overwrite=T)
    
  } else {
    out.reproj = raster(localtif_path) 
  }
  
  
  return(out.reproj)
}








scenarioname.default = "Baseline"
# fname.default = (paste0("Data/Paramset3/", scenarioname.default, "/", scenarioname.default, "-0-99-EU-Cell-2056.csv"))
# fname.default = (paste0("Data/Normal/Normal/Paramset1/", scenarioname.default, "/", scenarioname.default, "-0-99-EU-Cell-2016.csv"))
# rs.LL <- stack(spdf.default)[[4:22]]
# agent.LL = rs.LL[[17]]


# csv.default = getCSV(fname.default)
# 
# spdf.default = getSPDF_UK(fname.default)
# rs.LL <- stack(spdf.default)
# agent.LL = rs.LL[[29]]
# 
# 
# 
# r.default = projectRaster(agent.LL, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = RESOLUTION_WEB)

# writeRaster(r.default, filename = "~/Nextcloud/CRAFTY/CRAFTY_WEB/GISData/UK_default.tif")
r.default = raster("GISData/UK_default.tif")
r_dummy  = raster(extent(r.default))
r_dummy = setValues(r_dummy, values = 0)
proj4string(r_dummy) = proj4string(r.default)





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
          tmp_fragstat_m  = sapply(1:nlayers(res_rs_LL), FUN = function(x) ClassStat(res_rs_LL[[x]], cellsize = RESOLUTION_CRAFTY, bkgd = NA, latlon = T)$mean.frac.dim.index)
          
          
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