library(rdrop2) # Dropbox access
library(gplots) # color palette
library(RColorBrewer)
library(grid)
library(DT)

library(raster)
library(rgdal)
library(rgeos)

library(stringr)

# library(maptools)
# library(spatstat) # density map

library(dplyr)    # reshaping data frame 

library(leaflet)  # leaflet.js
library(leaflet.extras)
library(shinyjs) # hidden function
library(wesanderson)
library(markdown)

library(colorspace) # lighten


library(Gmisc) # transition plot 

RESOLUTION_WEB = 1E3 # 1.5E4
RESOLUTION_SN = 1.5E4
RESOLUTION_CRAFTY = 1.5E4

PLOT_HEIGHT = 1000 

SIDEBAR_WIDTH = 2
MAINPANEL_WIDTH = 12 - SIDEBAR_WIDTH


TRANSPARENCY_DEFAULT = 0.9 
aft_group2_colours = c("#E3C16B", #1 IA EA Bio SusAr
                       "#F3EF0C", #2 IP EP
                       "#216E12", #3 PN
                       "#7d7d47", #4 MW VEP AF
                       "#0a1c01", #5 NW
                       "#EE0F05", #6 Urban
                       "#fafaf7") #7 Unmanaged



aft_group2_colours_17 = aft_group2_colours[c(4,# AF 
                                             1,# Bio
                                             1, # EA
                                             2, # EP
                                             1, # IA
                                             1, # IA
                                             2, # IP
                                             4, 5, 3, 3, 3, 3, 1, 4, 6,7)]



LEGEND_MAR = -0.4
LEGEND_CEX = 1

# Lon-Lat projection 
proj4.LL <- CRS("+proj=longlat +datum=WGS84")

# Proj4js.defs["EPSG:3035"] etrs89/etrs-laea
# Scope: Single CRS for all Europe. Used for statistical mapping at all scales and other purposes where true area representation is required.
# Reference: http://spatialreference.org/ref/epsg/3035/
proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";

# British National Grid
proj4.BNG = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.1502,0.247,0.8421,-20.4894 +units=m +no_defs"
proj4.OSGB1936 ="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"  # proj4string(LAD2019_shp) # EPSG:27700

# WGS 84 / Pseudo-Mercator -- Spherical Mercator, Google Maps, OpenStreetMap, Bing, ArcGIS, ESRI
proj4.spherical = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs" # EPSG:3857



source("RScripts/Data_UK.R")

r_dummy  = raster(extent(r_default))
r_dummy = setValues(r_dummy, values = 0)
proj4string(r_dummy) = proj4string(r_default)




app_init <- function() { 
  
  # create caching folders
  if(!dir.exists(path_filecache)) { 
    dir.create(path_filecache, recursive = T)
    dir.create(path_rastercache, recursive = T)
  }
  
  # begin cluster for raster processing
  # endCluster()
  beginCluster(n_thread)
}

app_init()

# does not work quite properly yet.. 
if (!file.exists(drop_token_name)) { 
  
  token <- drop_auth(cache=F)
  saveRDS(token, drop_token_name, version = 2)
} else {
  # token <- readRDS(drop_token_name)
  # @todo trycatch
  drop_auth(rdstoken = drop_token_name)
}

accessDropbox <- function() { 
  warning("it does not do anything yet..")
}


provider_names = c(
  "OpenStreetMap.Mapnik"
  , "OpenTopoMap"  
  , "Stamen.Terrain"
  # , "Thunderforest"      
  , "Esri.WorldImagery"             
  , "Esri.WorldPhysical"              
  , "Esri.NatGeoWorldMap" 
  # , "CartoDB"
  # , "NASAGIBS.ModisTerraTrueColorCR"
  # , "NASAGIBS.ModisTerraBands367CR"      
  , "NASAGIBS.ViirsEarthAtNight2012"
  # , "Wikimedia"      
)

capital_csvname_baseline = "Baseline-0-99-UK-AggregateCapital.csv"
baseline_capital_tmp =  unlist(read.csv(paste0("Tables/Summary/", capital_csvname_baseline)))
names(baseline_capital_tmp)[-1]= as.character(capital_names$Capital)

# @TODO IFU S3
# simple caching @todo improve.. 

getCSV = function(filename_in, location = "Dropbox") { 
  
  
  cat("Read ", filename_in, " from ", location)
  
  if (location == "Dropbox") {
    cached_path =  paste0(path_filecache, filename_in)
    print(cached_path)
    if(!file.exists(cached_path)) {
      
      cached_dir = dirname(cached_path)
      if (!file.exists(cached_dir)) { 
        dir.create( cached_dir, recursive = T)
      }
      
      res =drop_read_csv(fs::path_expand(paste0(path_data_dropbox, filename_in)), dest = cached_dir)
      
    } else {
      res = read.csv(cached_path, sep = ",")
    }
    
  } else { # assume the files in the cached folder already
    local_path_tmp =  paste0(path_data_local, filename_in)
    
    res = read.csv(paste0(local_path_tmp), sep = ",")
  }
  
  return(res)
}


# getSPDF_EU <- function(tmp_in_name, location = "Dropbox") {
#   
#   print(tmp_in_name)
#   
#   # Target outcome
#   # result.tmp <- read.csv(paste0( tmp_in_name))
#   result.tmp = getCSV(tmp_in_name, location)
#   result.tmp$lon = x.lat.v[result.tmp$X]
#   result.tmp$lat = y.lon.v[result.tmp$Y]
#   
#   # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
#   result.spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL), data = data.frame(result.tmp), tolerance = 0.0011)
#   # plot(SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL))
#   return(result.spdf)
# }


getSPDF_UK <- function(tmp_in_name, location = "Dropbox") {
  
  # Target outcome
  # tmp_in_name = "~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/1May2021/Normal/BehaviouralBaseline/Baseline-SSP3/Baseline-SSP3-0-99-UK-Cell-2050.csv"
  # result_raw <- read.csv(paste0( tmp_in_name))
  
  result_raw = getCSV(tmp_in_name, location)
  
  result_joined = inner_join(uk_coords[, 3:4], result_raw, by = c("X_col" = "X", "Y_row" = "Y"))
  
  result_tmp = result_joined[, indicator_names_dot]
  
  
  # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
  result.spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(uk_coords$xcoord_bng, uk_coords$ycoord_bng), proj4string = crs(proj4.OSGB1936)), data = data.frame(result_tmp))# , tolerance = 0.0011)
  # plot(SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL))
  return(result.spdf)
}



getRaster<- function(fname, band.name, location = location_UK, resolution = RESOLUTION_WEB, printPath=TRUE) {
  
  
  localtif_path = paste0(path_rastercache, fname, "_", band.name, ".tif")
  band.name_dot = indicator_names_dot[match(band.name, indicator_names)]
  print(band.name_dot)
  
  if (printPath) { 
    print(localtif_path)
  }
  
  if(!file.exists(localtif_path)) {
    
    localdir_path =  dirname(localtif_path)
    if (!dir.exists(localdir_path)) {
      dir.create(localdir_path, recursive = T)
    }
    
    if (printPath) { 
      print(location)
      print(fname)
    }
    spdf.out = getSPDF_UK(fname, location = location)
    
    
    # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
    rs.LL <- stack(spdf.out)
    print(rs.LL)
    
    
    print("reprojection")
    
    # print(names(rs.LL))
    
    out.reproj = projectRaster(rs.LL[[band.name_dot]], crs = proj4.spherical, method = "ngb", res = resolution)
    writeRaster(out.reproj, filename = localtif_path, overwrite=T)
    
    print("reprojection done")  
  } else {
    out.reproj = raster(localtif_path) 
  }
  
  
  return(out.reproj)
}








# 
# # call once on a local workstation
# createChangedNumberTable <- function() { 
#    
#   # paramset = "Paramset1"
#   # scenario = "RCP8_5-SSP3"  
#   # 
#   library(doMC)
#   registerDoMC()
#   
#   library(openxlsx)
#   
#   foreach(price = foodprice_names, .errorhandling ="stop") %do% {
#     print(price)
#     
#     foreach(demand = fooddemand_names) %do% { 
#       print(demand)
#       
#       foreach(paramset = paramsets, .errorhandling = "stop") %dopar% { 
#         print(paramset)
#         
#         tb_localdir_path =  file.path(paste0("Tables/ChangedPixelNo/", price, "/", demand, "/",paramset, "/" ))
#         
#         if (!dir.exists(tb_localdir_path)) {
#           dir.create(tb_localdir_path, recursive = T)
#         }
#         
#         res1 = foreach(scenario = scenario_names, .combine = "cbind", .errorhandling="stop") %dopar% { 
#           
#           runid_tmp = which(scenario_names == scenario) - 1 
#           
#           res=  stack(lapply(target_years_other, FUN = function(year) getRaster(getFname(  paramset = paramset, scenario = scenario, fooddemand = "Normal",year =  year), 20, location = location_UK)))
#           
#           res_m = as.matrix(res)
#           res_m = res_m[!is.na(res_m[,1]),]
#           
#           tmp_changedno = sapply(2:ncol(res_m), FUN = function(x) length(res_m[res_m[,x]!=res_m[,x-1], x]))
#           names(tmp_changedno) = target_years_other[-1]
#           write.xlsx(tmp_changedno, file = paste0(tb_localdir_path, "/", scenario, "_ChangedPixelNo.xlsx"))
#           
#           return(NULL)
#         }
#         
#         
#         
#       }
#     }
#   }
#   
#   return(TRUE)
# }


# 
# 
# call once on a local workstation
# write tifs
# create a report 
createTempFiles <- function() {
  # price = "Normal"
  # demand = "Normal"
  # paramset = "Paramset1"
  # scenario = "RCP8_5-SSP3"
  
  
  endCluster()
  # library(parallel)
  library(doMC)
  registerDoMC()
  paramset_tmp = paramsets[1]
  indicator_names_in = indicator_names #  "LandUseIndex"  
  version_names_in = version_names # [1:4]
  
  default_version_byscenario
  
  # foreach(version_tmp = version_names_in, .errorhandling = "stop") %do% {
  # print(version_tmp)
  
  res1 = foreach(s_idx = seq_along(scenario_names),  .errorhandling = "stop") %dopar% {
    
    scenario = scenario_names[s_idx]
    print(scenario)
    
    version_tmp = default_version_byscenario[s_idx]
    
    
    res2 = sapply(target_years_other, FUN = function(year) sapply(indicator_names_in, FUN = function(b_name) {
      res3 = getRaster(getFname(version_tmp, paramset = paramset_tmp, scenario = scenario, year =  year), band.name =  b_name, location = location_UK, printPath = FALSE); 
      return(TRUE);
    }))
    
    print("ok")
    
    return(res2)
  }
  
  
  # }
  
  
  return(TRUE)
}


# 
# # call once on a local workstation
# createFragstatsTable <- function() { 
#   # price = "Normal"
#   # demand = "Normal"
#   # paramset = "Paramset1"
#   # scenario = "RCP8_5-SSP3"  
#   
#   library(doMC)
#   registerDoMC()
#   
#   library(openxlsx) # excel 
#   library(SDMTools) # fragmentation statistics
#   
#   foreach(price = foodprice_names, .errorhandling ="stop") %do% {
#     print(price)
#     
#     foreach(demand = fooddemand_names) %do% { 
#       print(demand)
#       
#       foreach(paramset = paramsets, .errorhandling = "stop") %dopar% { 
#         print(paramset)
#         
#         tb_localdir_path =  file.path(paste0("Tables/FragStats/", price, "/", demand, "/",paramset, "/" ))
#         
#         if (!dir.exists(tb_localdir_path)) {
#           dir.create(tb_localdir_path, recursive = T)
#         }
#         
#         res1 = foreach(scenario = scenario_names, .combine = "cbind", .errorhandling = "stop") %dopar% { 
#           
#           runid_tmp = which(scenario_names == scenario) - 1 
#           
#           
#           res_rs=  stack(lapply(target_years_other, FUN = function(year) getRaster(paste0("Data/", price, "/", demand, "/",paramset, "/", scenario  , "/", scenario  , "-",runid_tmp, "-99-EU-Cell-", year, ".csv"), 20, location = "Local")))
#           
#           res_rs_LL = projectRaster(res_rs, crs = proj4.LL, res = RES_FRAG, method = "ngb") # 0.1 
#           tmp_fragstat_m  = sapply(1:nlayers(res_rs_LL), FUN = function(x) ClassStat(res_rs_LL[[x]], cellsize = RESOLUTION_CRAFTY, bkgd = NA, latlon = T)$mean.frac.dim.index)
#           
#           
#           colnames(tmp_fragstat_m) = target_years_other
#           rownames(tmp_fragstat_m) = aft_names.8classes
#           write.xlsx(tmp_fragstat_m, file = paste0(tb_localdir_path, "/", scenario, "_FragStats.xlsx"))
#           
#           return(NULL)
#         }
#         
#         
#         
#       }
#     }
#   }
#   
#   return(TRUE)
# }



# smr = sapply(1:nlayers(spdf.default[4:22]),FUN = function(x)  summary(spdf.default[4:22][[x]], na.rm=T))
# smr.max = sapply(1:19, FUN = function(x) as.numeric(smr[[x]]["Max."])) * 1.2
# smr.min = sapply(1:19, FUN = function(x) as.numeric(smr[[x]]["Min."])) * 0.8
# 
# smr.max[!is.finite(smr.max)] = 30
# smr.min[!is.finite(smr.min)] = 0
# 

# pal.list = lapply(1:19,FUN = function(x)  colorNumeric("RdYlBu", domain = c(smr.min[x], smr.max[x]), na.color = "transparent"))
# 
# pal.list[[16]] = pal.list[[18]] =  aft.pal 


# 
# saveData <- function(data) {
#   data <- t(data)
#   
#   # Create a unique file name
#   fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
#   # Write the data to a temporary file locally
#   filePath <- file.path(tempdir(), fileName)
#   write.csv(data, filePath, row_names = FALSE, quote = TRUE)
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