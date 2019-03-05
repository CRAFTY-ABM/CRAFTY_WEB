library(rdrop2)
library(gplots)
library(shiny)
library(raster)
library(RColorBrewer)
library(gplots)
library(rgdal)
library(rgeos)

library(leaflet)
library(dplyr)
library(leaflet.extras)

# Scenarios (total 8)
scenario.names = c("Baseline", "RCP2_6-SSP1", "RCP2_6-SSP4", "RCP4_5-SSP1", "RCP4_5-SSP3", "RCP4_5-SSP4", "RCP8_5-SSP3", "RCP8_5-SSP5")
paramsets = paste0("Paramset", 1:5)
paramsets.fullanems = c("Behavioural baseline", "Thresholds", "Variations", "Larger Thresholds", "Larger Variations")
indicator.names =  c("Service.Meat","Service.Crops","Service.Diversity",
                     "Service.Timber","Service.Carbon","Service.Urban",
                     "Service.Recreation","Capital.Crop.productivity","Capital.Forest.productivity",
                     "Capital.Grassland.productivity","Capital.Financial.capital","Capital.Human.capital",
                     "Capital.Social.capital","Capital.Manufactured.capital","Capital.Urban.capital",
                     "LandUse","LandUseIndex","Agent", "Competitiveness")
#   [1] "Service.Meat"                   "Service.Crops"                  "Service.Diversity"             
# [4] "Service.Timber"                 "Service.Carbon"                 "Service.Urban"                 
# [7] "Service.Recreation"             "Capital.Crop.productivity"      "Capital.Forest.productivity"   
# [10] "Capital.Grassland.productivity" "Capital.Financial.capital"      "Capital.Human.capital"         
# [13] "Capital.Social.capital"         "Capital.Manufactured.capital"   "Capital.Urban.capital"         
# [16] "LandUse"                        "LandUseIndex"                   "Agent"                         
# [19] "Competitiveness"         



# aft.colors = rich.colors(17)
aft.names.fromzero <- c( "Ext_AF", "IA", "Int_AF", "Int_Fa", "IP", "MF", "Min_man", "Mix_Fa", "Mix_For", "Mix_P", "Multifun", "P-Ur", "UL", "UMF", "Ur", "VEP", "EP")


# aftNames <- c("IA","Int_Fa","Mix_Fa","Int_AF","Ext_AF","MF", "Mix_For","UMF","IP","EP","Mix_P","VEP","Multifun","Min_man","UL","P-Ur","Ur", "Lazy FR")

aft.colors.fromzero <-  (c("Ext_AF" = "yellowgreen", "IA"  = "yellow1", "Int_AF" =  "darkolivegreen1", "Int_Fa" = "lightgoldenrod1",  "IP" = "red1", "MF" =  "green3", "Min_man" = "lightyellow3",  "Mix_Fa" = "darkgoldenrod",  "Mix_For" = "green4",   "Mix_P" = "violetred",  "Multifun" = "blueviolet", "P-Ur"="lightslategrey", "UL" = "grey", "UMF" = "darkgreen", "Ur" = "black", "VEP" = "red4", "EP" = "red3")) # , "Lazy FR" = "black")

aft.fullnames <- c("Ext. agro-forestry","Int. arable","Int. agro-forestry","Int. mixed farming","Int. pastoral","Managed forest","Minimal management",
                   "Ext. mixed farming","Mixed forest","Mixed pastoral","Multifunctional","Peri-Urban", "Unmanaged land","Umanaged forest","Urban", "Very ext. pastoral","Ext. pastoral")

# AFTColours <- c("Ext. agro-forestry" = "darkolivegreen3","Int. arable"="gold1","Int. agro-forestry"="darkolivegreen1","Int. mixed farming"="darkorange1",
# "Int. pastoral"="firebrick1","Managed forest"="chartreuse2","Minimal management"="darkgrey","Ext. mixed farming"="darkorange3","Mixed forest"="chartreuse4",
# "Mixed pastoral"="firebrick2", "Multifunctional"="dodgerblue3","Unmanaged land"="black","Umanaged forest"="darkgreen","Very ext. pastoral"="firebrick4","Ext. pastoral"="firebrick3")

target_years = seq(2020, 2090, 10)


aft.pal <- colorFactor(col2hex(as.character(aft.colors.fromzero)), levels = 0:17, na.color = "transparent") # "#0C2C84", "#41B6C4", "#FFFFCC"), # , bins = 17) 




path.wd <- ("KIT_Modelling/CRAFTY/crafty_web/")
path.serverwd = getwd()
path.droptmp = paste0(path.serverwd, "/droptmp/")

if(!dir.exists(path.droptmp)) { 
  dir.create(path.droptmp)
}

authDropbox <- function() {
  token<-drop_auth()
  saveRDS(token, "droptoken.rds")
}

accessDropbox <- function() { 
  token <- readRDS("droptoken.rds") 
  drop_acc(dtoken = token)
  
}

# A seed used in the CRAFTY runs 
seedid = "99"

# Lon-Lat projection 
proj4.LL <- CRS("+proj=longlat +datum=WGS84")

# Proj4js.defs["EPSG:3035"] etrs89/etrs-laea
# Scope: Single CRS for all Europe. Used for statistical mapping at all scales and other purposes where true area representation is required.
# Reference: http://spatialreference.org/ref/epsg/3035/
proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";


# result<-read.csv("C:/Users/brown-c/Documents/Work_docs/IMPRESSIONS/CRAFTY Europe IMPRESSIONS/Results/Raw results/Baseline/Baseline-0-0-EU-Cell-2096.csv")
# result<-read.csv("~/Dropbox/KIT_Modelling/CRAFTY/Calibration/ model runs_old/Baseline-0-99-EU-Cell-2016.csv")


#import data

# Cell ID and cooridnates 
# ctry.ids <- read.csv("~/Dropbox/KIT/CLIMSAVE/IAP/Cell_ID_LatLong.csv")
# saveRDS(ctry.ids, file = "ctry.ids.Rds")
ctry.ids = readRDS("ctry.ids.Rds")
x.lat.v = sort(unique(ctry.ids$Longitude))
y.lon.v = sort(unique(ctry.ids$Latitude))


# simple caching
getCSV = function(tmp.in.name) { 
  
  localfile_path =  paste0(path.droptmp, tmp.in.name)
  
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


getSPDF <- function(tmp.in.name) {
  
  # Target outcome
  # result.tmp <- read.csv(paste0( tmp.in.name))
  result.tmp = getCSV(tmp.in.name)
  result.tmp$lon = x.lat.v[result.tmp$X]
  result.tmp$lat = y.lon.v[result.tmp$Y]
  
  # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
  result.spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL), data = data.frame(result.tmp), tolerance = 0.0011)
  # plot(SpatialPoints(cbind(result.tmp$lon, result.tmp$lat), proj4string = proj4.LL))
  return(result.spdf)
}

scenarioname.default = "Baseline"

fname.default = (paste0("Data/Paramset1/", scenarioname.default, "/", scenarioname.default, "-0-99-EU-Cell-2016.csv"))
spdf.default = getSPDF(fname.default)
rs.LL <- stack(spdf.default)[[4:22]]
agent.LL = rs.LL[[17]]
r.default = projectRaster(agent.LL, crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E4)

# getRaster(fname.default, band.idx = 18)


getRaster<- function(fname, band.idx) {
  
  localfile_path = paste0("rastertmp/",fname, "_", band.idx, ".tif")
  
  if(!file.exists(localfile_path)) {
    localdir_path =  dirname(localfile_path)
    if (!dir.exists(localdir_path)) {
      dir.create(localdir_path, recursive = T)
    }
    spdf.out = getSPDF(fname)
    rs.LL <- stack(spdf.out)[[4:22]]
    out.reproj = projectRaster(rs.LL[[band.idx]], crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", method = "ngb", res = 1E4)
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



smr = sapply(1:nlayers(spdf.default[4:22]),FUN = function(x)  summary(spdf.default[4:22][[x]]))
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