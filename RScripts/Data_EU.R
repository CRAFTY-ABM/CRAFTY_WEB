# dropbox relative path 
path_dropbox <- "KIT_Modelling/CRAFTY/CRAFTY_WEB_EU_DATA/"

# local data archive
# path_localstorage = paste0("~/CRAFTY_WEB_UK_DATA/")


node_name = Sys.info()["nodename"]

if (node_name == "buntu") { 
  path_localstorage = paste0("/DATA80G/Andrea-Madingley runs/")
} else { 
  path_localstorage = paste0("/Users/seo-b/Nextcloud/AndreaS/CRAFTY results/Final runs/Andrea-Madingley runs/")
}




# British National Grid
# proj4.BNG = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.1502,0.247,0.8421,-20.4894 +units=m +no_defs"
# proj4.OSGB1936 ="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"  # proj4string(LAD2019_shp) # EPSG:27700
# 

MODEL_INFO = character()
MODEL_INFO$NAME = "CRAFTY-EU"
MODEL_INFO$TITLE ="CRAFTY-EU interactive web-interface"
MODEL_INFO$AFT_TB_NAME = "Tables/AFT_Names_EU.csv"
MODEL_INFO$REGIONNAME = "EU"
MODEL_INFO$PREFIX = "EU"
MODEL_INFO$CRS = proj4.LL # proj4.OSGB1936 # UK
MODEL_INFO$CRS_UI = proj4.UI

MODEL_INFO$RESOLUTION_UI = 1.5E4 # 10/60




# CRAFTY run ID 
MODEL_INFO$runid="0"
# A seed used in the CRAFTY runs 
MODEL_INFO$seedid = "99"


MODEL_INFO$Xcol = "X"
MODEL_INFO$Yrow = "Y"

MODEL_INFO$Xcol_csv = "X"
MODEL_INFO$Yrow_csv = "Y"

target_years_aggcsv = seq(2016, 2086, 10)
target_years_other =  seq(2016, 2086, 10)

MODEL_INFO$YEAR_DEFAULT = target_years_other[1]
MODEL_INFO$YEAR_START = target_years_other[1]
MODEL_INFO$YEAR_END = target_years_other[length(target_years_other)]

MODEL_INFO$paramset_default = ""


# number of threads to process raster
n_thread = 6

MODEL_INFO$region_names = c("England", "Scotland", "Wales")
# 
MODEL_INFO$location = "Local"

# AFT
MODEL_INFO$AFT_TB = read.csv(MODEL_INFO$AFT_TB_NAME) 
MODEL_INFO$N_AFT = nrow(MODEL_INFO$AFT_TB )  # including NOT_ASSIGNED = UL



# MODEL_INFO$coords = read.csv("Tables/Cell_ID_XY_UK.csv")
MODEL_INFO$coords = read.csv("Tables/Cell_ID_XY_EU.csv")


MODEL_COORDS_SPDF_LL =  readRDS("GISData/ctry.ids.Rds")[,c("Longitude", "Latitude")]

MODEL_INFO$SP_LL = SpatialPoints(MODEL_COORDS_SPDF_LL, proj4string = proj4.LL)
# MODEL_INFO$COORDS_SPDF = coordinates(spTransform(MODEL_INFO$COORDS_SP_LL, CRSobj = MODEL_INFO$CRS_SPDF))


MODEL_INFO$n_cell_total = nrow(MODEL_INFO$coords)

MODEL_SHP = readOGR("GISData/CLIMSAVE_EuropeRegions_for_SA.shp")
MODEL_INFO$AREA_CELL = gArea(MODEL_SHP, byid = T)

# data version
MODEL_INFO$data_prefix = ""




# absolute path (for local)
path_data_local = paste0(path_localstorage, MODEL_INFO$data_prefix)

# relative path (for dropbox)
path_data_dropbox = paste0(path_dropbox, MODEL_INFO$data_prefix)

path_shinywd = "~/shiny_tmp_dev"
path_filecache = paste0(path_shinywd, "/filetmp/")
path_rastercache = paste0(path_shinywd, "/rastertmp/")


# Scenarios (total 8)
MODEL_INFO$scenario_names = c("Baseline - EPAs"
                              , "Baseline - NPAs"
                              , "Baseline - NPASs"
                              , "Baseline - AllPAs"
                              , "RCP2_6-SSP1 - EPAs"
                              , "RCP2_6-SSP1 - NPAs"
                              , "RCP2_6-SSP1 - NPASs"
                              , "RCP2_6-SSP1 - AllPAs"
                              , "RCP4_5-SSP3 - EPAs"
                              , "RCP4_5-SSP3 - NPAs"
                              , "RCP4_5-SSP3 - NPASs" 
                              , "RCP4_5-SSP3 - AllPAs"
                              , "RCP8_5-SSP5 - EPAs"
                              , "RCP8_5-SSP5 - NPAs"
                              , "RCP8_5-SSP5 - NPASs"
                              , "RCP8_5-SSP5 - AllPAs")


MODEL_INFO$scenario_names_rcp = str_split(MODEL_INFO$scenario_names, pattern = " - ", simplify = T)[,1]
MODEL_INFO$scenario_names_pa = str_split(MODEL_INFO$scenario_names, pattern = " - ", simplify = T)[,2]


MODEL_INFO$scenario_names_full = MODEL_INFO$scenario_names 

selected_scenario_current = MODEL_INFO$scenario_names_full[1]
selected_scenario_future = MODEL_INFO$scenario_names_full[5]

# 
version_names = c("", "")#,"Default_v18", "PenalsingOverProduction_v18", "Default_v17", "PenalsingOverProduction_v17", "Default_v16", "PenalsingOverProduction_v16", "Default_v15", "PenalsingOverProduction_v15")#, "Default_v14", "PenalsingOverProduction_v16")#, "New SN", "New SN with Penalising")

version_prefix =c("", "")#,"Removal_v18", "NoRemoval_v18","Removal_v17", "NoRemoval_v17","Removal_v16", "NoRemoval_v16", "Removal_v15", "NoRemoval_v15")#, "Removal_v14", "NoRemoval_v14")#, "NewSN_Removal", "NewSN_NoRemoval") 

version_default_idx = 1
version_default = version_names[version_default_idx]


# default_version_byscenario = version_names[c(2,2,2,1,1,2,1)] # UK
default_version_byscenario = version_names[rep(1, length(MODEL_INFO$scenario_names))] # EU





MODEL_INFO$scenario_default = selected_scenario_current

# scenarioname.default = "Baseline"
# r_default = raster("GISData/UK_default.tif") # UK
r_default_list = readRDS("GISData/Baseline - EPAs.Rds")   # EU
r_default = projectRaster(r_default_list[[1]]$LandUseIndex, crs = proj4.UI, method = "ngb")

# ext = extent(projectRaster(r_default, crs =proj4.LL))
# MODEL_INFO$extent = as.vector(ext)

# MODEL_INFO$extent = c(-8.439121, 2.794859, 49.77235, 60.93977 ) # UK
MODEL_INFO$extent = c(-20, 40, 29.37785, 75.14285 ) # EU





# dummy name
# default_fname = paste0(version_default, "/Thresholds/Baseline/Baseline-0-", MODEL_INFO$seedid, "-", MODEL_INFO$PREFIX, "-Cell-2020.csv")

getFname = function(version, paramset, scenario, year ) { 
  
  # fs::path_expand(paste0( fooddemand, "/" ,foodprice,"/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-UK-Cell-", year, ".csv"))
  fs::path_expand(paste0(version_prefix[match(version,version_names)], "/", paramset, "/", scenario, "/", MODEL_INFO$scenario_names_rcp[which(MODEL_INFO$scenario_names == scenario)], "-", MODEL_INFO$runid, "-", MODEL_INFO$seedid, "-", MODEL_INFO$PREFIX, "-Cell-", year, ".csv"))
  
}

default_fname = getFname(version_default, MODEL_INFO$paramset_default, MODEL_INFO$scenario_default, MODEL_INFO$year_default)



getAFTCSVname = function(version, paramset, scenario ) { 
  
  # fs::path_expand(paste0( fooddemand, "/" ,foodprice,"/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-UK-Cell-", year, ".csv"))
  fs::path_expand(paste0(version_prefix[match(version,version_names)], "/", paramset, "/", scenario, "/", MODEL_INFO$scenario_names_rcp[which(MODEL_INFO$scenario_names == scenario)], "-", MODEL_INFO$runid, "-", MODEL_INFO$seedid, "-", MODEL_INFO$PREFIX, "-AggregateAFTComposition.csv"))
  
}

getServiceCSVname = function(version, paramset, scenario ) { 
  
  # fs::path_expand(paste0( fooddemand, "/" ,foodprice,"/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-UK-Cell-", year, ".csv"))
  fs::path_expand(paste0(version_prefix[match(version,version_names)], "/", paramset, "/", scenario, "/", MODEL_INFO$scenario_names_rcp[which(MODEL_INFO$scenario_names == scenario)], "-", MODEL_INFO$runid, "-", MODEL_INFO$seedid, "-", MODEL_INFO$PREFIX, "-AggregateServiceDemand.csv"))
  
}

#import data

# Cell ID and cooridnates 
# CHESS_BNG_csv = read.csv(paste0("GISData/CHESS_1k_grid.csv")) # BNG perhaps

# CHESS_BNG_csv = CHESS_BNG_csv[, c("FID", "POINT_X", "POINT_Y")]
# CHESS_BNG_sp = SpatialPixels(points = SpatialPoints(cbind(CHESS_BNG_csv$POINT_X, CHESS_BNG_csv$POINT_Y), proj4string =  crs(proj4.BNG)))
#  
# CHESS_BNG_r  = raster(SpatialPixelsDataFrame(CHESS_BNG_sp, data =data.frame(CHESS_BNG_csv$FID)))
# # plot(UK_BNG_r)
# 
# CHESS_mask_r = CHESS_BNG_r
# CHESS_mask_r[!is.na(CHESS_mask_r)] = 1 
# 
# CHESS_LL_sp = spTransform(CHESS_BNG_sp, CRSobj = crs(proj4.LL))
# CHESS_LL_coords = data.frame(coordinates(CHESS_LL_sp))
# colnames(CHESS_LL_coords) = c("Longitude", "Latitude")




# getPath()



MODEL_INFO$paramsets_fullnames = c("") #"Behavioural baseline",  , "Variations (P3)", "Larger Thresholds (P4)", "Larger Variations (P5)") # , "Behavioural baseline Gu=0 (P6)",  "Behavioural baseline Gu=0.2 (P7)") #,  "Behavioural baseline YearNameFalse (P8)")

# paramsets_fullnames = c("Thresholds") #"Behavioural baseline",  , "Variations (P3)", "Larger Thresholds (P4)", "Larger Variations (P5)") # , "Behavioural baseline Gu=0 (P6)",  "Behavioural baseline Gu=0.2 (P7)") #,  "Behavioural baseline YearNameFalse (P8)")  # UK

n_paramset = length(MODEL_INFO$paramsets_fullnames)
# paramsets = paste0("Paramset", 1:n.paramset)

MODEL_INFO$paramsets =  c("") # "BehaviouralBaseline", 
# paramsets =  c("Thresholds") # "BehaviouralBaseline",  # UK

p_idx_default = 1 

service_tb = read.csv("Tables/Services_EU.csv") %>% as.data.frame
# serviceNames <- c("Food.crops", "Fodder.crops", "GF.redMeat", "Fuel", "Softwood", "Hardwood", "Biodiversity",
# "Carbon", "Recreation", "Flood.reg", "Employment", "Ldiversity", "GF.milk")
serviceNames = service_tb$Name

# serviceNames_full = service_tb$Fullname
serviceNames_full = serviceNames
# 

# serviceColours <- c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity" = "red", "Timber" = "tan4", "Carbon" = "darkgreen", "Urban" = "grey", "Recreation" = "orange") #  EU
serviceColours = c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity"="turquoise", "Timber" = "tan4","Carbon"="black", "Urban" = "grey","Recreation"="dodgerblue2") # EU
# serviceColours = c("Food.crops" = "coral1", "Fodder.crops" ="goldenrod1", "GF.redMeat" = "turquoise", "Fuel" = "tan4", "Softwood" = "black", "Hardwood" = "grey", "Biodiversity" = "dodgerblue2", "Carbon"="darkgreen", "Recreation" = "orange", "Flood.reg" = "lightblue", "Employment" = "purple", "Ldiversity" = "brown", "GF.milk" = "green", "Sus.Prod" = "pink") # UK


capital_tb = read.csv("Tables/Capitals_EU.csv") %>% as.data.frame
capitalNames = capital_tb$Name
capitalNames_full = capitalNames # capital_tb$Fullname






# 
# indicator.names =  c("Service.Meat","Service.Crops","Service.Diversity",
#                      "Service.Timber","Service.Carbon","Service.Urban",
#                      "Service.Recreation","Crop.productivity","Forest.productivity",
#                      "Grassland.productivity","Financial.capital","Human.capital",
#                      "Social.capital","Manufactured.capital","Urban.capital",
#                      "LandUse_notuse","Land Use (17 AFTs)","Agent_notuse", "Competitiveness", "Land Use (8 AFTs)")




# Tick,X,Y,Service:Food.crops,Service:Fodder.crops,Service:GF.redMeat,Service:Fuel,Service:Softwood,Service:Hardwood,Service:Biodiversity,Service:Carbon,Service:Recreation,Service:Flood.reg,Service:Employment,Service:Ldiversity,Service:GF.milk,Capital:Human,Capital:Social,Capital:Manufactured,Capital:Financial,Capital:Arable.suit,Capital:Igrass.suit,Capital:SNGrass.suit,Capital:Bioenergy.suit,Capital:AgroForestry.suit,Capital:NNConifer.suit,Capital:NConifer.suit,Capital:NNBroadleaf.suit,Capital:Nbroadleaf.suit,Capital:Tree.suit,LandUseIndex,Agent

indicator_names = c(paste0("Service:", serviceNames), paste0("Capital:", capitalNames), "LandUseIndex") #, "Agent")
indicator_names_dot = c(paste0("Service.", serviceNames), paste0("Capital.", capitalNames), "LandUseIndex") #, "Agent")

indicator_names_full = c(serviceNames_full, capitalNames_full, "Land Use (agent types)")

MAPTITLE_DEFAULT = "Land Use (Agent Types)"





aft.shortnames.fromzero <- c( "Ext_AF", "IA", "Int_AF", "Int_Fa", "IP", "MF", "Min_man", "Mix_Fa", "Mix_For", "Mix_P", "Multifun", "P-Ur", "UL", "UMF", "Ur", "VEP", "EP")
# 
  
aft_colours_fromzero <-  (c("Ext_AF" = "yellowgreen", "IA"  = "yellow1", "Int_AF" =  "darkolivegreen1", "Int_Fa" = "lightgoldenrod1",  "IP" = "red1", "MF" =  "green3", "Min_man" = "lightyellow3",  "Mix_Fa" = "darkgoldenrod",  "Mix_For" = "green4",   "Mix_P" = "violetred",  "Multifun" = "blueviolet", "P-Ur"="lightslategrey", "UL" = "grey", "UMF" = "darkgreen", "Ur" = "black", "VEP" = "red4", "EP" = "red3")) # , "Lazy FR" = "black")
# 
aft_fullnames_fromzero <- c("Ext. agro-forestry","Int. arable","Int. agro-forestry","Int. mixed farming","Int. pastoral","Managed forest","Minimal management",
                            "Ext. mixed farming","Mixed forest","Mixed pastoral","Multifunctional","Peri-Urban", "Unmanaged land","Umanaged forest","Urban", "Very ext. pastoral","Ext. pastoral")



# capital_colours =  (c("Ext_AF" = "yellowgreen", "IA"  = "yellow1", "Int_AF" =  "darkolivegreen1", "Int_Fa" = "lightgoldenrod1",  "IP" = "red1", "MF" =  "green3", "Min_man" = "lightyellow3",  "Mix_Fa" = "darkgoldenrod",  "Mix_For" = "green4",   "Mix_P" = "violetred",  "Multifun" = "blueviolet", "NNBroadleaf"="orange", "NBroadleaf" = "lightblue", "UMF" = "darkgreen", "Ur" = "black", "VEP" = "red4", "EP" = "red3")) # , "Lazy FR" = "black")
# # 
#   

aft_names_default =  aft_fullnames_fromzero #  c("IA","Int_Fa","Mix_Fa","Int_AF","Ext_AF","MF", "Mix_For","UMF","IP","EP","Mix_P","VEP","Multifun","Min_man","UL","P-Ur","Ur") #, "Lazy FR") # EU
aft_names_default_withLazyFR = c("Lazy FR", aft_names_default) # from -1

aft_colours_default =  aft_colours_fromzero
aft_colours_default_withLazyFR = c("Lazy FR" = "black", aft_colours_fromzero)

# aft_pal_default = colorFactor(col2hex(as.character(aft_colours_default)),  levels = as.character(c(0:length(aft_names_default), -1)), na.color = "transparent")
aft_pal_from_zero = colorFactor(col2hex(as.character(aft_colours_default)), levels = as.character(c(0:15, -1)), na.color = "transparent") # 

aft_pal_default = aft_pal_from_zero


MODEL_INFO$AFT_LEVELS = -1:16
MODEL_INFO$AFT_LEVELS_TRANSITION = 0:16


# aftnames = data.frame(rbind(c("AF", "Agroforestry", "Agroforestry"),
#                             c("Bioenergy", "Bioenergy", "Bioenergy"),
#                             c("EA", "EA", "Extensive Agriculture"),
#                             c("EP", "Ext_Pastoral", "Extensive Pastoral"),
#                             c("IAfodder", "IAfodder", "Intensive Agriculture Fodder"),
#                             c("IAfood", "IAfood", "Intensive Agriculture Food"),
#                             c("IP", "Int_Pastoral", "Intensive Pastoral"),
#                             c("MW", "Mixed woodland", "Mixed woodland"),
#                             c("NWCons", "NWCons", "Natural woodland Conservation"),
#                             c("PNB", "PNB", "Productive N Broadleaf"),
#                             c("PNC", "PNC", "Productive N Conifer"),
#                             c("PNNB", "PNNB", "Productive NN Broadleaf"),
#                             c("PNNC", "PNNC", "Productive NN Conifer"),
#                             c("SusAr", "SusAr", "Sustainable Agriculture"),
#                             c("VEP", "V_Ext_Pastoral", "Very Extensive Pastoral"),
#                             c("Urban", "Urban", "Urban"),
#                             c("NOT_ASSIGNED", "Unmanaged", "Unmanaged")
# ))

# colnames(aftnames) = c("AFT", "AFT_cb", "Description")


# aft_shortnames_fromzero = as.character(aftnames$AFT)
# aft_shortnames_fromzero[17] = "Unmanaged"
# 
# aft_names_fromzero =  as.character(aftnames$Description)
# 
# 
# n_aft = length(aft_shortnames_fromzero)
# 



# 
# aft_tb = read.csv("Tables/AgentColors_v2.csv", strip.white = T, stringsAsFactors = F) %>% as.data.frame
# 
# aft_tb[aft_tb$Name == "Lazy FR", ]$Name = "Unmanaged"


# aftNames<-c("Intensive arable food/fodder","Extensive arable", "Sustainable arable", "Productive broadleaf","Productive conifer","Mixed woodland","Conservation","Intensive pastoral","Extensive pastoral","Very extensive pastoral","Agro-forestry","Bioenergy","Urban","Unmanaged")
# aftColours<-c("#E3C16B","#91714C","#C0BCAE","#BDED50","#268c20","#215737","#0a1c01","#F3EF0C","#BCB918","#7d7d47","#28b1c9","#2432d1","#EE0F05","#fafaf7")

# aft_colors_alpha = aft_tb$Color[match( aft_shortnames_fromzero, aft_tb$Name)]
# 
# aft_colors_fromzero = col2hex(paste0("#", substr(aft_colors_alpha, start = 4, stop = 10), substr(aft_colors_alpha, start = 2, stop = 3))) # ignore alpha channel
# 
# # 17 colours
# aft_colors_fromzero_17 = aft_colors_fromzero

# reduced colours
# aft_colors_fromzero[aft_shortnames_fromzero %in% c("PNNB", "PNC", "PNNC", "PNB", "MW")] = col2hex("darkblue")




# aft_pal <- colorFactor(col2hex(as.character(aft_colors_fromzero)),  levels = as.character(c(0:15, -1)), na.color = "transparent")
# aft_pal_group2 <- colorFactor(col2hex(as.character(aft_group2_colours_17)),  levels = as.character(c(0:15, -1)), na.color = "transparent")
 




# aft_colors_fromzero_ts = aft_colors_fromzero
# aft_colors_fromzero_ts[17] = "black" 
# aft_lty_ts = c(rep(1, 13), 2)


 

### Transition plot 
TRANSITION_COLOURS = aft_colours_default 
TRANSITION_NAMES = aft_names_default 



## reclass labels before visualisation (@todo to functions)

reclassLabels <- function(labels_in) { 
  
  labels_out = labels_in
  labels_out[labels_out==-1] = 13 # Lazy FR to UL
  
  return(labels_out)
}



