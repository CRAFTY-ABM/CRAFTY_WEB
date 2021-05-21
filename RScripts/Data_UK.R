library(dplyr)
library(gplots)
library(leaflet)
library(raster)

# CRAFTY run ID 
runid="0"
# A seed used in the CRAFTY runs 
seedid = "99"

# number of threads to process raster
n_thread = 4

region_names = c("England", "Scotland", "Wales")
# 
location_UK = "Local"

# dropbox relative path 
path_dropbox <- "KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/"

# local data archive
path_localstorage = paste0("~/CRAFTY_WEB_UK_DATA/")

# data version
# data_prefix = "15May2021_v4_NewProduction_SN_originaldemand/"
# data_prefix = "16May2021_v5_NewProduction_SN2_originaldemand/"
# data_prefix = "16May2021_v6_NewProduction_originaldemand/"
# data_prefix = "20May2021_v8_SusProd/"
data_prefix = "21May2021_v9_NotRemovingNegative/"

# absolute path (for local)
path_data_local = paste0(path_localstorage, data_prefix)

# relative path (for dropbox)
path_data_dropbox = paste0(path_dropbox, data_prefix)

path_shinywd = "~/shiny_tmp_dev"
path_filecache = paste0(path_shinywd, "/filetmp/")
path_rastercache = paste0(path_shinywd, "/rastertmp/")

# dummy name
default_fname = "Normal/Thresholds/Baseline/Baseline-0-99-UK-Cell-2020.csv"

getFname = function( paramset, scenario, fooddemand, year ) { 
  
  # fs::path_expand(paste0( fooddemand, "/" ,foodprice,"/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-UK-Cell-", year, ".csv"))
  fs::path_expand(paste0( "Normal/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-UK-Cell-", year, ".csv"))
  
}


scenarioname.default = "Baseline"
r_default = raster("GISData/UK_default.tif")

# ext = extent(projectRaster(r.default, crs = proj4.LL))
ext = c(-8.439121, 2.794859, 49.77235, 60.93977 )


drop_token_name = "Authentication/droptoken.rds"



#import data

# Cell ID and cooridnates 

CHESS_BNG_csv = read.csv(paste0("GISData/CHESS_1k_grid.csv")) # BNG perhaps

CHESS_BNG_csv = CHESS_BNG_csv[, c("FID", "POINT_X", "POINT_Y")]
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

uk_coords= read.csv("Tables/Cell_ID_XY_UK.csv")



# getPath()


# Scenarios (total 8)
scenario_names = c("Baseline"
                   , "RCP2_6-SSP1"
                   # "Baseline-SSP4", "Baseline-SSP5"
                   , "RCP4_5-SSP2", "RCP4_5-SSP4"
                   , "RCP6_0-SSP3"
                   , "RCP8_5-SSP2" , "RCP8_5-SSP5")

foodprice_names = c("") # c("Normal", "Increased", "Decreased")  # 50%
# fooddemand_names = c("Normal", "LowMeatDemand")
fooddemand_names = c("Normal")#, "IncFoodDemand", "DecFoodDemand")


paramsets_fullnames = c("Thresholds") #"Behavioural baseline",  , "Variations (P3)", "Larger Thresholds (P4)", "Larger Variations (P5)") # , "Behavioural baseline Gu=0 (P6)",  "Behavioural baseline Gu=0.2 (P7)") #,  "Behavioural baseline YearNameFalse (P8)") 

n_paramset = length(paramsets_fullnames)
# paramsets = paste0("Paramset", 1:n.paramset)
paramsets =  c("Thresholds") # "BehaviouralBaseline", 


service_tb = read.csv("Tables/Services.csv") %>% as.data.frame
# serviceNames <- c("Food.crops", "Fodder.crops", "GF.redMeat", "Fuel", "Softwood", "Hardwood", "Biodiversity",
# "Carbon", "Recreation", "Flood.reg", "Employment", "Ldiversity", "GF.milk")
serviceNames = service_tb$Name


# serviceColours <- c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity" = "red", "Timber" = "tan4", "Carbon" = "darkgreen", "Urban" = "grey", "Recreation" = "orange")
# serviceColours = c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity"="turquoise", "Timber" = "tan4","Carbon"="black", "Urban" = "grey","Recreation"="dodgerblue2")
serviceColours = c("Food.crops" = "coral1", "Fodder.crops" ="goldenrod1", "GF.redMeat" = "turquoise", "Fuel" = "tan4", "Softwood" = "black", "Hardwood" = "grey", "Biodiversity" = "dodgerblue2", "Carbon"="darkgreen", "Recreation" = "orange", "Flood.reg" = "lightblue", "Employment" = "purple", "Ldiversity" = "brown", "GF.milk" = "green", "Sus.Prod" = "pink")


capital_tb = read.csv("Tables/Capitals.csv") %>% as.data.frame
capitalNames = capital_tb$Name

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







# aft.colors = rich.colors(17)
# aft.shortnames.fromzero <- c( "Ext_AF", "IA", "Int_AF", "Int_Fa", "IP", "MF", "Min_man", "Mix_Fa", "Mix_For", "Mix_P", "Multifun", "P-Ur", "UL", "UMF", "Ur", "VEP", "EP")
# 
# 
# # aftNames <- c("IA","Int_Fa","Mix_Fa","Int_AF","Ext_AF","MF", "Mix_For","UMF","IP","EP","Mix_P","VEP","Multifun","Min_man","UL","P-Ur","Ur", "Lazy FR")
# 
# aft.colors.fromzero <-  (c("Ext_AF" = "yellowgreen", "IA"  = "yellow1", "Int_AF" =  "darkolivegreen1", "Int_Fa" = "lightgoldenrod1",  "IP" = "red1", "MF" =  "green3", "Min_man" = "lightyellow3",  "Mix_Fa" = "darkgoldenrod",  "Mix_For" = "green4",   "Mix_P" = "violetred",  "Multifun" = "blueviolet", "P-Ur"="lightslategrey", "UL" = "grey", "UMF" = "darkgreen", "Ur" = "black", "VEP" = "red4", "EP" = "red3")) # , "Lazy FR" = "black")
# 
# aft.names.fromzero <- c("Ext. agro-forestry","Int. arable","Int. agro-forestry","Int. mixed farming","Int. pastoral","Managed forest","Minimal management",
#                         "Ext. mixed farming","Mixed forest","Mixed pastoral","Multifunctional","Peri-Urban", "Unmanaged land","Umanaged forest","Urban", "Very ext. pastoral","Ext. pastoral")



capital_colours =  (c("Ext_AF" = "yellowgreen", "IA"  = "yellow1", "Int_AF" =  "darkolivegreen1", "Int_Fa" = "lightgoldenrod1",  "IP" = "red1", "MF" =  "green3", "Min_man" = "lightyellow3",  "Mix_Fa" = "darkgoldenrod",  "Mix_For" = "green4",   "Mix_P" = "violetred",  "Multifun" = "blueviolet", "NNBroadleaf"="orange", "NBroadleaf" = "lightblue", "UMF" = "darkgreen", "Ur" = "black", "VEP" = "red4", "EP" = "red3")) # , "Lazy FR" = "black")
# 






aftnames = data.frame(rbind(c("AF", "Agroforestry", "Agroforestry"),
                            c("Bioenergy", "Bioenergy", "Bioenergy"),
                            c("EA", "EA", "Extensive Agriculture"),
                            c("EP", "Ext_Pastoral", "Extensive Pastoral"),
                            c("IAfodder", "IAfodder", "Intensive Agriculture Fodder"),
                            c("IAfood", "IAfood", "Intensive Agriculture Food"),
                            c("IP", "Int_Pastoral", "Intensive Pastoral"),
                            c("MW", "Mixed woodland", "Mixed woodland"),
                            c("NWCons", "NWCons", "Natural woodland Conservation"),
                            c("PNB", "PNB", "Productive N Broadleaf"),
                            c("PNC", "PNC", "Productive N Conifer"),
                            c("PNNB", "PNNB", "Productive NN Broadleaf"),
                            c("PNNC", "PNNC", "Productive NN Conifer"),
                            c("SusAr", "SusAr", "Sustainable Agriculture"),
                            c("VEP", "V_Ext_Pastoral", "Very Extensive Pastoral"),
                            c("Urban", "Urban", "Urban"),
                            c("NOT_ASSIGNED", "Unmanaged", "Unmanaged")
))

colnames(aftnames) = c("AFT", "AFT_cb", "Description")


aft_shortnames_fromzero = as.character(aftnames$AFT)
aft_shortnames_fromzero[17] = "Unmanaged"

aft_names_fromzero =  as.character(aftnames$Description)


n_aft = length(aft_shortnames_fromzero)

capital_names = data.frame(Capital = c("Human", 
                                       "Social", 
                                       "Manufactured", 
                                       "Financial", 
                                       "Arable.suit", 
                                       "Igrass.suit", 
                                       "SNGrass.suit", 
                                       "Bioenergy.suit", 
                                       "AgroForestry.suit", 
                                       "NNConifer.suit", 
                                       "NConifer.suit", 
                                       "NNBroadleaf.suit", 
                                       "NBroadleaf.suit", 
                                       "Tree.suit"
))



aft_tb = read.csv("Tables/AgentColors.csv", strip.white = T, stringsAsFactors = F) %>% as.data.frame

aft_tb[aft_tb$Name == "Lazy FR", ]$Name = "Unmanaged"



aft_colors_alpha = aft_tb$Color[match( aft_shortnames_fromzero, aft_tb$Name)]

aft_colors_fromzero = col2hex(paste0("#", substr(aft_colors_alpha, start = 4, stop = 10), substr(aft_colors_alpha, start = 2, stop = 3))) # ignore alpha channel

# 17 colours
aft_colors_fromzero_17 = aft_colors_fromzero

# reduced colours
aft_colors_fromzero[aft_shortnames_fromzero %in% c("PNNB", "PNC", "PNNC", "PNB", "MW")] = col2hex("darkblue")


target_years_aggcsv = seq(2020, 2100, 10)
target_years_other =  seq(2020, 2100, 10)



aft_colors_fromzero_ts = aft_colors_fromzero
aft_colors_fromzero_ts[17] = "black" 
aft_lty_ts = c(rep(1, 11), 2)

n_cell_total = nrow(uk_coords)


aft_pal <- colorFactor(col2hex(as.character(aft_colors_fromzero)),  levels = as.character(c(0:15, -1)), na.color = "transparent")

# aft.pal(6)


# reduced
aft_group_colors =  aft_colors_fromzero_17[ c(1:5, 7:9, 14:17)]
aft_group_colors[7] = "darkblue"
aft_group_colors[12] = "black"

aft_group_names = c( aft_names_fromzero)[ c(1:5, 7:9, 14:17)]
aft_group_names[5] = "Intensive Agriculture"
aft_group_names[7] = "Productive Woodland"
aft_group_shortnames = c( aft_shortnames_fromzero  )[ c(1:5, 7:9, 14:17)]
aft_group_shortnames[5] = "IA"
aft_group_shortnames[7] = "PW"

#### 

# gugi_values 

# aft_params_df_l = lapply(paramsets, FUN = function(paramset) {
#   dt = read.csv(paste0("Tables/", paramset, ".csv"))
#   rownames(dt) = dt$Name 
#   dt[aft.shortnames.fromzero, ]
# }
# )


# aft_parms_df
# str(aft_parms_df)

