


runid="0"
data_prefix = "8Apr2021/"


fname.default = "Normal/BehaviouralBaseline/Baseline/Baseline-0-99-UK-Cell-2020.csv"

getFname = function(foodprice, paramset, scenario, fooddemand, year ) { 
  
  fs::path_expand(paste0( fooddemand, "/" ,foodprice,"/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-UK-Cell-", year, ".csv"))
}


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
scenario.names = c("Baseline"
                   # , "RCP2_6-SSP1", "RCP2_6-SSP4", "RCP4_5-SSP1"
                   , "RCP4_5-SSP2", "RCP4_5-SSP4"
                   # "RCP8_5-SSP3"
                   , "RCP8_5-SSP2" , "RCP8_5-SSP5")

foodprice.names = c("") # c("Normal", "Increased", "Decreased")  # 50%
# fooddemand.names = c("Normal", "LowMeatDemand")
fooddemand.names = c("Normal", "IncFoodDemand", "DecFoodDemand")


paramsets.fullnames = c("Behavioural baseline", "Thresholds") # , "Variations (P3)", "Larger Thresholds (P4)", "Larger Variations (P5)") # , "Behavioural baseline Gu=0 (P6)",  "Behavioural baseline Gu=0.2 (P7)") #,  "Behavioural baseline YearNameFalse (P8)") 

n.paramset = length(paramsets.fullnames)
# paramsets = paste0("Paramset", 1:n.paramset)
paramsets =  c("BehaviouralBaseline", "Thresholds")


service_tb = read.csv("Tables/Services.csv") %>% as.data.frame
# serviceNames <- c("Food.crops", "Fodder.crops", "GF.redMeat", "Fuel", "Softwood", "Hardwood", "Biodiversity",
# "Carbon", "Recreation", "Flood.reg", "Employment", "Ldiversity", "GF.milk")
serviceNames = service_tb$Name


# serviceColours <- c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity" = "red", "Timber" = "tan4", "Carbon" = "darkgreen", "Urban" = "grey", "Recreation" = "orange")
# serviceColours = c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity"="turquoise", "Timber" = "tan4","Carbon"="black", "Urban" = "grey","Recreation"="dodgerblue2")
serviceColours = c("Food.crops" = "coral1", "Fodder.crops" ="goldenrod1", "GF.redMeat" = "turquoise", "Fuel" = "tan4", "Softwood" = "black", "Hardwood" = "grey", "Biodiversity" = "dodgerblue2", "Carbon"="darkgreen", "Recreation" = "orange", "Flood.reg" = "lightblue", "Employment" = "purple", "Ldiversity" = "brown", "GF.milk" = "green")


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


indicator.names = c(paste0("Service:", serviceNames), paste0("Capital:", capitalNames), "LandUseIndex") #, "Agent")


indicators_categorical = indicator.names[c(28)]





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


aft.shortnames.fromzero = as.character(aftnames$AFT)
aft.shortnames.fromzero[17] = "Unmanaged"

aft.names.fromzero =  as.character(aftnames$Description)


n_aft = length(aft.shortnames.fromzero)

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
                                       "Nbroadleaf.suit", 
                                       "Tree.suit"
))



aft_tb = read.csv("Tables/AgentColors.csv", strip.white = T, stringsAsFactors = F) %>% as.data.frame

aft_tb[aft_tb$Name == "Lazy FR", ]$Name = "Unmanaged"

aft_colors_alpha = aft_tb$Color[match( aft.shortnames.fromzero, aft_tb$Name)]

aft_colors_fromzero = col2hex(paste0("#", substr(aft_colors_alpha, start = 4, stop = 10), substr(aft_colors_alpha, start = 2, stop = 3))) # ignore alpha channel

target_years_aggcsv = seq(2020, 2100, 10)
target_years_other =  seq(2020, 2100, 10)

aft.pal <- colorFactor(col2hex(as.character(aft_colors_fromzero)),  levels = as.character(c(0:15, -1)), na.color = "transparent")

aft.pal(6)



#### 

# gugi_values 

aft_params_df_l = lapply(paramsets, FUN = function(paramset) {
  dt = read.csv(paste0("Tables/", paramset, ".csv"))
  rownames(dt) = dt$Name 
  dt[aft.shortnames.fromzero, ]
}
)


# aft_parms_df
# str(aft_parms_df)

