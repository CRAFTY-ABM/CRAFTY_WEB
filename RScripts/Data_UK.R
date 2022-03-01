library(dplyr)
library(gplots)
library(leaflet)
library(raster)

# CRAFTY run ID 
runid="0"
# A seed used in the CRAFTY runs 
seedid = "99"

# number of threads to process raster
n_thread = 6

region_names = c("England", "Scotland", "Wales")
# 
location_UK = "Local"

# dropbox relative path 
path_dropbox <- "KIT_Modelling/CRAFTY/CRAFTY_WEB_UK_DATA/"

# local data archive
path_localstorage = paste0("~/CRAFTY_WEB_UK_DATA/")

# data version
data_prefix = ""

version_names = c("Default_v26", "PenalsingOverProduction_v26")#,"Default_v18", "PenalsingOverProduction_v18", "Default_v17", "PenalsingOverProduction_v17", "Default_v16", "PenalsingOverProduction_v16", "Default_v15", "PenalsingOverProduction_v15")#, "Default_v14", "PenalsingOverProduction_v16")#, "New SN", "New SN with Penalising")

version_prefix =c("Removal_v26", "NoRemoval_v26")#,"Removal_v18", "NoRemoval_v18","Removal_v17", "NoRemoval_v17","Removal_v16", "NoRemoval_v16", "Removal_v15", "NoRemoval_v15")#, "Removal_v14", "NoRemoval_v14")#, "NewSN_Removal", "NewSN_NoRemoval") 

version_default_idx = 1
version_default = version_names[version_default_idx]


default_version_byscenario = version_names[c(2,2,2,1,1,2,1)]


# absolute path (for local)
path_data_local = paste0(path_localstorage, data_prefix)

# relative path (for dropbox)
path_data_dropbox = paste0(path_dropbox, data_prefix)

path_shinywd = "~/shiny_tmp_dev"
path_filecache = paste0(path_shinywd, "/filetmp/")
path_rastercache = paste0(path_shinywd, "/rastertmp/")

# dummy name
default_fname = paste0(version_default, "/Thresholds/Baseline/Baseline-0-99-UK-Cell-2020.csv")

getFname = function(version, paramset, scenario, year ) { 
  
  # fs::path_expand(paste0( fooddemand, "/" ,foodprice,"/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-UK-Cell-", year, ".csv"))
  fs::path_expand(paste0(version_prefix[match(version,version_names)], "/", paramset, "/", scenario, "/", scenario, "-", runid, "-99-UK-Cell-", year, ".csv"))
  
}


# Scenarios (total 8)
scenario_names = c("Baseline"
                   , "RCP2_6-SSP1"
                   , "RCP4_5-SSP2", "RCP4_5-SSP4"
                   , "RCP6_0-SSP3"
                   , "RCP8_5-SSP2" , "RCP8_5-SSP5")

scenario_names_full = c("Baseline"
                   , "RCP2.6-SSP1"
                   , "RCP4.5-SSP2", "RCP4.5-SSP4"
                   , "RCP6.0-SSP3"
                   , "RCP8.5-SSP2" , "RCP8.5-SSP5")

selected_scenario_current = scenario_names_full[1]
selected_scenario_future = scenario_names_full[4]


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




paramsets_fullnames = c("Thresholds") #"Behavioural baseline",  , "Variations (P3)", "Larger Thresholds (P4)", "Larger Variations (P5)") # , "Behavioural baseline Gu=0 (P6)",  "Behavioural baseline Gu=0.2 (P7)") #,  "Behavioural baseline YearNameFalse (P8)") 

n_paramset = length(paramsets_fullnames)
# paramsets = paste0("Paramset", 1:n.paramset)
paramsets =  c("Thresholds") # "BehaviouralBaseline", 
p_idx_default = 1 

service_tb = read.csv("Tables/Services.csv") %>% as.data.frame
# serviceNames <- c("Food.crops", "Fodder.crops", "GF.redMeat", "Fuel", "Softwood", "Hardwood", "Biodiversity",
# "Carbon", "Recreation", "Flood.reg", "Employment", "Ldiversity", "GF.milk")
serviceNames = service_tb$Name

serviceNames_full = service_tb$Fullname


# serviceColours <- c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity" = "red", "Timber" = "tan4", "Carbon" = "darkgreen", "Urban" = "grey", "Recreation" = "orange")
# serviceColours = c("Meat" = "coral1", "Crops" = "goldenrod1", "Diversity"="turquoise", "Timber" = "tan4","Carbon"="black", "Urban" = "grey","Recreation"="dodgerblue2")
serviceColours = c("Food.crops" = "coral1", "Fodder.crops" ="goldenrod1", "GF.redMeat" = "turquoise", "Fuel" = "tan4", "Softwood" = "black", "Hardwood" = "grey", "Biodiversity" = "dodgerblue2", "Carbon"="darkgreen", "Recreation" = "orange", "Flood.reg" = "lightblue", "Employment" = "purple", "Ldiversity" = "brown", "GF.milk" = "green", "Sus.Prod" = "pink")


capital_tb = read.csv("Tables/Capitals.csv") %>% as.data.frame
capitalNames = capital_tb$Name
capitalNames_full = capital_tb$Fullname

 




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

indicator_names_full = c(serviceNames_full, capitalNames_full, "Land Uses (agent types)")





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





aft_tb = read.csv("Tables/AgentColors_v2.csv", strip.white = T, stringsAsFactors = F) %>% as.data.frame

aft_tb[aft_tb$Name == "Lazy FR", ]$Name = "Unmanaged"



# aftNames<-c("Intensive arable food/fodder","Extensive arable", "Sustainable arable", "Productive broadleaf","Productive conifer","Mixed woodland","Conservation","Intensive pastoral","Extensive pastoral","Very extensive pastoral","Agro-forestry","Bioenergy","Urban","Unmanaged")
# aftColours<-c("#E3C16B","#91714C","#C0BCAE","#BDED50","#268c20","#215737","#0a1c01","#F3EF0C","#BCB918","#7d7d47","#28b1c9","#2432d1","#EE0F05","#fafaf7")

aft_colors_alpha = aft_tb$Color[match( aft_shortnames_fromzero, aft_tb$Name)]

aft_colors_fromzero = col2hex(paste0("#", substr(aft_colors_alpha, start = 4, stop = 10), substr(aft_colors_alpha, start = 2, stop = 3))) # ignore alpha channel

# 17 colours
aft_colors_fromzero_17 = aft_colors_fromzero

# reduced colours
# aft_colors_fromzero[aft_shortnames_fromzero %in% c("PNNB", "PNC", "PNNC", "PNB", "MW")] = col2hex("darkblue")


target_years_aggcsv = seq(2020, 2080, 10)
target_years_other =  seq(2020, 2080, 10)



aft_colors_fromzero_ts = aft_colors_fromzero
aft_colors_fromzero_ts[17] = "black" 
aft_lty_ts = c(rep(1, 13), 2)

n_cell_total = nrow(uk_coords)




aft_pal <- colorFactor(col2hex(as.character(aft_colors_fromzero)),  levels = as.character(c(0:15, -1)), na.color = "transparent")
aft_pal_group2 <- colorFactor(col2hex(as.character(aft_group2_colours_17)),  levels = as.character(c(0:15, -1)), na.color = "transparent")

# aft.pal(6)


# reduced
aft_group_colors =  aft_colors_fromzero_17[c(1:5, 7:11, 14:17)]
# aft_group_colors[7] = "darkblue"
aft_group_colors[length(aft_group_colors)] = "black"

aft_group_names_old <-c("Intensive arable food/fodder", # 1
            "Extensive arable", # 2
            "Sustainable arable", # 3
            "Productive broadleaf", # 4
            "Productive conifer", # 5
            "Mixed woodland", # 6
            "Conservation", # 7 
            "Intensive pastoral", # 8 
            "Extensive pastoral", # 9 
            "Very extensive pastoral", # 10
            "Agro-forestry",#11
            "Bioenergy",#12
            "Urban",#13
            "Unmanaged")#14
 

aft_group_shortnames = c( aft_shortnames_fromzero  )[  c(1:5, 7:11, 14:17)]
aft_group_shortnames[5] = "IA"
aft_group_shortnames[9] = "PB"
aft_group_shortnames[10] = "PC"

aft_group_fullnames = aft_group_names_old[c(1:4)]



# Set the arable and pastoral colours by their relative intensity
A.col<-"#998459"
#  P.col<-"#BCB918"
P.col<-"#c7bd00"









### Shaded

aft_shaded_colours_default = c("#E9D3AA", "#EED8B0", "#d9abd3", "#BDED50", "#268c20", "#215737", "#0a1c01", "#F6EB64", "#F9EE67", "#FFFAC5", "#28b1c9", "#2432d1", "#EE0F05", "#fafaf7")






 
#### 
intens<-read.csv("Tables/Intensity levels.csv")



arable_min = min(intens[, c(3,4,7,8,11,12, 15,16,19, 20)])
arable_max = max(intens[, c(3,4,7,8,11,12, 15,16,19, 20)])
arable_avg = mean(sapply(intens[, c(3,4,7,8,11,12, 15,16,19, 20)], as.numeric))

pastoral_min = min(intens[, c(3,4,7,8,11,12, 15,16,19, 20)+2])
pastoral_max = max(intens[, c(3,4,7,8,11,12, 15,16,19, 20)+2])
pastoral_avg = mean(sapply(intens[, c(3,4,7,8,11,12, 15,16,19, 20)+2], as.numeric))


Arable_cols_extended =  lighten(A.col, amount = 0.9 - c(arable_max, arable_avg, arable_min))
Pastoral_cols_extended = lighten(P.col, amount = 0.9 - c(pastoral_max, pastoral_avg, pastoral_min))

Arable_names_extended    =c("˥ intensive", "Arable", "˩ extensive")
Pastoral_names_extended  =c("˥ intensive", "Pastoral", "˩ extensive")

aft_shaded_colours_extended <-c(Pastoral_cols_extended, Arable_cols_extended, "#d9abd3","#BDED50","#268c20","#215737","#0a1c01", "#28b1c9","#2432d1","#EE0F05","#fafaf7")
aft_group_names_extended = c(Pastoral_names_extended, Arable_names_extended, aft_group_names_old [c(3:7)],aft_group_names_old [c(11:14)])


aft_shaded_colours_srt_default = aft_shaded_colours_default[c(11, 12, 2, 9, 1, 1, 8, 6, 7, 4, 5, 4, 5, 3, 10, 13, 14)]

pal_shaded_default = colorFactor(col2hex(as.character(aft_shaded_colours_srt_default)),  levels = as.character(c(0:15, -1)), na.color = "transparent")


### Halfway classes 3

aft_group2_tb =t( matrix( nrow = 2, c("IAfood",  "Arable", 
                      "IAfodder",  "Arable", 
                      "EA",  "Arable",
                      "Bioenergy",  "Arable",
                      "SusAr", "Arable", 
                      "IP", "Pastoral", 
                      "EP", "Pastoral",
                      "PNB", "Forest", 
                      "PNNB", "Forest", 
                      "PNC", "Forest", 
                      "PNNC", "Forest", 
                      "MW",  "Very extensive/mixed", 
                      "VEP",  "Very extensive/mixed", 
                      "AF", "Very extensive/mixed", 
                      "NWCons", "Conservation",
                      "Urban", "Urban",
                      "Lazy FR", "Unmanaged")))
# aft_group2_names =  ( unique(aft_group2_tb[,2]))
aft_group2_names =c("Arable","Pastoral","Forest","Very extensive/mixed","Conservation","Urban","Unmanaged")


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




 


### Intensity plot

impact_coeff = data.frame(rbind(
  c("Urban",	1.0), 
  c("IAfood", 0.8),
  c("IAfodder",	0.80),
  c("IP",	0.70),
  c("AF",	0.65),
  c("Bioenergy",	0.60),
  c("PNNB", 0.5),
  c("PNNC",	0.50),
  c("EP", 0.40),
  c("EA", 0.4),
  c("SusAr", 	0.40),
  c("PNB", 0.2),
  c("PNC",	0.20),
  c("MW",	0.15),
  c("VEP",	0.10),
  c("NWCons",	0.01),
  c("Unmanaged",	0.0)
))

colnames(impact_coeff) = c("AFT", "Impact")







