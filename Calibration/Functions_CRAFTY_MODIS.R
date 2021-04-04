
# read modis land cover files 
lc.raster.files <- sort(list.files("~/Dropbox/KIT_Modelling/CRAFTY/Calibration_old/MCD12Q1_1km/", pattern = "MCD12Q1_365day_EU_LC1_Primary.*\\.tif$", full.names = T))
lc.rs <- stack(lc.raster.files[6:13]) # 2006 ~ 2013

plot(lc.rs)

# MODIS12Q1Class    Code    Name
# 0     WAT    Water Bodies
# 1     ENF	Evergreen Needleleaf Forests 
# 2	    EBF	Evergreen Broadleaf Forests 
# 3	    DNF	Deciduous Needleleaf Forests
# 4	    DBF	Deciduous Broadleaf Forests
# 5	    MF	Mixed Forests
# 6	    CSH	Closed Shrublands
# 7	    OSH	Open Shrublands
# 8	    WSA	Woody Savannas
# 9	    SAV	Savannas
# 10	GRA	Grasslands
# 11	WET	Permanent Wetlands
# 12	CRO	Croplands
# 13	URB	Urban and Built-Up Lands
# 14	CVM	Cropland/Natural Vegetation Mosaics
# 15    SNO	Snow and Ice
# 16	BRN	Barren or Sparsely Vegetated
# 99	INT	Interrupted Areas
# 100	MD Missing Data

mcd12q1.code <- c("WAT", "ENF", "EBF", "DNF", "DBF", "MF", "CSH", "OSH", "WSA", "SAV", "GRA", "WET", "CRO", "URB", "CVM", "SNO", "BRN")
mcd12q1.names <- c("Water", "Evergreen Needleleaf forest", "Evergreen Broadleaf forest", "Deciduous Needleleaf Forests", "Deciduous Broadleaf Forests", "Mixed Forests", "Closed Shrublands", "Open Shrublands", "Woody Savannas", "Savannas", "Grasslands", "permanent Wetlands", "Croplands", "Urban and built-up", "Cropland/Natural vegetation mosaic", "Snow and Ice", "Barren or sparsely vegetated")


