library(parallel)
library(doMC)
library(raster)
library(sp)
library(rJava)
# .jinit()
# .jaddClassPath("/Users/user/Documents/git/HelloWorld/bin")    # replace with the path to the folder containing your class file
# .jclassPath()
# .jmethods()
# .jconstructors()
# https://www.r-bloggers.com/a-primer-in-using-java-from-r-part-1/
# Signatures in JNI notation
# JAVA TYPE	SIGNATURE
# boolean	Z
# byte	B
# char	C
# short	S
# int	I
# long	J
# float	F
# double	D
# type[]	[ type
# method type	( arg-types ) ret-type
# fully-qualified-class	Lfully-qualified-class ;
# In the fully-qualified-class row of the table above note the
# 
# L prefix
# ; suffix
# For example
# 
# the Java method: long f (int n, String s, int[] arr);
# has type signature: (ILjava/lang/String;[I)J

RUN_ID = "CRAFTY_CoBRA_EU"
INTERACTIVE = FALSE

# jdk.base.path <- "/usr/lib/jvm/java-11-openjdk-amd64/jre/"

jdk.base.path <- "/Library/Java/JavaVirtualMachines/jdk1.8.0_171.jdk/Contents/Home/"
jdk.base.path <- "/usr/lib/jvm/java-8-openjdk-amd64/"

jdk.bin.path <- paste0(jdk.base.path, "bin/java")

Sys.info()

# path.batch.base <- paste0("/home/alan/git/tmp/",  RUN_ID, "/")
# path.batch.base <- paste0("/Users/seo-b/git/")
path.batch.base <- paste0(path.expand("~"), "/git/")

path.batch.data <- paste0(path.batch.base, "data/")


path_data = paste0(path.expand("~"), "/Dropbox/KIT_Modelling/CRAFTY/crafty_cobra_impressions_kit_data/data_KIT2020/")

aft.names.fromzero <- c( "Ext_AF", "IA", "Int_AF", "Int_Fa", "IP", "MF", "Min_man", "Mix_Fa", "Mix_For", "Mix_P", "Multifun", "P-Ur", "UL", "UMF", "Ur", "VEP", "EP")

scenario.filename = "Scenario_KIT2020_minimal.xml"

java.mx <- "15g" # 25 GB max in LRZ
java.ms <- "1g -d64"  

if (!dir.exists(path.batch.data)) { 
    dir.create(path.batch.data, recursive = T)
}

setwd(path.batch.base)




crafty_impressions_libs = list.files(paste0(path.batch.base, "CRAFTY_CoBRA_Impressions_KIT/lib/"), pattern = "jar")
modelling_libs = list.files(paste0(path.batch.base, "modelling-utilities/lib/"), pattern = "jar")
# craftycobra_libs = list.files(paste0(path.batch.base, "CRAFTY_CoBRA/lib/"), pattern = "jar")
# craftycobra_template_libs = list.files(paste0(path.batch.base, "CRAFTY_Template/lib/"), pattern = "jar")
# craftycobra_demo_libs = list.files(paste0(path.batch.base, "CRAFTY_CoBRA_Demo/lib/"), pattern = "jar")



crafty_jclasspath = c(
    paste0(path.batch.base, "CRAFTY_CoBRA_Impressions_KIT/bin/")
    ,paste0(path.batch.base, "CRAFTY_CoBRA/bin/")
    # ,"/Users/seo-b/git/CRAFTY_CoBRA_Demo/bin/"
    ,paste0(path.batch.base, "modelling-utilities/bin/")
    ,paste0(path.batch.base, "CRAFTY_Template/bin/")
    , paste0(path.batch.base, "CRAFTY_CoBRA_Impressions_KIT/lib/", crafty_impressions_libs)
    # , paste0("/Users/seo-b/git/CRAFTY_Template/lib/", craftycobra_template_libs)
    
    # , paste0("/Users/seo-b/git/modelling-utilities/lib/", modelling_libs)
    # , paste0("/Users/seo-b/git/CRAFTY_CoBRA/lib/", craftycobra_libs)
    # ,paste0(path.batch.base, "CRAFTY_Template/config/"
    # , paste0("/Users/seo-b/git/CRAFTY_CoBRA_Demo/lib/", craftycobra_demo_libs) # bt list?
    
    ,paste0(path.batch.base, "CRAFTY_CoBRA_Impressions_KIT/config/")
    ,paste0(path.batch.base, "CRAFTY_CoBRA_Impressions_KIT/config/log4j/")
    # 
    #paste0(path.batch.base, "tmp/ModellingUtilities.jar",
)

# jep_ext-1.1.1.jar added to the impressions



# /Library/Java/JavaVirtualMachines/jdk1.8.0_171.jdk/Contents/Home/bin/java -Dlog4j.configuration=log4j/log4j2020_normal.properties -Djava.library.path=/Users/seo-b/git/CRAFTY_CoBRA/lib/jniLibs -Dfile.encoding=UTF-8 



# -Dlog4j.configuration=log4j/log4j2020_normal.properties -Djava.library.path=/Users/seo-b/git/CRAFTY_CoBRA/lib/jniLibs


# .jinit( parameters="-Dfile.encoding=UTF-8 -Dlog4j.configuration=log4j_cluster.properties")
.jinit(parameters = "-Dfile.encoding=UTF-8", silent = FALSE, force.init = FALSE)
.jinit( parameters="-Xms256m -Xmx1024m") # The .jinit returns 0 if the JVM got initialized and a negative integer if it did not. A positive integer is returned if the JVM got initialized partially. Before initializing the JVM, the rJava library must be loaded.
# .jclassPath() # print out the current class path settings. 

for (i in 1:length(crafty_jclasspath)) { 
    .jaddClassPath(crafty_jclasspath[i])
}
# .jclassPath() # print out the current class path settings. 

# CRAFTY_sargs =   c("-d", "/Users/seo-b/Dropbox/KIT_Modelling/CRAFTY/crafty_cobra_impressions_kit_data/data_TemplateCoBRA","-f", "xml/Scenario_2020.xml", "-o", "99", "-r", "1",  "-n", "1", "-sr", "0" )

# 2019 
CRAFTY_sargs =   c("-d", path_data,"-f", "Scenario_KIT2019.xml", "-o", "99", "-r", "1",  "-n", "1", "-sr", "0")


# Template
# CRAFTY_sargs =   c("-d", "/Users/seo-b/Dropbox/KIT_Modelling/CRAFTY/crafty_cobra_impressions_kit_data/data_TemplateCoBRA/","-f", "xml/Scenario_2020.xml", "-o", "99", "-r", "1" )


if (INTERACTIVE) { 
    CRAFTY_sargs[length(CRAFTY_sargs) + 1 ] = "-i"
}


# crafty_jobj = .jnew("org.volante.abm.serialization.ModelRunner")
# .jcall(crafty_jobj, method = "prepareRrun",  returnSig = "Lorg/volante/abm/schedule/RunInfo",   crafty_sargs)

CRAFTY_jclass = J("org.volante.abm.serialization.ModelRunner") # Better using the reflection based API in rJava.  
CRAFTY_jobj = new(CRAFTY_jclass) # Create a new instance (to call non-static methods)

# .jmethods(crafty_jobj) # list up the methods

# prepares a run and returns run information 
CRAFTY_RunInfo_jobj = CRAFTY_jobj$EXTprepareRrun(CRAFTY_sargs)
# CRAFTY_RunInfo_jobj$getScenario()
# CRAFTY_jobj$getRunInfo()
# schedule = CRAFTY_RunInfo_jobj$getSchedule()

start = 1 # first year 
end = 30 # fifth year

# running from the first timestep to the fifth
CRAFTY_loader_jobj = CRAFTY_jobj$EXTsetSchedule(as.integer(start), as.integer(end))

tick = 1 



# Cell ID and cooridnates 
# ctry.ids <- read.csv("~/Dropbox/KIT/CLIMSAVE/IAP/Cell_ID_LatLong.csv")
# x.lat.v = sort(unique(ctry.ids$Longitude))
# y.lon.v = sort(unique(ctry.ids$Latitude))

crafty_sp =NA 

n_thread = detectCores()
# registerDoMC(n_thread)





par(mfrow=c(2,2))
# crafty main loop 
for (tick in start:end) { 
    
    nextTick = CRAFTY_jobj$EXTtick()
    
    stopifnot(nextTick == (tick + 1 )) # assertion
    
    # visualise something 
    
    allregions_iter = CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()
    r = allregions_iter$'next'()
    # for all regions 
    
    allcells_l = as.list(r$getAllCells()$toArray())
    
    
    print("Process output")
    # slower..  
    # system.time({ 
    #     val_fr = sapply(allcells_l, function(c) c$getOwnersFrLabel() )
    # })
    # print("sapply")
    # system.time({
    #     val_fr = sapply(allcells_l2, function(c) c$getOwnersFrLabel() )
    # })
    if (is.na(crafty_sp)) { 
        print("XY coords")
        
        
        system.time({
            val_x =foreach(c = allcells_l, .combine = "c") %dopar% { 
                c$getX()
            }
            val_y =foreach(c = allcells_l, .combine = "c") %dopar% { 
                c$getY()
            }
        })
        
        # val_x = as.numeric(lapply(allcells_l, function(c) c$getX(),  mc.cores = n_thread))
        # val_y = as.numeric(lapply(allcells_l, function(c) c$getY(),  mc.cores = n_thread))
        crafty_sp =SpatialPoints(cbind(val_x, val_y))
    }
    
    if (FALSE) { 
        print("mclapply")
        system.time({
            val_fr =mclapply(allcells_l, function(c) c$getOwnersFrLabel(), mc.cores = n_thread)
            val_fr_fac = factor(val_fr, levels = aft.names.fromzero)
            
        })
        
        
        system.time({
            val_fr =mclapply(allcells_l, function(c) c$getOwnersFrLabel(), mc.cores = n_thread)
        })
        
        system.time({
            val_fr =foreach(c = allcells_l, .combine = "c") %dopar% { 
                c$getOwnersFrLabel()
            }
        })
        
        val_fr_fac = factor(val_fr, levels = aft.names.fromzero)
        
        
        # allcells_l[[1]]$getOwnersFrLabel()
        
        # .jevalArray(a)
        #   b <- .jarray(c("hello","world"))
        #   print(b)
        #   c <- .jarray(list(a,b))
        #   print(c)
        #   # simple .jevalArray will return a list of references
        #   print(l <- .jevalArray(c))
        #   # to convert it back, use lapply
        #   lapply(l, .jevalArray)
        #   
        #   # two-dimensional array resulting in int[2][10]
        #   d <- .jarray(list(a,a),"[I")
        #   print(d)
        #   # use dispatch to convert a matrix to [[D
        #   e <- .jarray(matrix(1:12/2, 3), dispatch=TRUE)
        #   print(e)
        #   # simplify it back to a matrix
        #   .jevalArray(e, simplify=TRUE)
        #       a <- .jarray(1:10)
        #   print(a)
        
        
        
        
        
        
        # Capital
        system.time({
            # DoubleMap: Crop.productivity = 0.384626502, Forest.productivity = 0.18026224, Grassland.productivity = 0.413186214, Financial.capital = 0.624110478, Human.capital = 0.580604373, Social.capital = 0.608155398, Manufactured.capital = 0.679367708, Urban.capital = 0.018665655}"
            val_ec_l =mclapply(allcells_l, function(c) c$getEffectiveCapitals()$getAll(), mc.cores = n_thread)
            val_ec = do.call(rbind, val_ec_l)
            str(val_ec)
            
        })
    }
    # Supply
    system.time({
        print("process supply")
        
        supply_names = c("Meat", "Crops", "Diversity", "Timber", "Carbon", "Urban", "Recreation")
        # "Java-Object{DoubleMap: Meat = 26.092238813836513, Crops = 248.28197511127283, Diversity = 0.54, Timber = 0.0, Carbon = 422.0, Urban = 0.0, Recreation = 0.03866233780388677}"
        # val_supl_l =mclapply(allcells_l, function(c) c$getSupply()$getAll(), mc.cores = n_thread)
        # val_supl = do.call(rbind, val_supl_l)
        
        system.time({
            val_supl =foreach(c = allcells_l, .combine = "rbind") %dopar% { 
                c$getSupply()$getAll()
            }
        })
        
        
        
        # str(val_supl)
        val_supl = data.frame(val_supl)
        colnames(val_supl) = supply_names 
    })
    
    supply_spdf = SpatialPixelsDataFrame(crafty_sp, data = val_supl)
    supply_rs = stack(supply_spdf)
    names(supply_rs) = paste0(names(supply_rs), "_tick", tick, "")
    plot(supply_rs)
    # # plot(crafty_sp, col = as.numeric(val_fr_fac$AFT), main = paste0("Tick=", tick))
    # 
    # fr_spdf = SpatialPixelsDataFrame(crafty_sp, data =data.frame( as.numeric(val_fr_fac )))
    # fr_r = raster(spdf)
    # # plot(fr_r)
    # # par(mfrow=c(3,3))
    # plot(fr_r, main = paste0("Tick=", tick))
    
    rm(allregions_iter)
    
    # safe to alter capital csv files
    
    if (nextTick <= end) { 
        print(paste0("============R API: NextTick=", nextTick))
    }
}



print("Close run")


# close the run 
CRAFTY_jobj$EXTcloseRrun()

rm(CRAFTY_loader_jobj)
rm(CRAFTY_jobj)
rm(CRAFTY_jclass)
rm(CRAFTY_RunInfo_jobj)

# public void doOutput(Regions r) {
#     for (Outputter o : outputs) {
#         if (this.runInfo.getSchedule().getCurrentTick() >= o.getStartYear()
#             && (this.runInfo.getSchedule().getCurrentTick() <= o.getEndYear())
#             && (this.runInfo.getSchedule().getCurrentTick() - o.getStartYear())
#             % o.getEveryNYears() == 0) {
#             // <- LOGGING
#             log.info("Handle outputter " + o);
#             // LOGGING ->
#                 
#                 o.doOutput(r);
#         }
#     }
# }

# public void regionsToRaster(String filename, Regions r, CellToDouble converter,
# boolean writeInts, DecimalFormat format, String nDataString) throws Exception {

# r = allregions_iter$'next'()
# .jmethods(r)
# allcells_iter = r$getAllCells()$iterator()
# # r2 = .jcast(allregions$iterator(), new.class = "org/volante/abm/data/Region", check = T, convert.array = F)
# 
# 
# allcells_iter$
#     
#     
#     c = allcells_iter$'next'()
# c$getX()
# c$getY()
# fl = c$getOwnersFrLabel()
# 
# # val = allcells_iter$'next'()$getOwnersFrLabel()
# 
# val =  lapply(allcells_iter, function(c) c$getOwnersFrLabel() )
# fr_iter = r$getFunctionalRoles()$iterator()
# 
# while(fr_iter$hasNext()) { 
#     
# }
# f = fr_iter$'next'()
# f$getLabel()



# regionsToRaster(fn, r, this, isInt(), doubleFmt, nDataString);
# region r = .. 
# Extent e = r.getExtent();
# Raster raster = new Raster(e.getMinX(), e.getMinY(), e.getMaxX(), e.getMaxY());
# raster.setNDATA(nDataString);
# for (Cell c : r.getAllCells()) {
#     raster.setXYValue(c.getX(), c.getY(), converter.apply(c));
# }

# RasterWriter writer = new RasterWriter();
# if (format != null) {
#     writer.setCellFormat(format);
# } else if (writeInts) {
#     writer.setCellFormat(RasterWriter.INT_FORMAT);
# }
# writer.writeRaster(filename, raster);


