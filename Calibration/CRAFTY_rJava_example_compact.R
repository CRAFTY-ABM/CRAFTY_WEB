
library(raster)
library(sp)
library(rJava)
library(jdx)


# location of this script 
setwd("~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_WEB/Calibration/") 
source("Functions_CRAFTY_rJava.R")
source("Functions_CRAFTY_common.R")
# source("../RScripts/Functions_CRAFTY_WEB.R")

# location of the downloaded data
path_crafty_package = "~/Dropbox/KIT_Modelling/CRAFTY/"

# Location of the CRAFTY Jar file
path_crafty_jar = path.expand(paste0(path_crafty_package, "CRAFTY_R_package/CRAFTY_KIT_engine2020.jar"))
# Location of the CRAFTY lib files
path_crafty_libs = path.expand(paste0(path_crafty_package, "CRAFTY_R_package/CRAFTY_KIT_engine2020/lib/"))
crafty_libs = list.files(paste0(path_crafty_libs), pattern = "jar")

# Location of the input data
path_crafty_inputdata = path.expand(paste0(path_crafty_package, "CRAFTY_R_package/data_KIT2020/"))
# Name of the scenario file
scenario.filename = "Scenario_KIT2019.xml"
scenario.filename = "Scenario_KIT2019_standard_nooutput.xml" # 8years without CSV output

# # Location of the output files
# path_batch_root = "~/Downloads/CRAFTY_R_batch/"
# RUN_ID = "CRAFTY_CoBRA_EU_testrun"
# path_batch = paste0(path_batch_root, RUN_ID)
# 
# if (!dir.exists(path_batch)) { 
#   dir.create(path_batch, recursive = T)
# }




#### JAVA configuration 
crafty_jclasspath = c(
  path_crafty_jar
  , paste0(path_crafty_libs, crafty_libs)
  
)


# add java classpath
.jinit() 
for (i in 1:length(crafty_jclasspath)) { 
  .jaddClassPath(crafty_jclasspath[i])
}
# .jclassPath() # print out the current class path settings. 


.jinit( parameters="-Dlog4j.configuration=log4j2020_normal.properties")
.jinit(parameters = "-Dfile.encoding=UTF-8", silent = FALSE, force.init = FALSE)
.jinit( parameters=paste0("-Xms", java.ms, " -Xmx", java.mx)) # The .jinit returns 0 if the JVM got initialized and a negative integer if it did not. A positive integer is returned if the JVM got initialized partially. Before initializing the JVM, the rJava library must be loaded.

# .jinit(parameters = paste0("-Duser.dir=",path_batch ))

## Parallelisation
parallelize = FALSE

if (parallelize) { 
  library(doMC)
  registerDoMC(n_thread)
  n_thread = detectCores()
  
  # library(doSNOW) # JVM object not sharable? error occurred.
  # n_thread = detectCores()
  # cl = makeCluster(n_thread)
  # registerDoSNOW(cl)
  
}




############# CRAFTY configuration
# Model run 
start_year_idx = 1 # first year of the input data
end_year_idx = 8 # fifth year of the input data 

# Scenario 2019 
CRAFTY_sargs =   c("-d", path_crafty_inputdata, "-f", scenario.filename, "-o", "99", "-r", "1",  "-n", "1", "-sr", "0") # change the argument as you wish 


########### Model running 
print(paste0("============CRAFTY JAVA-R API: Create the instance"))

CRAFTY_jobj = new(J(CRAFTY_main_name)) # Create a new instance (to call non-static methods)

# prepares a run and returns run information 
CRAFTY_RunInfo_jobj = CRAFTY_jobj$EXTprepareRrun(CRAFTY_sargs)
print(paste0("============CRAFTY JAVA-R API: Run preparation done"))

# running from the first timestep to the fifth
CRAFTY_loader_jobj = CRAFTY_jobj$EXTsetSchedule(as.integer(start_year_idx), as.integer(end_year_idx))



# slower..  
# system.time({ 
#     val_fr = sapply(allcells_l, function(c) c$getOwnersFrLabel() )
# })
# print("sapply")
# system.time({
#     val_fr = sapply(allcells_l2, function(c) c$getOwnersFrLabel() )
# })

region = CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()$'next'()


# # alloc_m = region$getAllocationModel()
# # .jmethods(alloc_m)
# # btmap = region$getBehaviouralTypeMapByLabel() 
# 
# a = region$getFunctionalRoleMapByLabel()
# a$values()
# a2 = a$get("Ur")
# a2$getAlternativeFrId()
# a2$getSampledGivingInThreshold()

allcells_uset = region$getAllCells() 
allcells_l =   as.list(allcells_uset)


#### Get XY coordinates
print("Get XY coords")

system.time({
  val_xy =foreach(c = allcells_l, .combine = "rbind") %do% { 
    c(X=c$getX(), Y=c$getY())
  }
  val_xy = data.frame(val_xy)
  colnames(val_xy) = c("X", "Y")
  x_coord = x.lat.v[val_xy$X]
  y_coord = y.lon.v[val_xy$Y]
})

crafty_sp =SpatialPoints(cbind(x_coord, y_coord))
proj4string(crafty_sp) = proj4.LL



# 
# crafty main loop 
for (tick in start_year_idx:end_year_idx) { 
  
  nextTick = CRAFTY_jobj$EXTtick()
  
  stopifnot(nextTick == (tick + 1 )) # assertion
  
  
  # safe to alter capital csv files
  
  
  
  
  # Yet experimental as rJava frequently hangs.. 
  doProcessFR = T
  if (doProcessFR) { 
    
    region = CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()$'next'()
    
    # allcells = region$getAllCells()
    allcells_uset = region$getAllCells() # slower
    # allcells_arr = allcells_uset$toArray() # slower # often throws jave execption/warning 
    allcells_l =   as.list(allcells_uset)
    
    print("Process output")
    
    # visualise something 
    
    # allregions_iter = CRAFTY_loader_jobj$getRegions()$getAllRegions()$iterator()
    # region = allregions_iter$'next'()
    # for all regions 
    
    # allcells_uset = r$getAllCells() # UnmodifiableSet
    # a = allcells_uset$forEach()
    # 
    # 
    # allcells_stream = allcells_uset$stream()
    
    # aa = allcells_stream$toArray()
    # str(aa[[1]]$getX())
    # .jmethods(allcells_stream$forEach())
    # x= allcells_stream$sorted()
    # x2 = x$forEach()$getX()
    
    # allcells_l =  as.list(r$getAllCells()) # faster with as.list
    print(Sys.time())
    
    if (parallelize) { 
      system.time({
        val_df = foreach(c = allcells_l, .combine = rbind, .packages = c("rJava"), .verbose = F, .export = c("region",  "allcells_l") ) %dopar% {
          c(  c$getOwnersFrLabel(), c$getEffectiveCapitals()$getAll(), c$getSupply()$getAll())
        }
        val_fr = val_df[,1]
        
      })
    } else  {
      system.time({
        val_df = t(sapply(allcells_l, FUN = function(c) c(  c$getOwnersFrLabel()#, c$getEffectiveCapitals()$getAll(), c$getSupply()$getAll()
        )))
        val_fr = val_df[1,]
        
      })
    }
    print(Sys.time())
    
    
    
    val_fr_fac = factor(val_fr, levels = aft.names.fromzero)
    
    
    fr_spdf = SpatialPixelsDataFrame(crafty_sp, data =data.frame( as.numeric(val_fr_fac )), tolerance = 0.0011)
    fr_r = raster(fr_spdf)
    # plot(fr_r)
    # par(mfrow=c(3,3))
    plot(fr_r, main = paste0("Tick=", tick), xlab = "lon", ylab = "lat")
    
    # rm(allregions_iter)
    rm(allcells_l)
  }
  
  
  if (nextTick <= end_year_idx) { 
    print(paste0("============CRAFTY JAVA-R API: NextTick=", nextTick))
  } else { 
    print(paste0("============CRAFTY JAVA-R API: Simulation done (tick=", tick, ")"))
    
  }
}
# stopCluster(cl)

val_fr_last = val_fr_fac

fr_spdf = SpatialPixelsDataFrame(crafty_sp, data =data.frame( as.numeric(val_fr_last )), tolerance = 0.0011)
fr_r = raster(fr_spdf)
plot(fr_r, main = paste0("Tick=", tick), xlab = "lon", ylab = "lat", col = aft.colors.fromzero )




a = region$getFunctionalRoles() 
b = a$toArray()
# b[[1]]$getMeanGivingInThreshold()
# b[[1]]$getMeanGivingUpThreshold()
# b[[1]]$getSampledGivingInThreshold()
# b[[1]]$getSampledGivingInThreshold()
# b[[1]]$getProduction()
# b[[1]]$getAllocationProbability()
# b[[1]]$getNewFunctionalComp()

b[[4]]
b[[4]]$getMeanGivingInThreshold()
b[[2]]$getMeanGivingUpThreshold()
b[[2]]$getSampledGivingInThreshold()
# b[[1]]$getSampledGivingInThreshold()
# b[[1]]$getProduction()
# b[[1]]$getAllocationProbability()
# b[[1]]$getNewFunctionalComp()






rm(region)
stop("ends here")


print("Close run")


# close the run 
CRAFTY_jobj$EXTcloseRrun() # ignore the MPI initialisation error

# delete the java objects
rm(CRAFTY_loader_jobj)
rm(CRAFTY_jobj)
rm(CRAFTY_jclass)
rm(CRAFTY_RunInfo_jobj)


