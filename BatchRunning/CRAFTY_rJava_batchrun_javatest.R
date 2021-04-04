
library(raster)
library(sp)
library(jdx)
library(xml2)
library(doSNOW) # doMC did not work

# location of this script 
setwd("~/Dropbox/KIT_Modelling/CRAFTY/CRAFTY_WEB/Calibration/") 
source("Functions_CRAFTY_rJava.R")
source("Functions_CRAFTY_common.R")
# source("../RScripts/Functions_CRAFTY_WEB.R")

# location of the downloaded data
path_crafty_package = "~/Dropbox/KIT_Modelling/CRAFTY/"
path_crafty_data = "~/Dropbox/KIT_Modelling/CRAFTY/crafty_cobra_impressions_kit_data/"
# Location of the CRAFTY Jar file
path_crafty_jar = path.expand(paste0(path_crafty_package, "CRAFTY_R_package/CRAFTY_KIT_engine2020.jar"))
# Location of the CRAFTY lib files
path_crafty_libs = path.expand(paste0(path_crafty_package, "CRAFTY_R_package/lib/"))
crafty_libs = list.files(paste0(path_crafty_libs), pattern = "jar")

# Make sure that in the classpath setting , gt-opengis-9.0.jar must be included before geoapi-20050403.jar. Otherwise it throws an uncatchable error during the giving up process: loading libraries without ordering them particularly, the opengis library is loaded after the geoapi library following alphabetical order.
# Related commit - https://github.com/CRAFTY-ABM/CRAFTY_CoBRA/commit/4ce1041cae349572032fc7e25be49652781f5866

crafty_libs = crafty_libs[crafty_libs != "geoapi-20050403.jar"  ] 
crafty_libs = c(crafty_libs[1:14],  "geoapi-20050403.jar", crafty_libs[15:length(crafty_libs )])

# Location of the input data
path_crafty_inputdata = path.expand(paste0(path_crafty_data, "data_EU28/"))



path_crafty_batch_run = path.expand(paste0("~/tmp/"))
### Scenario  

# Name of the scenario file
scenarios <- c( "Baseline", "RCP2_6-SSP1","RCP2_6-SSP4","RCP4_5-SSP1","RCP4_5-SSP3","RCP4_5-SSP4","RCP8_5-SSP3","RCP8_5-SSP5")
n.scenario = length(scenarios)
scenario.filenames = paste0("Scenario_", scenarios, "_everyyear_8years_relative_landusecontrol_nodisplay")

scenario.filename.dummy =  paste0("Scenario_Baseline_everyyear_8years_relative_landusecontrol_nodisplay_dummy.xml")


 
#### JAVA configuration 
crafty_jclasspath = c(
    path_crafty_jar
    , paste0(path_crafty_libs, crafty_libs)
    
)



# ## Parallelisation
# parallelize = T
# 
# if (parallelize) { 
#     
#     # Each process roughly occupies 2.8-3.3 G memory (10 arcmin EU-28 version) 
#     n_thread = 8 # detectCores()
#     cl = makeCluster(n_thread)
#     registerDoSNOW(cl)
#     
# }




setwd(path_crafty_batch_run)

library(rJava)

.jinit(parameters="-Dlog4j.configuration=log4j2020_normal.properties")
.jinit(parameters = "-Dfile.encoding=UTF-8", silent = FALSE, force.init = FALSE)
.jinit( parameters=paste0("-Xms", java.ms, " -Xmx", java.mx)) # The .jinit returns 0 if the JVM got initialized and a negative integer if it did not. A positive integer is returned if the JVM got initialized partially. Before initializing the JVM, the rJava library must be loaded.

# add java classpath
.jclassPath() # print out the current class path settings.
for (i in 1:length(crafty_jclasspath)) { 
    .jaddClassPath(crafty_jclasspath[i])
}
# .jinit(parameters = paste0("user.dir=", path_crafty_batch_run )) # does not work.. 

.jcall( 'java/lang/System', 'S', 'setProperty', 'user.dir', path_crafty_batch_run )

print(  .jcall( 'java/lang/System', 'S', 'getProperty', 'user.dir' ))

start_year_idx = 1 
end_year_idx = 5 
s.idx = 2
p.idx = 2 
paramset =  paste0("Paramset", p.idx)

scenario.filename = paste0(scenario.filenames[s.idx], "_", paramset, ".xml") 

CRAFTY_sargs =   c("-d", path_crafty_inputdata, "-f", scenario.filename, "-o", "99", "-r", "1",  "-n", "1", "-sr", "0") # change the argument as you wish 
 
CRAFTY_jobj = new(J(CRAFTY_main_name)) # Create a new instance (to call non-static methods)

# prepares a run and returns run information 
CRAFTY_RunInfo_jobj = CRAFTY_jobj$EXTprepareRrun(CRAFTY_sargs)
print(paste0("============CRAFTY JAVA-R API: Run preparation done"))

# running from the first timestep to the fifth
CRAFTY_loader_jobj = CRAFTY_jobj$EXTsetSchedule(as.integer(start_year_idx), as.integer(end_year_idx))

# CRAFTY_jobj$main(CRAFTY_sargs)
# print(  .jcall( 'java/lang/System', 'S', 'getProperty', 'user.dir' ))

.jclassPath()
nextTick = CRAFTY_jobj$EXTtick()
nextTick = CRAFTY_jobj$EXTtick()
# nextTick = CRAFTY_jobj$EXTtick()
# nextTick = CRAFTY_jobj$EXTtick()








 