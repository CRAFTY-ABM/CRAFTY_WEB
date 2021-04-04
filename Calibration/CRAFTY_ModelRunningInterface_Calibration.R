##### iEMSs 2020

library(doMC)

path_wd = "~/Rtemp/"
GroupName = "RaSh" 

username_server = "di52doh"
username = Sys.info()["user"]

if (username == username_server ) { 
  path.script = "~/pd/CRAFTY/Scripts/"
} else if (username %in%  c("seo-b", "alan") ) { 
  
  path.script = "~/Dropbox/KIT_Modelling/CRAFTY/Scripts/"
  
}

# setwd(path.script)



# Working directory 
wd <- paste0( path_wd, "/", GroupName)
if (!dir.exists(wd)) { 
  dir.create(wd, recursive = T)
}
setwd(wd)


# A land use agent is able to leverage the capitals available on a land parcel (represented as a cell) to provide a range of services. Each agent has a production function as part of its FR, which maps capital levels onto service provision (see section 7. Submodels, Production). An agent’s benefit according to a given level of service provision can be calculated based on societal demands, overall supply levels and marginal utility functions. 

# Agent Functional Types (AFTs)
AFTs <- c("EP", "Ext_AF", "IA", "Int_AF", "Int_Fa", "IP", "MF", "Min_man","Mix_Fa", "Mix_For", "Mix_P", "Multifun", 
          "P-Ur", "UL", "UMF", "Ur", "VEP")
n.aft <- length(AFTs)
 

scenario.filenames = c("Scenario_KIT2019_10year.xml", "Scenario_KIT2019_doubled.xml", "Scenario_KIT2019_tripled.xml","Scenario_KIT2019_nonupled.xml")
# scenario.filenames = c("Scenario_KIT2019_nonupled.xml")

n.scenario.files = length(scenario.filenames)

scenario.filename = "Scenario_KIT2019.xml"
n.paramset = 1
paramset = read.csv2(paste0("~/Dropbox/KIT_Modelling/CRAFTY/EUpaper/Batchruns/ParameterDesign/Paramset", 1, ".csv"), sep = ",")







# Param-set 1 (Behavioural baseline)
# Param-set 2 (Thresholds)
# Param-set 3 (Variations)

n.thread = n.paramset

library(doMC)
# registerDoMC(n.thread)


path.batch.out = "~/Rtemp/RaSh/tmp"


# source(paste0(path.script, "/module/CRAFTY_Running_V4_Original.R"))
source(paste0(path.script, "/module/CRAFTY_Running_V6.R"))


run.log = runCRAFTY(GroupName, RUN_ID = "RaShtest", paramset, scenario.filename, path.batch.out,   INTERACTIVE = TRUE)


stop("ends here")


#p.idx = 1

foreach(s.idx = 1:n.scenario.files, .errorhandling = "stop", .verbose = T) %do% {
  #    
  scenario.filename = scenario.filenames[s.idx]
  # scenario.filename = scenario.filenames[1]
  
  foreach(p.idx = 1:n.paramset, .errorhandling = "stop", .verbose = T) %dopar% { 
    
    setwd(wd)
    
    paramset = read.csv2(paste0("Paramsets/Paramset", p.idx, ".csv"), sep = ",")
    
    stopifnot(dim(paramset) == c(17,8))
    
    
    # Run identifier 
    # RUN_ID <- paste0("Run_Meat",utility.b["Meat"], "_Crops", utility.b["Crops"],"_AvgGI",round(mean(Gi), 2), "_AvgGU", round(mean(Gu), 2), "_", format(Sys.time(),     "%Y-%m-%d_h%Hm%M"))
    RUN_ID <- paste0("Run_", GroupName, "_Paramset_",p.idx, "_", format(Sys.time(),     "%Y-%m-%d_h%Hm%M"))
    
    print(RUN_ID)
    path.batch.out <- paste0("~/", GroupName, "/CSV/",RUN_ID  )
    
    # Run the model - takes a while (ca. 5 min) 
    # source(paste0(path.script, "/module/CRAFTY_Running_V3.R"), echo = T, verbose = T) 
    
    run.log = runCRAFTY(GroupName, RUN_ID, paramset, scenario.filename, path.batch.out)
    # run.log = runCRAFTY(GroupName, RUN_ID, paramset)
    # You will face the logging information - Wait for the message like
    

    
    # which means it is done. 
    print(paste0("Run ", path.batch.out, "is finished."))
    
    setwd(wd) # back to the working directory 
    
    
  }
}
##### Model running done  ####



# Output metrics
# service supply
# AFT maps 
# transitions 
# fragrmentation metrics (relative to the own baseline sc.)
# 
# 























