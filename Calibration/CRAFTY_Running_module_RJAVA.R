library(xml2)


GroupName = "RaSh"
RUN_ID = "test"
paramset = paramset
scenario.filename = scenario.filename
INTERACTIVE = TRUE

runCRAFTY = function(GroupName, RUN_ID, paramset, scenario.filename, path.batch.out, INTERACTIVE = FALSE) {
  
  
  
  username_server = "di52doh"
  username = Sys.info()["user"]
  
  
  suffix = "tmp/"
  
  
  model_name = "crafty_cobra_impressions_kit"
  #  model_name = "CRAFTY_TemplateBasic"
  
  if (username == username_server ) { 
    
    ### LRZ di52xeg
    
    jdk.base.path <- "/usr/lib/jvm/java-8-openjdk-amd64/"
    jdk.bin.path <- paste0(jdk.base.path, "bin/java")
    
    path.batch.org <-  paste0("/naslx/projects/pn69tu/di52doh/workspace/", model_name)  
    
    path.batch.base <- paste0("/naslx/projects/pn69tu/di52doh/workspace/", suffix,  RUN_ID, "/")
    
    
    java.mx <- "15g" # 25 GB max in LRZ
    java.ms <- "1g -d64"  
    
  } else if (username == "seo-b") { 
    
    
    jdk.base.path <- "/Library/Java/JavaVirtualMachines/jdk1.8.0_171.jdk/Contents/Home/"
    jdk.bin.path <- paste0(jdk.base.path, "bin/java")
    
    path.batch.org <- paste0("/Users/seo-b/workspace/", model_name) 
    
    path.batch.base <- paste0("/Users/seo-b/workspace/", suffix, RUN_ID, "/")
    
    
    
    java.mx <- "15g" # 25 GB max in LRZ
    java.ms <- "1g -d64"  
    
    
    
  } else if (username == "alan") { 
    
    
    jdk.base.path <- "/usr/lib/jvm/java-8-openjdk-amd64/jre/"
    jdk.bin.path <- paste0(jdk.base.path, "bin/java")
    
    path.batch.org <- paste0("/home/alan/workspace/", model_name) 
    
    path.batch.base <- paste0("/home/alan/workspace/", suffix, RUN_ID, "/")
    
    
    java.mx <- "20g" #  
    java.ms <- "1g -d64"  
    
  } else {
    
    stop("Wrong Hostname or User ID!")
  }
  
  
  # system(paste0("cp -r ", path.batch.org, "/* ", path.batch.base, "/"))
  system(paste0("rsync -a --exclude='.*' ", path.batch.org, "/ ", path.batch.base, "/"))
  # system(paste0("ln -s ~/workspace/worlds_data ",  path.batch.base, "/"))
  
  writeLines((RUN_ID), con = paste0(path.batch.base, "/info.txt"))
  
  
  path.batch.data <- paste0(path.batch.base, "data/")
  
  setwd(path.batch.base)
  
  
  
  ### Scenario  
  
  # Read the scenario file
  scenario.xml <- xml2::read_xml(paste0(path.batch.data, scenario.filename))
  str(scenario.xml)
  scenario.l <- xml2::as_list(scenario.xml)
  
  # # Replace utility_beta
  # for (u.idx in 1:length(utility.b)) {
  #     print(  attr(competition.l$competition[[u.idx]]$curve, "b") <- as.character(utility.b[u.idx]))
  # }
  # 
  
  
  
  ## Write the modified competition file
  scenario.xml.modified <- as_xml_document(scenario.l)
  write_xml(scenario.xml.modified, paste0(path.batch.data, scenario.filename))
  
  
  
  
  ### Competition 
  
  # # Read the competition file
  # competition.xml <- xml2::read_xml(paste0(path.batch.data, "xml/Competition.xml"))
  # str(competition.xml)
  # competition.l <- xml2::as_list(competition.xml)
  # 
  # # Replace utility_beta
  # for (u.idx in 1:length(utility.b)) {
  #     print(  attr(competition.l$competition[[u.idx]]$curve, "b") <- as.character(utility.b[u.idx]))
  # }
  # 
  # ## Write the modified competition file
  # competition.xml.modified <- as_xml_document(competition.l)
  # write_xml(competition.xml.modified, paste0(path.batch.data, "xml/Competition.xml"))
  
  
  
  
  ### Write giving-in and giving-up threshold
  
  
  
  # paramset
  
  csv.l <- list.files(paste0(path.batch.data, "/agents/"), pattern = "csv", full.names = T)
  csv.names <- list.files(paste0(path.batch.data, "/agents/"), pattern = "csv", full.names = F)
  
  csv.dt.l <- lapply(csv.l, read.csv)
  n.aft <- length(csv.dt.l)
  
  # csv.target <- lapply(1:n.aft, FUN = function(x) {res = csv.dt.l[[x]]; res[c("givingInDistributionMean", "givingUpDistributionMean")] = c(Gi[x], Gu[x]); res})
  # print(b.idx)
  # print(csv.target)
  
  
  targetparams = colnames(paramset)[-1] 
  csv.target <- lapply(1:n.aft, FUN = function(x) {res = csv.dt.l[[x]]; res[targetparams] = paramset[x,targetparams] ; res})
  print(csv.target)
  
  
  
  path.batch.out <- paste0("~/", GroupName, "/CSV/",RUN_ID  )
  
  if (!dir.exists(path.batch.out) ) { 
    dir.create(path.batch.out, recursive = T)
    dir.create(paste0(path.batch.out,"/AFT"), recursive = T)
    
  }
  
  sapply(1:n.aft, FUN = function(x) write.csv(csv.target[[x]], file = paste0(path.batch.out, "/AFT/", csv.names[x]), row.names = F, quote = F))
  sapply(1:n.aft, FUN = function(x) write.csv(csv.target[[x]], file = paste0(path.batch.data, "/agents/", csv.names[x]), quote=F, row.names = F))
  
  

  
  crafty.run.cmd <- paste0(jdk.bin.path, " -Xmx", java.mx, " -Xms", java.ms, " -Dfile.encoding=UTF-8 ", 
                           "-Dlog4j.configuration=log4j.properties ",
                           "-classpath lib/*:",  
                           ":", path.batch.base, "/bin:",  
                           "/Users/seo-b/workspace/tmp/ModellingUtilities.jar:",
                           "/Users/seo-b/workspace/tmp/CRAFTY_KIT_2020.jar:", "/Users/seo-b/workspace/tmp/CRAFTY_KIT_engine2020.jar",  
                           #"/lib/collections-generic-4.01.jar:", path.batch.base, "/lib/colt-1.2.0.jar:", path.batch.base, "/lib/commons-cli-1.2.jar:", path.batch.base, "/lib/commons-math3-3.2.jar:", path.batch.base, "/lib/CRAFTY-CoBRA_Impressions.jar:", path.batch.base, "/lib/gt-api-9.0.jar:", path.batch.base, "/lib/gt-data-9.0.jar:", path.batch.base, "/lib/gt-epsg-wkt-9.0.jar:", path.batch.base, "/lib/gt-main-9.0.jar:", path.batch.base, "/lib/gt-metadata-9.0.jar:", path.batch.base, "/lib/gt-opengis-9.0.jar:", path.batch.base, "/lib/gt-referencing-9.0.jar:", path.batch.base, "/lib/gt-shapefile-9.0.jar:", path.batch.base, "/lib/guava-12.0.1.jar:", path.batch.base, "/lib/jai_core.jar:", path.batch.base, "/lib/javacsv.jar:", path.batch.base, "/lib/javaRasters.jar:", path.batch.base, "/lib/jchart2d-3.2.2.jar:", path.batch.base, "/lib/jep-2.23.jar:", path.batch.base, "/lib/jide-oss-2.9.7.jar:", path.batch.base, "/lib/JRI.jar:", path.batch.base, "/lib/jscience.jar:", path.batch.base, "/lib/jts-1.13.jar:", path.batch.base, "/lib/jung-algorithms-2.0.1.jar:", path.batch.base, "/lib/jung-api-2.0.1.jar:", path.batch.base, "/lib/jung-graph-impl-2.0.1.jar:", path.batch.base, "/lib/jung-io-2.0.1.jar:", path.batch.base, "/lib/LARA_Base.jar:", path.batch.base, "/lib/LARA_Toolbox.jar:", path.batch.base, "/lib/log4j-1.2.17.jar:", path.batch.base, "/lib/ModellingUtilities.jar:", path.batch.base, "/lib/monte-cc.jar:", path.batch.base, "/lib/MORe.jar:", path.batch.base, "/lib/ParMa.jar", 
                           " org.volante.abm.serialization.ModelRunner -d ", path.batch.data,  " -f ", scenario.filename , " -o 99 -r 1 -n 5 -sr 4 ", ifelse(INTERACTIVE,yes =  "-i", no = ""))
   
  ### @todo copy FR file 
  
  
  # file.copy(paste0(path.batch.data, "Paramsets/FunctionalRoles.xml", 
  
  run.log = system(crafty.run.cmd, wait = T, intern = F, ignore.stdout = F, ignore.stderr = F, invisible = F, ) # , show.output.on.console = T)
  
  ### set wd back 
  # setwd(oldwd)
  
  
  # After the running, execute the below line to copy output CSV files to the Run_ID folder
  # system(paste0("rsync -a --exclude='.*' ", path.batch.base, "/output/* ", path.batch.out, "/"))
  
  print(paste("the result is saved in ", path.batch.base))
  #system(paste0("rm -rf ", path.batch.base))
  
  # writeLines(run.log, con = paste0(path.batch.base, "/log.txt"))
  
  
  return(run.log)
}


































