setwd("~/Dropbox/eclipse_workspace/EU1/data/")

csv.l <- list.files("agents default backup/", pattern = "csv", full.names = T)
csv.names <- list.files("agents default backup/", pattern = "csv", full.names = F)

csv.dt.l <- lapply(csv.l, read.csv)
n.aft <- length(csv.dt.l)


###
# java -cp "lib/*" org.volante.abm.serialization.ModelRunner -d ~/workspace/EU1/data -f Scenario.xml -o 0 -r 1 -n 1 -sr 0


crafty.run.cmd <- paste0("java -Xmx10g -Dfile.encoding=UTF-8 -classpath /Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/resources.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/rt.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jsse.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jce.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/charsets.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jfr.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/sunec.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/nashorn.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/cldrdata.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/jfxrt.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/dnsns.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/localedata.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/sunjce_provider.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/sunpkcs11.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/jaccess.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/zipfs.jar:/System/Library/Java/Extensions/MRJToolkit.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/bin:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/config/log:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/collections-generic-4.01.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/colt-1.2.0.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/commons-cli-1.2.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/commons-math3-3.2.jar:/Users/seo-b/workspace/EU1/lib/CRAFTY-CoBRA_2018.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/gt-api-9.0.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/gt-data-9.0.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/gt-epsg-wkt-9.0.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/gt-main-9.0.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/gt-metadata-9.0.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/gt-opengis-9.0.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/gt-referencing-9.0.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/gt-shapefile-9.0.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/guava-12.0.1.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jai_core.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/javacsv.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/javaRasters.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jchart2d-3.2.2.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jep-2.23.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jide-oss-2.9.7.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/JRI.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jscience.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jts-1.13.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jung-algorithms-2.0.1.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jung-api-2.0.1.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jung-graph-impl-2.0.1.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/jung-io-2.0.1.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/LARA_Base.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/LARA_Toolbox.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/log4j-1.2.17.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/ModellingUtilities.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/monte-cc.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/MORe.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/ParMa.jar", 
                         
                         
                         
                         ":/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/repast.simphony.bin_and_src.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/saf.core.run", 

                         "time.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/simple-xml-2.7.1.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/stax-api-1.0.1.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/units-0.01.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/Uranus.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/vecmath-1.3.1.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/wstx-asl-3.2.6.jar:/Users/seo-b/workspace/CRAFTY_CoBRA_Impressions/lib/xmlgraphics-commons-1.3.1.jar org.volante.abm.serialization.ModelRunner -d /Users/seo-b/workspace/EU1/data -f Scenario.xml -o 0 -r 1 -n 1 -sr 0"
)


crafty.run.cmd.linux <- paste0("java -Xmx10g -Dfile.encoding=UTF-8 -classpath /Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/resources.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/rt.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jsse.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jce.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/charsets.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jfr.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/sunec.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/nashorn.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/cldrdata.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/jfxrt.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/dnsns.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/localedata.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/sunjce_provider.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/sunpkcs11.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/jaccess.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/ext/zipfs.jar:/System/Library/Java/Extensions/MRJToolkit.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/bin:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/config/log:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/collections-generic-4.01.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/colt-1.2.0.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/commons-cli-1.2.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/commons-math3-3.2.jar:/home/alan/Dropbox/eclipse_workspace/EU1/lib/CRAFTY-CoBRA_2018.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/gt-api-9.0.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/gt-data-9.0.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/gt-epsg-wkt-9.0.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/gt-main-9.0.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/gt-metadata-9.0.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/gt-opengis-9.0.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/gt-referencing-9.0.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/gt-shapefile-9.0.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/guava-12.0.1.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jai_core.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/javacsv.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/javaRasters.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jchart2d-3.2.2.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jep-2.23.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jide-oss-2.9.7.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/JRI.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jscience.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jts-1.13.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jung-algorithms-2.0.1.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jung-api-2.0.1.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jung-graph-impl-2.0.1.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/jung-io-2.0.1.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/LARA_Base.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/LARA_Toolbox.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/log4j-1.2.17.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/ModellingUtilities.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/monte-cc.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/MORe.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/ParMa.jar", 
                         
                         
                         
                         ":/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/repast.simphony.bin_and_src.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/saf.core.run", 
                         
                         "time.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/simple-xml-2.7.1.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/stax-api-1.0.1.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/units-0.01.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/Uranus.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/vecmath-1.3.1.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/wstx-asl-3.2.6.jar:/home/alan/Dropbox/eclipse_workspace/CRAFTY_CoBRA_Impressions/lib/xmlgraphics-commons-1.3.1.jar org.volante.abm.serialization.ModelRunner -d /home/alan/Dropbox/eclipse_workspace/EU1/data -f Scenario.xml -o 0 -r 1 -n 1 -sr 0"
)


# path.batch.base <- "~/workspace/EU1/data/"
path.batch.base <- "~/Dropbox/eclipse_workspace/EU1/data/"

# Agents. Land managers and institutions are represented explicitly as agents in CRAFTY SIRIOS. Both share a common architecture where agents are made up of a behavioural type (BT) describing their capabilities of decision making, a functional role (FR) characterising function and role in the system, and a collection of properties.

# A land use agent is able to leverage the capitals available on a land parcel (represented as a cell) to provide a range of services. Each agent has a production function as part of its FR, which maps capital levels onto service provision (see section 7. Submodels, Production). An agent’s benefit according to a given level of service provision can be calculated based on societal demands, overall supply levels and marginal utility functions (see Figure 1). See Table 1 for a complete list of agent variables.

# Table 1: Variables and States of agents and their FR and BT. Default states are given in parenthesis if applicable.
# Benefit: Denotes the agent’s current benefit value (]-∞,∞[)
# Giving-in threshold: During competition, if a competing agent’s competitiveness is greater than the incumbent agent’s by a value larger than the giving-in threshold then the incumbent agent relinquishes that cell to the competitor. (]-∞,∞[)
# Giving-up threshold:  If an agent’s competitiveness falls below its giving-up threshold (defines the minimum return an agent is willing to accept from a cell) it needs to aban- don the particular cell (considering giving-up probability). (]-∞,∞[)
# Giving-up probability
# Probability for giving up in case the agent’s competitiveness falls below the giving-up threshold ([0,1] (0.2 by default))

# serviceLevelNoiseMax: Noise describes the maximum value of a uniform distribution which is added to the AFT’s default optimal production.

# Functional component 

# Role: Refers to the Functional Role (FR) 
# Optimal production: Amount of produced service in case of optimal conditions (all relevant capitals = 1.0) [0, ∞[ or formula (based on JEP5)
# Capital sensitivities: Sensitivities of production towards capital values ([0,1])
# Production model: Component responsible for the calculation of service provision (SimpleProductionModel)
# Behavioural component Type: Refers to Behavioural Type (BT)
# Decision triggers: Set of components that evaluate cur- rent conditions and may trigger the decision making they are configured for. (UnmetDemandDT, RegionalUnmetDemandDT)
# [LARA component]: Reference to the LARA component which performs the actual decision making. Only for cognitive BTs. See Ta- ble 1: Variables and States of agents and their FR and BT. Default states are given in parenthesis if applicable. for the component’s variables. (CobraLAgentComp)

givingInDistributionMean.v <- seq(-2, 2, 0.5)
givingUpDistributionMean <- seq(-2, 2, 0.5)
serviceLevelNoiseMax.v <- seq(0, 1, 0.2)

params.expand.df <- expand.grid(givingInDistributionMean.v, givingUpDistributionMean, serviceLevelNoiseMax.v)
n.batch <- nrow(params.expand.df) 

# for (b.idx in 1:n.batch) {
    for (b.idx in 342) {
        
    csv.target <- lapply(1:n.aft, FUN = function(x) {res = csv.dt.l[[x]]; res[c("givingInDistributionMean", "serviceLevelNoiseMax")] = params.expand.df[b.idx,]; res}) 
    print(b.idx)
    print(csv.target)
    
    path.batch.out <- paste0(path.batch.base, "agents_Run", b.idx)
    if (!dir.exists(path.batch.out) ) { 
     dir.create(path.batch.out)   
    }
    
    # /home/alan/Dropbox/eclipse_workspace/EU1/data/worlds/EU/regionalisations/28/socialNetwork/28_EU_SocialNetwork_HDFF.xml
    
    sapply(1:n.aft, FUN = function(x) write.csv(csv.target[[x]], file = paste0(path.batch.out, "/", csv.names[x]), row.names = F))
    
    sapply(1:n.aft, FUN = function(x) write.csv(csv.target[[x]], file = paste0(path.batch.base, "/agents/", csv.names[x]), row.names = F))
    # file.copy("agents default backup/BehaviouralTypes.xml", to = "agents/BehaviouralTypes.xml") 
    # file.copy("agents default backup/FunctionalRoles.xml", to = "agents/FunctionalRoles.xml") 
    
    system(crafty.run.cmd)
    
    # dir.create(paste0("output/calib/EU/28/Baseline/0-0/EU_Run", b.idx))
     
    # file.copy("output/calib/EU/28/Baseline/0-0/EU", to = paste0("output/calib/EU/28/Baseline/0-0/EU_Run", b.idx), recursive = T, overwrite = T, ) 
    system(paste("cp -r", "output/calib/EU/28/Baseline/0-0/EU", paste0("output/calib/EU/28/Baseline/0-0/EU_Run", b.idx)))
    
}

# mpirun -np 8 -output-filename output/V-Baseline-4/0-1/CRAFTY_V-Baseline-4_0-1 








 











