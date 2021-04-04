library(plot3D)
library(plotly)
library(rgl)
library(stringr)
library(RColorBrewer)
library(doMC)
library(gplots)

if (!require("webshot")) install.packages("webshot")


# Fragmentation statistics
library(SDMTools)
# 

library(latticeExtra)
library(raster)
library(parmigene)
library(infotheo)


# AFT table 
# Agent Functional Type	Composition 
# 1 Intensive arable farming	Intensively farmed (IA)
# 2 Intensive pastoral farming	Intensively grass  (IP)
# 3 Intensive agro-forestry mosaic	Intensively farmed, intensively grass, managed forest (Int_AF)
# 4 Intensive farming	Intensively farmed, intensively grass                   (Int_Fa)
# 5 Managed forestry	Managed forest                                          (MF)
# 6 Mixed farming	Intensively farmed, intensively grass, extensively grass    (Mix_Fa)
# 7 Mixed pastoral farming	intensively grass, extensively grass,   (Mix_P)
# 8 very extensively grass                                          
# 9 Mixed forest	Managed forest, unmanaged forest                (Mix_For)
# 10 Extensive pastoral farming	Extensively grass                   (EP)
# 11 Extensive agro-forestry mosaic	extensively grass,              (Ext_AF)
# 12 very extensively grass, managed forest                         
# 13 Very extensive pastoral farming	Very extensively grass      (VEP)
# 14 Multifunctional	4 or more land uses in uncommon combination  (Multifun)
# 15 Minimal management	very extensively grass, unmanaged forest, unmanaged land (Min_man)
# 16 Unmanaged land	Unmanaged land                      (UL)
# 17 Unmanaged forest	Unmanaged forest                (UMF)
# 18 Peri-urban	Any combination with > 40% urban area   (P-Ur)
# 19 Urban	Urban                                       (Ur)

#             0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16
# EP          0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0  216
# Ext_AF   1155    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# IA          0 2998    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# Int_AF      0    0 2579    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# Int_Fa      0    0    0 1400    0    0    0    0    0    0    0    0    0    0    0    0    0
# IP          0    0    0    0 1157    0    0    0    0    0    0    0    0    0    0    0    0
# MF          0    0    0    0    0 3575    0    0    0    0    0    0    0    0    0    0    0
# Min_man     0    0    0    0    0    0 1546    0    0    0    0    0    0    0    0    0    0
# Mix_Fa      0    0    0    0    0    0    0 1250    0    0    0    0    0    0    0    0    0
# Mix_For     0    0    0    0    0    0    0    0   69    0    0    0    0    0    0    0    0
# Mix_P       0    0    0    0    0    0    0    0    0  442    0    0    0    0    0    0    0
# Multifun    0    0    0    0    0    0    0    0    0    0 4364    0    0    0    0    0    0
# P-Ur        0    0    0    0    0    0    0    0    0    0    0  162    0    0    0    0    0
# UL          0    0    0    0    0    0    0    0    0    0    0    0 2325    0    0    0    0
# UMF         0    0    0    0    0    0    0    0    0    0    0    0    0   61    0    0    0
# Ur          0    0    0    0    0    0    0    0    0    0    0    0    0    0   18    0    0
# VEP         0    0    0    0    0    0    0    0    0    0    0    0    0    0    0  554    0

setwd("~/Dropbox/KIT_Modelling/CRAFTY/EUpaper/Batchruns/")



craftylayer.names <- c("Service.Meat", "Service.Crops", "Service.Diversity", "Service.Timber", "Service.Carbon", "Service.Urban", "Service.Recreation", "Capital.Crop.productivity", "Capital.Forest.productivity", "Capital.Grassland.productivity", "Capital.Financial.capital", "Capital.Human.capital", "Capital.Social.capital", "Capital.Manufactured.capital", "Capital.Urban.capital", "LandUseIndex", "Agent", "Competitiveness")


aft.names.fromzero <- c( "Ext_AF", "IA", "Int_AF", "Int_Fa", "IP", "MF", "Min_man", "Mix_Fa", "Mix_For", "Mix_P", "Multifun", "P-Ur", "UL", "UMF", "Ur", "VEP", "EP")

# Lon-Lat projection 
proj4.LL <- CRS("+proj=longlat +datum=WGS84")

# Proj4js.defs["EPSG:3035"] etrs89/etrs-laea
# Scope: Single CRS for all Europe. Used for statistical mapping at all scales and other purposes where true area representation is required.
# Reference: http://spatialreference.org/ref/epsg/3035/
proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";


n.thread = 1 
registerDoMC(n.thread)

# Cell ID and cooridnates 
ctry.ids <- read.csv("~/Dropbox/KIT/CLIMSAVE/IAP/Cell_ID_LatLong.csv")
x.lat.v = sort(unique(ctry.ids$Longitude))
y.lon.v = sort(unique(ctry.ids$Latitude))


input.filename = "Results/Paramset1/RCP2_6-SSP1/RCP2_6-SSP1-0-0-EU-Cell-2016.csv"


getClassStat = function(input.filename) { 
  
  input.m = read.csv2(input.filename, sep = ",")
  
  input.m$lon = x.lat.v[input.m$X]
  input.m$lat = y.lon.v[input.m$Y]
  
  
  # Create a spatial pixels data frame using the lon-lat table (Cell_ID_LatLong.csv) and the input data 
  result.spdf <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(input.m$lon, input.m$lat), proj4string = proj4.LL), data = data.frame(result.tmp), tolerance = 0.0011)
  
  # res.ETRS = ClassStat(map.etrs_laea, cellsize = 15000, bkgd = NA, latlon = F)
  res.LL = ClassStat( raster(result.spdf), cellsize = 15000, bkgd = NA, latlon = T)
  
  
  return(res.LL)
}








stop("ends here")









MODISLandCover_ClassStat.l <- readRDS(file = paste0("RData/MODISLandCover_ClassStat.RData"))
CRAFTY_ClassStats.l <- readRDS(file = paste0("RData/CRAFTY_ClassStat_nbatch486.RData"))


nrow.tmp1 <- 9
ncol.tmp1 <- 17 
CRAFTY.cs.mean.frac.dim.l <- lapply(CRAFTY_ClassStats.l, FUN = function(x1)   sapply(1:17, FUN = function(x2) as.numeric(sapply(x1, FUN = function(x3) x3[x3$class==x2, "mean.frac.dim.index"]))))


boxplot(t(CRAFTY.cs.mean.frac.dim.l[[1]]))
frac.avg <- sapply(CRAFTY.cs.mean.frac.dim.l, mean, na.rm=T)
frac.med <- sapply(CRAFTY.cs.mean.frac.dim.l, median, na.rm=T)

frac1 <- by(frac.avg, INDICES = list(params.expand.df$GivingIn, params.expand.df$GivingUp, params.expand.df$ServiceLevelNoise), FUN = c)
# dim(b)
frac1 <- (array(frac1, dim = c(9,9,6)))


par(mfrow=c(2,3))
frac_12 <- t(apply(frac1, MARGIN = c(1,2), FUN = mean))
image(frac_12)
frac_13 <- t(apply(frac1, MARGIN = c(1,3), FUN = mean))
image(frac_13)
frac_23 <- t(apply(frac1, MARGIN = c(2,3), FUN = mean))
image(frac_23)


frac1.med <- by(frac.med, INDICES = list(params.expand.df$GivingIn, params.expand.df$GivingUp, params.expand.df$ServiceLevelNoise), FUN = c)
# dim(b)
frac1.med <- (array(frac1.med, dim = c(9,9,6)))

frac.med_12 <- t(apply(frac1.med, MARGIN = c(1,2), FUN = median))
image(frac.med_12)
frac.med_13 <- t(apply(frac1.med, MARGIN = c(1,3), FUN = median))
image(frac.med_13)
frac.med_23 <- t(apply(frac1.med, MARGIN = c(2,3), FUN = median))
image(frac.med_23)  





stop("ends here")

library(stringr)
library(RColorBrewer)
library(gplots)

setwd("~/Dropbox/KIT_Modelling/CRAFTY/Calibration/")
# Mutual Information 

mi.files <- list.files("CRAFTY_runs_new_n486", pattern="mi", full.names = T)
mi.idx <- as.numeric(str_extract(mi.files, pattern ="(?<=mi_)[0-9]*"))


mi.files.ord <- mi.files[order(mi.idx)]
mi.m <- sapply(mi.files.ord, FUN = function(x) read.csv(x)[, 2])
str(mi.avg <- colMeans(mi.m))
(mi.med <- apply(mi.m, MARGIN = 2, FUN = median))

# plot(mi.avg, mi.med)

# boxplot(cbind(mi.med, mi.avg))

# str(mi.avg <- colMeans(mi.m))
lc.col <- c(brewer.pal(12, "Paired"), rich.colors(5))

targetyears <- 2006:2013
n.targetyears <- length(targetyears)
n.thread <- detectCores() -1 


givingInDistributionMean.v <- seq(-2, 2, 0.5)
givingUpDistributionMean.v <- seq(-2, 2, 0.5)
serviceLevelNoiseMax.v <- seq(0, 1, 0.2)

params.expand.df <- expand.grid(GivingIn = givingInDistributionMean.v,GivingUp= givingUpDistributionMean.v, ServiceLevelNoise= serviceLevelNoiseMax.v, KEEP.OUT.ATTRS=T)

val <- expand.grid(abc = c("a", "b", "c"), num = 1:4, k= paste0("k", 1:2))
array(unlist(val), dim = c(3,4,2))


b <- by(mi.avg, INDICES = list(params.expand.df$GivingIn, params.expand.df$GivingUp, params.expand.df$ServiceLevelNoise), FUN = c)
dim(b)
b2 <- (array(b, dim = c(9,9,6)))

b2.med <- by(mi.med, INDICES = list(params.expand.df$GivingIn, params.expand.df$GivingUp, params.expand.df$ServiceLevelNoise), FUN = c)
b2.med <- (array(b2.med, dim = c(9,9,6)))

# 
# b[3,4,2]
# b2[3,4,2]
# 
# b==b2
# b2b

b2_12 <- t(apply(b2, MARGIN = c(1,2), FUN = mean))
image(b2_12)
b2_13 <- t(apply(b2, MARGIN = c(1,3), FUN = mean))
image(b2_13)
b2_23 <- t(apply(b2, MARGIN = c(2,3), FUN = mean))
image(b2_23)

b2.med_12 <- t(apply(b2.med, MARGIN = c(1,2), FUN = median))
image(b2.med_12)
b2.med_13 <- t(apply(b2.med, MARGIN = c(1,3), FUN = median))
image(b2.med_13)
b2.med_23 <- t(apply(b2.med, MARGIN = c(2,3), FUN = median))
image(b2.med_23)

par(mfrow=c(1,1))
library(lattice)
levelplot(b2_12, xlab="givingInDistributionMean", ylab="givingUpDistributionMean", main = "Mutual Information between CRAFTY and MODIS (2006-2013)", col.regions = rev(heat.colors(30)), row.values = givingInDistributionMean.v, column.values = givingUpDistributionMean.v)







### mlrMBO 
# install.packages("mlrMBO")
# install.packages("DiceKriging")
# install.packages("rgenoud")
# install.packages("GGally")
# install.packages("lhs")

library(mlr)
library(mlrMBO)



# Various infill criteria (aka. acquisition functions) are available:
#     
# Expected improvement (EI)
# Upper/Lower confidence bound (LCB, aka. statistical lower or upper bound)
# Augmented expected improvement (AEI)
# Expected quantile improvement (EQI)
# API for custom infill criteria
# 
# # Purpose
# This Vignette shows you how to use mlrMBO for a guided optimization. In this setting mlrMBO proposes a candidate configuration and you can then decide for yourself whether you want to evaluate it or another value. You have to evaluate the objective function manually. The value and the result have to be feed back to mlrMBO. Afterwards you can request the next candidate and so on.

# Introduction
# Before we start the optimization you need to define the search space:
ps = makeParamSet(
  makeNumericParam("GivingIn", lower = -2, upper = 2),
  makeNumericParam("GivingUp", lower = -2, upper = 2)
  , makeNumericParam("ServiceLevelNoise", lower = 0, upper = 1)
  
)

# # Discrete case
# ps = makeParamSet(
#     makeDiscreteParam("GivingIn", givingInDistributionMean.v),
#     makeDiscreteParam("GivingUp", givingUpDistributionMean.v),
#     makeDiscreteParam("ServiceLevelNoise", serviceLevelNoiseMax.v)
# )

ps

# Furthermore we need an initial design that includes the results of the evaluated function
# des = generateDesign(n = 7, par.set = ps)
# des$y = NA
# des <- params.expand.df
# attr(des)

des <- expand.grid(GivingIn = givingInDistributionMean.v,GivingUp= givingUpDistributionMean.v, ServiceLevelNoise= serviceLevelNoiseMax.v, KEEP.OUT.ATTRS = F)
# des <- expand.grid(GivingIn = givingInDistributionMean.v,GivingUp= givingUpDistributionMean.v, KEEP.OUT.ATTRS = F)

# des <- expand.grid(GivingIn = givingInDistributionMean.v,ServiceLevelNoise= serviceLevelNoiseMax.v, KEEP.OUT.ATTRS = F)
str(des)

# After evaluating the objective function manually we can add the results

# des$y = c(1.20, 0.97, 0.91, 3.15, 0.58, 1.12, 0.50)

des$y = mi.med
# des$y = as.numeric(b2_12)


# k1 = (matrix(1:12, nrow = 4))
# k1
# as.numeric(k1)

library(parallel)
n_thread = detectCores() 
n_thread = 1

# Now we define our mlrMBO-Control object. For this example we stick to the defaults except that we set the infill-criterion to the Expected Improvement
ctrl = makeMBOControl(propose.points = n_thread )
ctrl = setMBOControlInfill(ctrl, crit = crit.ei)
# ctrl = setMBOControlInfill(ctrl, crit = crit.cb)



set.seed(1978)
des.init <- des[sample(1:nrow(des), size = 10),]
# des386 <- des[101:486,] 
# ctrl = makeTuneControlGrid(resolution = 1)


# undebug(dfRowsToList)
# debug(initSMBO)

# At each state the opt.state object can be plotted to visualize the predictions of the surrogate model
# plot(opt.state)
# The first panel shows the value of the infill criterion. The higher the value the more this area is desirable to be explored to find the optimum. In the following panels the mean prediction of the surrogate and the uncertainty estimation is plotted.

# Let’s see which point MBO suggests we should evaluate in the next step:

n_chain = 30
n.iter = as.integer(n_chain / n_thread)

res.m <- matrix(nrow = n.iter, ncol = 3)


# These information are enough to get us started and initialize the sequential MBO.
opt.state = initSMBO(par.set = ps, design = des.init, control = ctrl, minimize = FALSE, noisy = TRUE, show.info = T)
# 
library(parallelMap)
parallelStartMulticore(cpus = n_thread, show.info = TRUE)

# human in the loop type (because it is a pseudo run using the pre-runs)

for (i in 1:n.iter){ 
  
  cat(i)
  proposed <- proposePoints(opt.state)
  # print(proposed)
  
  
  
  if (nrow( proposed$prop.points)>1) { 
    
    proposed_gi = proposed$prop.points[,1]
    proposed_gu = proposed$prop.points[,2]
    proposed_sn = proposed$prop.points[,3]
    
  } else { 
    proposed_gi = proposed$prop.points[1]
    proposed_gu = proposed$prop.points[2]
    proposed_sn = proposed$prop.points[3]
    
  }
  
  newGivingIn <- sapply(proposed_gi , FUN = function(x) givingInDistributionMean.v[which.min(abs(givingInDistributionMean.v - as.numeric(x)))])
  newGivingUp <- sapply(proposed_gu , FUN = function(x) givingUpDistributionMean.v[which.min(abs(givingUpDistributionMean.v - as.numeric(x)))])
  newServiceLevelNoise <- sapply(proposed_sn , FUN = function(x) serviceLevelNoiseMax.v[which.min(abs(serviceLevelNoiseMax.v - as.numeric(x)))])
  
 
  param.try <- data.frame(GivingIn = newGivingIn, GivingUp = newGivingUp, ServiceLevelNoise  = newServiceLevelNoise)
  print(param.try)
  
   
  newMi = data.frame(t(sapply(1:nrow(param.try), FUN = function(x) subset(des, des$GivingIn == newGivingIn[x] & des$GivingUp == newGivingUp[x] & des$ServiceLevelNoise == newServiceLevelNoise[x]))))
  
  opt.state = updateSMBO(opt.state, x =param.try, y =  (newMi$y))
  
  
  
   
  
  # finalizeSMBO(opt.state)# 
  # # print(as.numeric(opt.state$opt.result$mbo.result$x) == as.numeric(res$x))
  # param.recommended<- as.numeric(opt.state$opt.result$mbo.result$x)
  # print(param.recommended)
  # res.m[i, ] <-param.recommended
  
}
 
parallelStop()


res = finalizeSMBO(opt.state)

res_opt_path = data.frame(res$opt.path)
str(res_opt_path)

proposed = proposePoints(opt.state)

plot(proposed$prop.points$GivingIn, proposed$crit.vals, pch=15)
  

plot(res_opt_path$GivingIn, res_opt_path$ei, type="p")

summary(res_opt_path)


res_y <- by(res_opt_path$y, INDICES = list(res_opt_path$GivingIn, res_opt_path$GivingUp, res_opt_path$ServiceLevelNoise), FUN = c)
dim(res_y)
res_y2 <- as.array(res_y)

b2.med <- by(mi.med, INDICES = list(params.expand.df$GivingIn, params.expand.df$GivingUp, params.expand.df$ServiceLevelNoise), FUN = c)
b2.med <- (array(b2.med, dim = c(9,9,6)))

# 
# b[3,4,2]
# b2[3,4,2]
# 
# b==b2
# b2b

y2_12 <- t(apply(res_y2, MARGIN = c(1,2), FUN = mean))
image(res_opt_path$GivingIn, res_opt_path$GivingUp, res_opt_path$y)


## Write a panel function (after examining 'args(panel.levelplot) to see what
## will be being passed on to the panel function by levelplot())
myPanel <- function(x, y, z, ...) {
  panel.levelplot(x,y,z,...)
  # panel.text(x, y, round(z, 3))
}


levelplot(y ~ GivingIn*GivingUp, res_opt_path, panel = myPanel)
levelplot(y ~ GivingIn*ServiceLevelNoise, res_opt_path, panel = myPanel)
levelplot(y ~ GivingUp*ServiceLevelNoise, res_opt_path, panel = myPanel)












levelplot(b2_12, xlab="givingInDistributionMean", ylab="givingUpDistributionMean", main = "Mutual Information between CRAFTY and MODIS (2006-2013)", col.regions = rev(heat.colors(30)), row.values = givingInDistributionMean.v, column.values = givingUpDistributionMean.v)


set.seed(1978)
# ctrl = generateRandomDesign(ps, n = 100)
# ctrl

 

# We don’t have to stick to the suggestion and evaluate another point:
# x = data.frame(q = 1.7, v = 1)

# After we evaluated the objective function manually we get a return value of 2.19. We take both values to update MBO:
# updateSMBO(opt.state, x = x, y = 2.19)

# Now we can plot the state again and ask for a proposal:
plot(opt.state)

# hist(res.m[1:200,1])
# hist(res.m[1:100,2])
# hist(res.m[1:100,3])
pdf("Results/CRAFTY_calibration_mlrmbo_timeseries.pdf", width = 8, height = 6, pointsize = 10)

plot(res_opt_path$GivingIn, type="l", xlab = "Iteration", ylab = "Value", main="MBO suggested optimal parameter values")
# axis(side = 1, at = seq(0, 150, 25))
lines(res_opt_path$GivingUp, type="l", col="red")
lines(res_opt_path$ServiceLevelNoise, type="l", col="blue")
legend("bottomright", legend = paste("", c("Giving-In", "Giving-Up", "ServiceLevelNoiseMax")), col=c("black", "red", "blue"), lty=1, bty="n")
dev.off()


plot(res_opt_path$GivingIn, res_opt_path$y, type="p", xlab = "Iteration", ylab = "Value", main="MBO suggested optimal parameter values")
plot(res_opt_path$GivingUp, res_opt_path$y, type="p", xlab = "Iteration", ylab = "Value", main="MBO suggested optimal parameter values")
plot(res_opt_path$ServiceLevelNoise, res_opt_path$y, type="p", xlab = "Iteration", ylab = "Value", main="MBO suggested optimal parameter values")


pdf("Results/CRAFTY_calibration_mlrmbo_infill.pdf", width = 8, height = 6, pointsize = 10)

plot(res_opt_path$cb, type="l", xlab = "Iteration", ylab = "Value", main="MBO suggested optimal parameter values")
plot(res_opt_path$y, type="l", xlab = "Iteration", ylab = "Value", main="MBO suggested optimal parameter values")


plot(res_opt_path$GivingIn, type="l", ylim=c(-2, 2), xlab = "Iteration", ylab = "Value", main="MBO suggested optimal parameter values")
# axis(side = 1, at = seq(0, 150, 25))
lines(res_opt_path$GivingUp, type="l", col="red")
lines(res_opt_path$ServiceLevelNoise, type="l", col="blue")
legend("bottomright", legend = paste("", c("Giving-In", "Giving-Up", "ServiceLevelNoiseMax")), col=c("black", "red", "blue"), lty=1, bty="n")
dev.off()









# (prop = proposePoints(opt.state))
# 
# # This time we evaluated the exact proposed points and get a value of 0.13.
# updateSMBO(opt.state, x = prop$prop.points, y = 0.01761373)


# Let’s assume we want to stop here. To get to the usual MBO result you can call:

res = finalizeSMBO(opt.state)
# res$x
# res$y
# 
# str(res$opt.path)
# 
# plot(res$opt.path$par.set$pars$GivingIn, res$opt.path$par.set$pars$GivingUp)



# plot(res$opt.path$env$path$GivingIn,res$opt.path$env$path$GivingUp, type="l")

# getOpt(res)

# plotOptPath(res$final.opt.state$opt.path)
plotOptPath(res$opt.path, colours = c("red", "blue", "green", "orange"), scale="robust")
proposePoints(opt.state)

res


res.grid.final.avg = colMeans(des[order(des$y, decreasing = T)[1],])

res.final.avg = colMeans(res.m[151:200,])



plot(res$opt.path$env$path$GivingIn, type="l")
plot(res$opt.path$env$path$GivingUp, type="l")
plot(res$opt.path$env$path$ServiceLevelNoise, type="l")


plot(res$opt.path$env$path$y, type = "l")

plot(res$final.opt.state)


fd.top1 =  (frac.avg[order(des$y, decreasing = T)[1]])
fd.top10 = mean(frac.avg[order(des$y, decreasing = T)[1:10]])


prop.points = res.final.avg
newGivingIn <- givingInDistributionMean.v[which.min(abs(givingInDistributionMean.v - as.numeric(prop.points[1])))]
newGivingUp <- givingUpDistributionMean.v[which.min(abs(givingUpDistributionMean.v - as.numeric(prop.points[2])))]
newServiceLevelNoise <- serviceLevelNoiseMax.v[which.min(abs(serviceLevelNoiseMax.v - as.numeric(prop.points[3])))]
# newFD = subset(frac.avg, des$GivingIn == newGivingIn & des$GivingUp == newGivingUp & des$ServiceLevelNoise == newServiceLevelNoise)
idx <- which(des$GivingIn == newGivingIn & des$GivingUp == newGivingUp & des$ServiceLevelNoise == newServiceLevelNoise)
des[idx,]
frac.avg[idx]




pdf("Results/CRAFTY_calibration_mlrmbo.pdf", width = 5, height = 6, pointsize = 10)
par(mfrow=c(1,1))

# des[order(des$y, decreasing = T)[1:10],-4]

boxplot(des[order(des$y, decreasing = T)[1:10],-4], ylab="Value", main = "Suggested parameter values")
points(1:3, res.grid.final.avg[-4], col = "blue", pch=15)
# boxplot(res.m[1:200,], add=F, ylim=c(-0.5, 2))
points(1:3, res.final.avg, col = "red", pch=15)

legend("topright", legend = c("Top-1 batch run", "Top-10 batch runs", "MBO suggested"), col=c( "blue", "black", "red"), pch=c(15,NA,15), lty=c(0,1,0), bty="n")
dev.off()
# # mlr::plotCalibration(res)
# # mlr::plotLearningCurve(res)
# 
# 
# 
pdf("Results/CRAFTY_calibration_mlrmbo_FD.pdf", width = 5, height = 6, pointsize = 10)
par(mfrow=c(1,1))

# des[order(des$y, decreasing = T)[1:10],-4]

boxplot(frac.avg, ylab="Fractal Dimension", main = "Avg. Fractal Dimension of the batch runs (n=486)")
points(1, frac.avg[idx], col = "red", pch=15)
# boxplot(res.m[1:200,], add=F, ylim=c(-0.5, 2))
points(1, fd.top1, col = "blue", pch=15)
points(1, fd.top10, col = "black", pch=15)

legend("topright", legend = c("Top-1 batch run", "Top-10 batch runs", "MBO suggested"), col=c( "blue", "black", "red"), pch=15, bty="n")
dev.off()


# plot3D::contour3D(params.expand.df$GivingIn,params.expand.df$GivingUp, z = as.numeric(b2_12))
# surface3d(params.expand.df$GivingIn,params.expand.df$GivingUp, z = as.numeric(b2_12))
# 
# p <- plot_ly(x=givingInDistributionMean.v, y= givingUpDistributionMean.v, z = b2_12) %>% add_surface()
# p
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
# chart_link = api_create(p, filename="surface-2")
# chart_link

#     
p_b12 <- plot_ly(x=givingInDistributionMean.v, y= givingUpDistributionMean.v, z = b2_12, xlab="GivingIn") %>% add_surface()
p_b12
layout(p_b12, title="Mutual Information (avg.)", xaxis = list(title = "GivingIn",  zeroline = TRUE,range = c(-2, 2)), yaxis = list(title = "GivingUp", zeroline = TRUE,range = c(-2, 2)))
# 
# p_b13 <- plot_ly(x=givingInDistributionMean.v, y= serviceLevelNoiseMax.v, z = (b2_13)) %>% add_surface()
# 
# p_b23 <- plot_ly(x=givingUpDistributionMean.v, y= serviceLevelNoiseMax.v, z = b2_23) %>% add_surface()  #  %>% add_trace(type="contour") 
# 
# p_f12 <- plot_ly(x=givingInDistributionMean.v, y= givingUpDistributionMean.v, z = frac_12) %>% add_surface()
# p_f13 <- plot_ly(x=givingInDistributionMean.v, y= serviceLevelNoiseMax.v, z = frac_13) %>% add_surface()
# p_f23 <- plot_ly(x=givingUpDistributionMean.v, y= serviceLevelNoiseMax.v, z = frac_23) %>% add_surface()  #  %>% add_trace(type="contour") 




p_c_b12 <- plot_ly(x=givingInDistributionMean.v, y= givingUpDistributionMean.v, z = b2_12, xlab="GivingIn") %>% add_trace(type="contour", contours = list(start = 0.16,  end = 0.173, size = 0.001))
l_c_b12 <- layout(p_c_b12, title="Mutual Information (avg.)",  xaxis = list(title = "GivingIn",  zeroline = TRUE,range = c(-2, 2)), yaxis = list(title = "GivingUp", zeroline = TRUE,range = c(-2, 2)))

p_c_b13 <- plot_ly(x=givingInDistributionMean.v, y= serviceLevelNoiseMax.v, z = (b2_13),  autocontour = F) %>% add_trace(type="contour", contours = list(start = 0.16,  end = 0.173, size = 0.001))
l_c_b13 <- layout(p_c_b13, title="Mutual Information (avg.)",  xaxis = list(title = "GivingIn",  zeroline = TRUE), yaxis = list(title = "ServiceLevelNoise", zeroline = TRUE))

p_c_b23 <- plot_ly(x=givingUpDistributionMean.v, y= serviceLevelNoiseMax.v, z = (b2_23),  autocontour = F) %>% add_trace(type="contour", contours = list(start = 0.16,  end = 0.173, size = 0.001))
l_c_b23 <- layout(p_c_b23, title="Mutual Information (avg.)",  xaxis = list(title = "GivingUp",  zeroline = TRUE), yaxis = list(title = "ServiceLevelNoise", zeroline = TRUE))


ps <-
  subplot(l_c_b12, l_c_b13, l_c_b23,  nrows = 3, margin = c(0.01, 0.01, 0.0, 0.09), shareX = F, shareY = F, titleX = T, titleY = T)
#  heights = c(0.1, 0.9), widths = c(0.9, 0.1),
# tmpFile <- tempfile(tmpdir = ".", fileext = ".pdf")
export(layout(ps, showlegend=F), file = "CRAFTY_MODIS_MutualInformation_contour.pdf", vwidth = 700, vheight = 900)
# browseURL(tmpFile)


### Fractal dimension 

p_c_f12 <- plot_ly(x=givingInDistributionMean.v, y= givingUpDistributionMean.v, z = frac_12, xlab="GivingIn") %>% add_trace(type="contour", contours = list(start = 1.0071,  end = 1.0078, size = 0.00005))
l_c_f12 <- layout(p_c_f12, title="Fractal Dimension (avg.)",  xaxis = list(title = "GivingIn",  zeroline = TRUE,range = c(-2, 2)), yaxis = list(title = "GivingUp", zeroline = TRUE,range = c(-2, 2)))

p_c_f13 <- plot_ly(x=givingInDistributionMean.v, y= serviceLevelNoiseMax.v, z = (frac_13),  autocontour = F) %>% add_trace(type="contour", contours = list(start = 1.0071,  end = 1.0078, size = 0.00005))
l_c_f13 <- layout(p_c_f13, title="Fractal Dimension (avg.)",  xaxis = list(title = "GivingIn",  zeroline = TRUE), yaxis = list(title = "ServiceLevelNoise", zeroline = TRUE))

p_c_f23 <- plot_ly(x=givingUpDistributionMean.v, y= serviceLevelNoiseMax.v, z = (frac_23),  autocontour = F) %>% add_trace(type="contour", contours = list(start = 1.0071,  end = 1.0078, size = 0.00005))
l_c_f23 <- layout(p_c_f23, title="Fractal Dimension (avg.)",  xaxis = list(title = "GivingUp",  zeroline = TRUE), yaxis = list(title = "ServiceLevelNoise", zeroline = TRUE))


ps <-
  subplot(l_c_f12, l_c_f13, l_c_f23,  nrows = 3, margin = c(0.01, 0.01, 0.0, 0.09), shareX = F, shareY = F, titleX = T, titleY = T)
#  heights = c(0.1, 0.9), widths = c(0.9, 0.1),
# tmpFile <- tempfile(tmpdir = ".", fileext = ".pdf")
export(layout(ps, showlegend=F), file = "CRAFTY_MODIS_FractalDimension_contour.pdf", vwidth = 700, vheight = 900)
# browseURL(tmpFile)







p_c_f13 <- plot_ly(x=givingInDistributionMean.v, y= serviceLevelNoiseMax.v, z = (frac_13), autocontour = F) %>% add_trace(type="contour") # , contours = list(start = 0.16,  end = 0.17, size = 0.001))

# png("test.png", width = 1000, height = 1000)
l_c_f13 <- layout(p_c_f13, title="Fractal Dimension",  xaxis = list(title = "GivingIn",  zeroline = TRUE), yaxis = list(title = "ServiceLevelNoise", zeroline = TRUE))
# dev.copy(device = png, filename= "test.png", width = 1000, height = 1000)






layout(ps, showlegend=F)

# dev.off()
# plot_ly(x=givingInDistributionMean.v, y= givingUpDistributionMean.v, z = frac_13, type = "histogram2dcontour")
# plot_ly(x=givingInDistributionMean.v, y= givingUpDistributionMean.v, z = frac_13, type = "histogram")


plot_ly(x=givingInDistributionMean.v, y= givingUpDistributionMean.v, z = frac_12) %>% add_trace() 

p1 <- plot_ly(x=givingInDistributionMean.v, y= givingUpDistributionMean.v, z = frac_12) %>% add_contour() #  %>% add_surface() # 
py <-  plot_ly(y = givingUpDistributionMean.v, type = "histogram2dcontour")
px <-  plot_ly(x = givingInDistributionMean.v, type = "histogram")

ps <- subplot(px, plotly_empty(), p1, py,   nrows = 2, heights = c(0.1, 0.9), widths = c(0.9, 0.1), margin = 0, shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE)
layout(ps, showlegend=F)

p1
# plotplotly<- function (x,y,z) { 
#  subplot(
#     plot_ly(x = x, type = "contour"),
#     plotly_empty(),
#     plot_ly(x = x, y = y, type = "histogram2dcontour"),
#     plot_ly(y = y, type = "histogram"),
#     nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
#     shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
# )}
# s <- plotplotly(givingInDistributionMean.v, givingUpDistributionMean.v, z = frac_12)
# p <- layout(s, showlegend = FALSE)
# p



(params.expand.df[1:40,])

arr.test <- array(1:486, dim = c(length(serviceLevelNoiseMax.v), length(givingUpDistributionMean), length(givingInDistributionMean.v)))

arr.test[1,,]
arr.test[,1,]
arr.test[,,1]


lambda <- function(info) sign(info)*sqrt(1-exp(-2*abs(info)))

mi.norm <- apply(mi.m, MARGIN = c(1,2), lambda)
mi.avg <- colMeans(mi.norm)


mi.r <- cbind(params.expand.df, MI = mi.avg)
# mi.2d <- matrix(mi.avg, nrow = 

# library(lattice)    
# wireframe(mi.r, shade = TRUE) 
# aspect = c(61/87, 0.4),
# light.source = c(10,0,10))





MODIS.cs.mean.frac.dim <- sapply(0:16, FUN = function(x) sapply(MODISLandCover_ClassStat.l, FUN = function(x2) x2[x2$class==(x), "mean.frac.dim.index"]))



boxplot(t(MODIS.cs.mean.frac.dim))

plot(MODIS.cs.mean.frac.dim[,1], type="l", ylim=c(1.0, 1.03))
sapply(2:16, FUN = function(x) lines(MODIS.cs.mean.frac.dim[,x], col=x))

library(Kendall)
MannKendall(MODIS.cs.mean.frac.dim[,1])
MODIS.mk <- do.call(rbind, apply(MODIS.cs.mean.frac.dim, MARGIN = 2, FUN = function(x) MannKendall(x)))

MODIS.mk[,2]


plot(CRAFTY.cs.mean.frac.dim.l[[1]][,1], type="l", ylim=c(1.0, 1.03), lwd=0, xlab="Years", ylab="Fractal dimension")
for (r.idx in 1:n.batch) { 
  sapply(1:16, FUN = function(x) lines(CRAFTY.cs.mean.frac.dim.l[[r.idx]][,x], col=x, lwd=0.1))
}


plot(mi.m[,1], type="l", ylim=c(0.0, 0.3), lwd=0, xlab="Years", ylab="Mutual Information (normalised)")
for (r.idx in 1:n.batch) { 
  lines(mi.norm[,r.idx], col="red", lwd=0.3)
}

