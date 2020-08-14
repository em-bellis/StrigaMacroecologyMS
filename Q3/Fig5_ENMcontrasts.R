## This script is used to recreate Figure 5
## please contact ebellis@astate.edu with any questions!

library(raster)
library(viridis)
library(maps)
library(pals)
library(dplyr)

all <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/all.CLY250.tif')
maiz <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/maiz.CLY250.tif')
sorg <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/sorg.CLY250.tif')
mill <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/mill.CLY250.tif')

meta <- read.csv('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig3/SI.dat.1.30.20.csv')
meta <- subset(meta, emergence != "NA") # 27 localities

meta.sorg <- as.data.frame(meta %>% filter(host=="sorghum") %>% group_by(locality) %>% summarize(Emg = mean(emergence), lat = mean(lat), lon=mean(lon),ENM = mean(ENM_a_s50km)))
meta.mill <- as.data.frame(meta %>% filter(host=="millet") %>% group_by(locality) %>% summarize(Emg = mean(emergence), lat = mean(lat), lon=mean(lon),ENM = mean(ENM_a_m50km)))
meta.maize <- as.data.frame(meta %>% filter(host=="maize") %>% group_by(locality) %>% summarize(Emg = mean(emergence), lat = mean(lat), lon=mean(lon),ENM = mean(ENM_a_z50km)))

avs <- all-sorg
avm <- all-mill
avz <- all-maiz

##mask based on habitat suitability of all occurrence model
core <- calc(all, fun=function(x){ x[x < 0.2] <- NA; return(x)} )
avs.core <- mask(avs, core)
avm.core <- mask(avm, core)
avz.core <- mask(avz, core)

##part A, mill
pdf('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig3/A_mill.pdf')
plot(avm.core, col=coolwarm(20), breaks=seq(-1,1,by=0.1),legend=T, xaxt='n', yaxt='n')
map(database="world", xlim=c(-20,60),ylim=c(-40,45),axes=FALSE, add=T, col="grey50", lwd=0.7)
dev.off()

##part B, sorg
pdf('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig3/B_sorg.pdf')
plot(avs.core, col=coolwarm(20), breaks=seq(-1,1,by=0.1),legend=T, xaxt='n', yaxt='n')
map(database="world", xlim=c(-20,60),ylim=c(-40,45),axes=FALSE, add=T, col="grey50", lwd=0.7)
#points(meta.sorg$lat, meta.sorg$lon, col="black",cex=0.6, pch=21)
dev.off()

##part C, maize
pdf('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig3/C_maiz.pdf')
plot(avz.core, col=coolwarm(20), breaks=seq(-1,1,by=0.1),legend=T, xaxt='n', yaxt='n')
map(database="world", xlim=c(-20,60),ylim=c(-40,45),axes=FALSE, add=T, col="grey50", lwd=0.7)
scalebar(1000, xy = c(35,-33), type = "line", divs = 1, lonlat = TRUE)
dev.off()
