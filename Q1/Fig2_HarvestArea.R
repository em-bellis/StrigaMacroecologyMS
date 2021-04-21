## This script is used to recreate Figure 3
## please contact ebellis@astate.edu with any questions!

library(raster)
library(maptools)
data(wrld_simpl)
library(raster)
library(dplyr)
library(rgdal)
library(dismo)
library(maps)
data(worldMapEnv)
library(ggplot2)
library(tidyr)
library(lme4)
library(pals)
library(rgeos)

################ downsample enm layer resolution to match earthstat 
enm <- raster('/Users/emilywork/Downloads/all.CLY250.tif')
enm <- aggregate(enm, fact=10)

################ read in the production maps from earthstat
sorg <- raster('/Users/emilywork/Desktop/StrigaMacroecology/EarthStat/sorghum_HarvAreaYield_Geotiff/sorghum_HarvestedAreaHectares.tif')
sorg.yld <- raster('/Users/emilywork/Desktop/StrigaMacroecology/EarthStat/sorghum_HarvAreaYield_Geotiff/sorghum_YieldPerHectare.tif')
sorg <- crop(sorg, enm)
sorg.yld <- crop(sorg.yld, enm)
mill <- raster('/Users/emilywork/Desktop/StrigaMacroecology/EarthStat/millet_HarvAreaYield_Geotiff/millet_HarvestedAreaHectares.tif')
mill.yld <- raster('/Users/emilywork/Desktop/StrigaMacroecology/EarthStat/millet_HarvAreaYield_Geotiff/millet_YieldPerHectare.tif')
mill <- crop(mill, enm)
mill.yld <- crop(mill.yld, enm)
maiz <- raster('/Users/emilywork/Desktop/StrigaMacroecology/EarthStat/maize_HarvAreaYield_Geotiff/maize_HarvestedAreaHectares.tif')
maiz.yld <- raster('/Users/emilywork/Desktop/StrigaMacroecology/EarthStat/maize_HarvAreaYield_Geotiff/maize_YieldPerHectare.tif')
maiz <- crop(maiz, enm)
maiz.yld <- crop(maiz.yld, enm)

all.yld <- maiz.yld + mill.yld + sorg.yld

############### create mask based on habitat suitability of all occurrence model and distance from striga hermonthica
core <- calc(enm, fun=function(x){ x[x < 0.1] <- NA; return(x)} )

shgeo <- read.csv('/Users/emilywork/Downloads/pnas.1908707117.sd03.csv', header=T) # supplement from https://doi.org/10.1073/pnas.1908707117 
coordinates(shgeo) <- ~lon+lat
projection(shgeo) <- CRS('+proj=longlat +datum=WGS84')
x <- circles(shgeo, d=200000, lonlat=T) #sample within radius of 200km
pol <- polygons(x)
all_within <- mask(core, pol)

################  load gps points for experimental studies
meta <- read.csv('DataFiles/SI.dat.1.30.20.csv')
meta <- subset(meta, emergence != "NA") # 27 localities

################ calcuate layer indicating dominant crop harvested 
f.sorg <- sorg/(sorg+mill+maiz)
f.mill <- mill/(sorg+mill+maiz)
f.maiz <- maiz/(sorg+mill+maiz)

dom.s <- overlay(f.sorg, f.mill, f.maiz, fun=Vectorize(function(x,y,z){ifelse(x>=y && x>=z,1,NA)}))
dom.m <- overlay(f.sorg, f.mill, f.maiz, fun=Vectorize(function(x,y,z){ifelse(y>x && y>=z,2,NA)}))
dom.z <- overlay(f.sorg, f.mill, f.maiz, fun=Vectorize(function(x,y,z){ifelse(z>x && z>y,3,NA)}))
dom <- cover(dom.s, dom.m)
dom <- cover(dom, dom.z)
dom <- setExtent(dom, core)
dom <- mask(dom, all_within)

################ estimate area in km^2 
dom.s <- setExtent(dom.s, all_within); dom.s <- mask(dom.s, all_within); cellStats(area(dom.s, na.rm=TRUE), sum)
dom.m <- setExtent(dom.m, all_within); dom.m <- mask(dom.m, all_within); cellStats(area(dom.m, na.rm=TRUE), sum)
dom.z <- setExtent(dom.z, all_within); dom.z <- mask(dom.z, all_within); cellStats(area(dom.z, na.rm=TRUE), sum)

cellStats(area(dom, na.rm=TRUE), sum)*100 # 100 hectares = 1 km^2
cellStats(area(all_within, na.rm=TRUE), sum)*100 # 100 hectares = 1 km^2

############## visualization 
## new df for meta
meta2 <- dplyr::select(meta, locality, lat, lon) %>% unique()
meta2$clust <- c(3,5,16,17,5,5,16,2,16,5,5,5,5,3,5,6,3,17,2,5,5,6,3,2,3,3,3)  #symbols based on groups from clustering analysis
dom.c <- crop(dom, extent(-20,45,-20,20))

pdf(file="CropProductionFig_A.pdf", pointsize=10, width=4.33, height=3)
plot(dom.c, col=c('sienna3','plum','gold2'),legend=F, xaxt='n', yaxt='n', xlim=c(-20,45),ylim=c(-5,20))
legend(-5,-15, legend=c("1","2","3","4","5","6"), pch=c(17,2,3,16,5,6), box.col=NA, cex=0.5, title="Group")
legend(-18,-15, legend=c("sorghum","millet","maize"), fill=c('sienna3','plum','gold2'), box.col=NA, cex=0.5, title ="Main crop")
map(database="world", xlim=c(-20,50),ylim=c(-10,20),add=T, col="grey40", lwd=0.5, mar=NA)
points(meta2$lon,meta2$lat, pch=meta2$clus, cex=0.7)
scalebar(1000, xy = c(-5,-10), label=" 1000 km",type = "line", divs = 1, lwd = 2,  adj=c(0.5, -0.5), lonlat = TRUE, cex=0.6)
dev.off()

pdf(file="CropProductionFig_B.pdf", pointsize=10, width=4.33, height=3)
plot(dom, col=c('sienna3','plum','gold2'),legend=F, xaxt='n', yaxt='n', xlim=c(-20,50),ylim=c(-40,45))
legend(-5,-5, legend=c("1","2","3","4","5","6"), pch=c(17,2,3,16,5,6), box.col=NA, cex=0.5, title="Group")
#legend(-18,-15, legend=c("sorghum","millet","maize"), fill=c('sienna3','plum','gold2'), box.col=NA, cex=0.5, title ="Main crop")
map(database="world", xlim=c(-20,50),ylim=c(-40,45),add=T, col="grey40", lwd=0.5, mar=NA)
scalebar(1000, xy = c(35,-32), label=" 1000 km",type = "line", divs = 1, lwd = 2,  adj=c(0.5, -0.5), lonlat = TRUE, cex=0.6)
dev.off()
