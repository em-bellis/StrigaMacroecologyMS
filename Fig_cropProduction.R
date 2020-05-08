##2.26.20
##determine correlation b/w relative emergence and crop area harvested

library(raster)
library(maptools)
data(wrld_simpl)
library(raster)
library(dplyr)
library(rgdal)
library(dismo)
library(maps)
data(worldMapEnv)
library(viridis)
library(ggplot2)
library(tidyr)
library(lme4)
library(pals)

################ downsample enm layer resolution to match earthstat 
enm <- raster('/Users/emilywork/Downloads/all.CLY250.tif')
enm <- aggregate(enm, fact=10)

################ read in the production maps from earthstat
sorg <- raster('/Users/emilywork/Desktop/EarthStat/sorghum_HarvAreaYield_Geotiff/sorghum_HarvestedAreaHectares.tif')
sorg <- crop(sorg, enm)
mill <- raster('/Users/emilywork/Desktop/EarthStat/millet_HarvAreaYield_Geotiff/millet_HarvestedAreaHectares.tif')
mill <- crop(mill, enm)
maiz <- raster('/Users/emilywork/Desktop/EarthStat/maize_HarvAreaYield_Geotiff/maize_HarvestedAreaHectares.tif')
maiz <- crop(maiz, enm)

############### create mask based on habitat suitability of all occurrence model and distance from striga hermonthica
core <- calc(enm, fun=function(x){ x[x < 0.1] <- NA; return(x)} )

shgeo <- read.csv('/Users/emilywork/Downloads/pnas.1908707117.sd03.csv', header=T)
coordinates(shgeo) <- ~lon+lat
projection(shgeo) <- CRS('+proj=longlat +datum=WGS84')
x <- circles(shgeo, d=200000, lonlat=T) #sample within radius of 200km
pol <- polygons(x)
all_within <- mask(core, pol)

################  load gps points for experimental studies
meta <- read.csv('SI.dat.1.30.20.csv')
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
pdf(file="CropProductionFig.pdf", pointsize=10, width=3.23, height=3.3)
plot(dom2, col=c('sienna3','plum','gold2'),legend=F, xaxt='n', yaxt='n')
legend(-18,-15, legend=c("sorghum","millet","maize"), fill=c('sienna3','plum','gold2'), box.col=NA, cex=0.5)
map(database="world", xlim=c(-20,60),ylim=c(-40,45),add=T, col="grey40", lwd=0.5, mar=NA)
points(cbind(unique(meta$lon),unique(meta$lat)), pch=17, cex=0.4)
scalebar(1000, xy = c(40,-35), label=" 1000 km",type = "line", divs = 1, lwd = 2,  adj=c(0.5, -0.5), lonlat = TRUE, cex=0.6)
dev.off()

############## fig for relative emergence. 
tmp <- meta %>% select(locality, emergence, host) %>% group_by(locality, host) %>% summarize(emg = mean(emergence), sd = sd(emergence), n=length(emergence))
tmp2 <- tmp %>% group_by(locality) %>% summarize(total=sum(emg))
tmp3 <- inner_join(tmp, tmp2)

ggplot(tmp3, aes(x=reorder(locality,-total), y=emg, col=host, fill=host)) + geom_bar(position="stack", stat="identity", alpha=0.4) +theme_minimal()+ scale_colour_manual(values=c('gold2','plum','sienna3')) + scale_fill_manual(values=c('gold2','plum','sienna3'))+ theme(axis.text.x=element_text(angle=90, vjust=0.2, hjust=1)) + ylab("Relative emergence") + xlab("Locality") + geom_errorbar()
