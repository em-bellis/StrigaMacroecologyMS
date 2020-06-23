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

shgeo <- read.csv('/Users/emilywork/Downloads/pnas.1908707117.sd03.csv', header=T)
coordinates(shgeo) <- ~lon+lat
projection(shgeo) <- CRS('+proj=longlat +datum=WGS84')
x <- circles(shgeo, d=200000, lonlat=T) #sample within radius of 200km
pol <- polygons(x)
all_within <- mask(core, pol)

################  load gps points for experimental studies
meta <- read.csv('/Users/emilywork/Documents/GitHub/StrigaMacroecologyMS/SI.dat.1.30.20.csv')
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
plot(dom, col=c('sienna3','plum','gold2'),legend=F, xaxt='n', yaxt='n')
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

### extract productivity 
all.yld <- maiz.yld + mill.yld + sorg.yld

tmp <- meta %>% group_by(locality,host) %>% summarize(Emg = mean(emergence), lat = mean(lat), lon=mean(lon))
tmp$productivity <- raster::extract(all.yld, cbind.data.frame(tmp$lon, tmp$lat))
tmp.wide <- as.data.frame(tmp %>% pivot_wider(names_from=host, values_from=Emg))
tmp.wide$spec <- NULL
for (i in 1:nrow(tmp.wide)){
  highest <- max(tmp.wide[i,5:7])
  sec_highest <- sort(tmp.wide[i,5:7],partial=2)[2]
  min_val <- min(tmp.wide[i,5:7])
  tmp.wide$pdi[i] <- ((highest - sec_highest) + (highest - min_val))/2
}
tmp.wide$pdi <- as.numeric(tmp.wide$pdi)
tmp.wide <- na.omit(tmp.wide)

ggplot(tmp.wide, aes(x=productivity, y=pdi)) + geom_point(size=2, alpha=0.5) + ylab("Difference in relative emergence \non two best hosts") + xlab("Total yield per hectare") + geom_smooth(method ="lm")
summary(lm(pdi ~ productivity, data=tmp.wide))
