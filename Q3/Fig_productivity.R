library(raster)
library(maptools)
data(wrld_simpl)
library(raster)
library(dplyr)
library(rgdal)
library(dismo)
library(viridis)
library(ggplot2)
library(tidyr)
library(lme4)
library(rgeos)
library(cowplot)

################ downsample enm layer resolution to match earthstat 
enm <- raster('/Users/emilywork/Downloads/all.CLY250.tif')
enm <- aggregate(enm, fact=10)

enm.s <- raster('/Users/emilywork/Downloads/sorg.CLY250.tif')
enm.s <- aggregate(enm.s, fact=10)

enm.z <- raster('/Users/emilywork/Downloads/maiz.CLY250.tif')
enm.z <- aggregate(enm.z, fact=10)

enm.m <- raster('/Users/emilywork/Downloads/mill.CLY250.tif')
enm.m <- aggregate(enm.m, fact=10)

################ read in the production maps from earthstat
sorg.yld <- raster('/Users/emilywork/Desktop/StrigaMacroecology/EarthStat/sorghum_HarvAreaYield_Geotiff/sorghum_YieldPerHectare.tif')
sorg.yld <- crop(sorg.yld, enm)
mill.yld <- raster('/Users/emilywork/Desktop/StrigaMacroecology/EarthStat/millet_HarvAreaYield_Geotiff/millet_YieldPerHectare.tif')
mill.yld <- crop(mill.yld, enm)
maiz.yld <- raster('/Users/emilywork/Desktop/StrigaMacroecology/EarthStat/maize_HarvAreaYield_Geotiff/maize_YieldPerHectare.tif')
maiz.yld <- crop(maiz.yld, enm)

all.yld <- maiz.yld + mill.yld + sorg.yld

meta <- read.csv('/Users/emilywork/Documents/GitHub/StrigaMacroecologyMS/SI.dat.1.30.20.csv')
meta <- subset(meta, emergence != "NA") # 27 localities

### extract productivity 
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

p <- ggplot(tmp.wide, aes(x=productivity, y=pdi)) + geom_point(size=2, alpha=0.5) + ylab("PDI") + xlab("Productivity") + geom_smooth(method ="lm", col="black", lty=2, level=0.9)+theme_minimal()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
anova(lm(pdi ~ productivity, data=tmp.wide), lm(pdi ~ 1, data=tmp.wide), test="Chisq")

arrange(tmp.wide, -pdi)
arrange(tmp.wide, -productivity)

### habitat suitability at location of all S. hermonthica occurrences 
# occurrences for subset on different hosts 
shgeo <- read.csv('/Users/emilywork/Downloads/pnas.1908707117.sd03.csv', header=T)
coordinates(shgeo) <- ~lon+lat
sh.s <- subset(shgeo, sorghum==1)
sh.z <- subset(shgeo, maize==1)
sh.m <- subset(shgeo, millet==1)

r <- enm
res(r) <- 0.01 #0.1 degree ~ 11 km
r <- extend(r, extent(r)+1)
acsel.s <- gridSample(sh.s, r, n=1)
acsel.z <- gridSample(sh.z, r, n=1)
acsel.m <- gridSample(sh.m, r, n=1)

hs <- raster::extract(enm.s, acsel.s)
acsel.s <- as.data.frame(acsel.s)
acsel.s$host <- "sorghum"
acsel.s$hs <- hs

hs <- raster::extract(enm.z, acsel.z)
acsel.z <- as.data.frame(acsel.z)
acsel.z$host <- "maize"
acsel.z$hs <- hs

hs <- raster::extract(enm.m, acsel.m)
acsel.m <- as.data.frame(acsel.m)
acsel.m$host <- "millet"
acsel.m$hs <- hs

acsel <- rbind.data.frame(acsel.s, acsel.z, acsel.m)
acsel$prod <- raster::extract(all.yld, cbind(acsel$lon,acsel$lat))

q <- ggplot(acsel, aes(x=prod, y=hs, fill=host, col=host)) + geom_point(alpha=0.5, pch=21, size=0.75) + ylab("Parasite HS") + xlab("Productivity") + geom_smooth(se=T, lwd=1, alpha=0.5, level=0.9)+theme_minimal()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), legend.position = "none") + xlim(c(0,4.75)) + scale_fill_manual(values=c('gold2','plum','sienna3'), name="Host")+ scale_color_manual(values=c('gold3','plum','sienna3'), name="Host")
q
pdf(file="ProductivityFig", height=5, width=3.22)
plot_grid(p,q, align="v", axis="r", labels=c('A','B'), cols=1)
dev.off()

acsel.sub <- subset(acsel, host =="millet" | host=="maize")
wilcox.test(subset(acsel, host=="maize")$prod, subset(acsel, host=="sorghum")$prod, alternative="greater")
