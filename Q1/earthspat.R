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

################  load ENM layers
enm.sorg<- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/ENMs/sorg.CLY250.tif')
#enm.mill<- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/ENMs/mill.CLY250.tif')
#enm.maiz<- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/ENMs/maiz.CLY250.tif')
#enm.all<- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/ENMs/all.CLY250.tif')

################ read in the production maps from harvestchoice
setwd('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/SPAM/')
sorg2 <- raster('sorg_r_h--SSA.tif')  ##the scaling of harvest area is much closer among the three crops compared to production
mill2 <- raster('pmil_r_h--SSA.tif')
maiz2 <- raster('maiz_r_h--SSA.tif')

################ read in the production maps from earthstat
sorg <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/EarthStat/sorghum_HarvAreaYield_Geotiff/sorghum_HarvestedAreaHectares.tif')
sorg <- crop(sorg, enm.sorg)
mill <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/EarthStat/millet_HarvAreaYield_Geotiff/millet_HarvestedAreaHectares.tif')
mill <- crop(mill, enm.sorg)
maiz <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/EarthStat/maize_HarvAreaYield_Geotiff/maize_HarvestedAreaHectares.tif')
maiz <- crop(maiz, enm.sorg)

################  load gps points for experimental studies
meta <- read.csv('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig3/SI.dat.1.30.20.csv')
meta <- subset(meta, emergence != "NA") # 27 localities

################  create summary dataframe
rel.emg <- as.data.frame(meta %>%
	select(c(locality,emergence,host)) %>%
	group_by(locality, host) %>%
	summarise(mean = mean(emergence), n = n()) %>%
	inner_join(meta) %>%
	select(c(locality,host,lat,lon,mean)) %>%
	unique() %>%
	spread(host,mean))

################  calculate avg production value for each crop within 50 km
ram <- cbind.data.frame(rel.emg$lon,rel.emg$lat)
colnames(ram) <-c("Lon","Lat")
coordinates(ram) <- ~Lon + Lat

# for earth stat
rel.emg$maize.es.50km <- NULL
for (i in 1:nrow(rel.emg)){
	x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
	pol <- polygons(x)
	m.50 <- mask(maiz, pol)
	mean.50 <- cellStats(m.50, 'mean', na.rm=T)
	mean.50
    rel.emg$maize.es.50km[i] <- mean.50
}
rel.emg$sorg.es.50km <- NULL
for (i in 1:nrow(rel.emg)){
	x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
	pol <- polygons(x)
	m.50 <- mask(sorg, pol)
	mean.50 <- cellStats(m.50, 'mean', na.rm=T)
	mean.50
    rel.emg$sorg.es.50km[i] <- mean.50
}
rel.emg$mill.es.50km <- NULL
for (i in 1:nrow(rel.emg)){
	x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
	pol <- polygons(x)
	m.50 <- mask(mill, pol)
	mean.50 <- cellStats(m.50, 'mean', na.rm=T)
	mean.50
    rel.emg$mill.es.50km[i] <- mean.50
}

# for harvest choice
rel.emg$maize.hc.50km <- NULL
for (i in 1:nrow(rel.emg)){
	x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
	pol <- polygons(x)
	m.50 <- mask(maiz2, pol)
	mean.50 <- cellStats(m.50, 'mean', na.rm=T)
	mean.50
    rel.emg$maize.hc.50km[i] <- mean.50
}
rel.emg$sorg.hc.50km <- NULL
for (i in 1:nrow(rel.emg)){
	x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
	pol <- polygons(x)
	m.50 <- mask(sorg2, pol)
	mean.50 <- cellStats(m.50, 'mean', na.rm=T)
	mean.50
    rel.emg$sorg.hc.50km[i] <- mean.50
}
rel.emg$mill.hc.50km <- NULL
for (i in 1:nrow(rel.emg)){
	x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
	pol <- polygons(x)
	m.50 <- mask(mill2, pol)
	mean.50 <- cellStats(m.50, 'mean', na.rm=T)
	mean.50
    rel.emg$mill.hc.50km[i] <- mean.50
}

################  create relative area harvested variables as a predictor
meta2 <- inner_join(meta, rel.emg)
meta2$hc.tot <- meta2$sorg.hc.50km + meta2$maize.hc.50km + meta2$mill.hc.50km
meta2$es.tot <- meta2$sorg.es.50km + meta2$maize.es.50km + meta2$mill.es.50km
meta2$sorg.hc.rat <- meta2$sorg.hc.50km/meta2$hc.tot
meta2$sorg.es.rat <- meta2$sorg.es.50km/meta2$es.tot
meta2$mill.hc.rat <- meta2$mill.hc.50km/meta2$hc.tot
meta2$mill.es.rat <- meta2$mill.es.50km/meta2$es.tot
meta2$maiz.hc.rat <- meta2$maize.hc.50km/meta2$hc.tot
meta2$maiz.es.rat <- meta2$maize.es.50km/meta2$es.tot

################ run mixed effects models
# for sorghum
mod.sorg <- lmer(emergence ~ sorg.hc.rat + (1 | host.gen), data=meta2[meta2$host=="sorghum",])
mod.sorg.n <- lmer(emergence ~ (1 | host.gen), data=meta2[meta2$host=="sorghum",])
anova(mod.sorg, mod.sorg.n) #p=0.003
mod.sorg <- lmer(emergence ~ sorg.es.rat + (1 | host.gen), data=meta2[meta2$host=="sorghum",])
mod.sorg.n <- lmer(emergence ~ (1 | host.gen), data=meta2[meta2$host=="sorghum",])
anova(mod.sorg, mod.sorg.n) #p=0.03

mod.sorg.enm <- lmer(emergence ~ ENM_a_s50km + (1 | host.gen), data=meta2[meta2$host=="sorghum",])
anova(mod.sorg.enm, mod.sorg)

# for millet
mod <- lmer(emergence ~ mill.hc.rat + (1 | host.gen), data=meta2[meta2$host=="millet",])
mod.n <- lmer(emergence ~ (1 | host.gen), data=meta2[meta2$host=="millet",])
anova(mod, mod.n) #p < 0.0001
mod <- lmer(emergence ~ mill.es.rat + (1 | host.gen), data=meta2[meta2$host=="millet",])
mod.n <- lmer(emergence ~ (1 | host.gen), data=meta2[meta2$host=="millet",])
anova(mod, mod.n) #p < 0.0001

mod.mill.enm <- lmer(emergence ~ ENM_a_m50km + (1 | host.gen), data=meta2[meta2$host=="millet",])
anova(mod.mill.enm, mod)


# for maize
mod <- lmer(emergence ~ maiz.hc.rat + (1 | host.gen), data=meta2[meta2$host=="maize",])
mod.n <- lmer(emergence ~ (1 | host.gen), data=meta2[meta2$host=="maize",])
anova(mod, mod.n) #p =0.005
mod <- lmer(emergence ~ maiz.es.rat + (1 | host.gen), data=meta2[meta2$host=="maize",])
mod.n <- lmer(emergence ~ (1 | host.gen), data=meta2[meta2$host=="maize",])
anova(mod, mod.n) #p =0.03

mod.maiz.enm <- lmer(emergence ~ ENM_a_z50km + (1 | host.gen), data=meta2[meta2$host=="maize",])
anova(mod.maiz.enm, mod)
