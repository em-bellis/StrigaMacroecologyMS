##Updated 6.29.20
##Crop Harvested as metric for specialization

library(raster)
library(rgdal)
library(maptools)
data(wrld_simpl)
library(dplyr)
library(dismo)
library(maps)
data(worldMapEnv)
library(tidyr)
library(lme4)
library(pals)
library(rgeos)
library(aplot)

##Geotiffs are available through earthstat http://www.earthstat.org/harvested-area-yield-175-crops/
##Download for sorghum, millet, and maize

### read in the production maps from earthstat
sorg <- raster('sorghum_HarvAreaYield_Geotiff/sorghum_HarvestedAreaHectares.tif')
sorg.yld <- raster('sorghum_HarvAreaYield_Geotiff/sorghum_YieldPerHectare.tif')
#sorg <- crop(sorg, enm)
#sorg.yld <- crop(sorg.yld, enm)
mill <- raster('millet_HarvAreaYield_Geotiff/millet_HarvestedAreaHectares.tif')
mill.yld <- raster('millet_HarvAreaYield_Geotiff/millet_YieldPerHectare.tif')
#mill <- crop(mill, enm)
#mill.yld <- crop(mill.yld, enm)
maiz <- raster('maize_HarvAreaYield_Geotiff/maize_HarvestedAreaHectares.tif')
maiz.yld <- raster('maize_HarvAreaYield_Geotiff/maize_YieldPerHectare.tif')
#maiz <- crop(maiz, enm)
#maiz.yld <- crop(maiz.yld, enm)

##total harvested area for focal hosts
all.harv <- maiz + mill + sorg

##relative harvested area for forcal hosts
maiz.harv.r <- (maiz/all.harv)
mill.harv.r <- (mill/all.harv)
sorg.harv.r <- (sorg/all.harv)

ENM.all <- read.csv("~/StrigaMacroecologyMS/DataFiles/New.Stand.1.6.20.csv")
Crop.Harvest <-ENM.all

Crop.Harvest$ENM_a_m50km <-NULL
Crop.Harvest$ENM_a_s50km <-NULL
Crop.Harvest$ENM_a_z50km <- NULL

##Convert coordinates for loop
##had to flip lon/lat, was plotting backwards
ram <- cbind.data.frame(Crop.Harvest$lat, Crop.Harvest$lon)
colnames(ram) <-c("Lat","Lon")
coordinates(ram) <- ~Lat + Lon

##visulize raster data with striga provenances from emperical studies 
##confirm "ram" is plotting correctly
plot(mill)
points(ram)

##sorghum loop
for (i in 1:nrow(Crop.Harvest)){
  x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
  pol <- polygons(x)
  m.50 <- mask(sorg.harv.r, pol)
  mean.50 <- cellStats(m.50, 'mean', na.rm=T)
  mean.50
  Crop.Harvest$sorg.50km[i] <- mean.50
}

##millet loop
for (i in 1:nrow(Crop.Harvest)){
  x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
  pol <- polygons(x)
  m.50 <- mask(mill.harv.r, pol)
  mean.50 <- cellStats(m.50, 'mean', na.rm=T)
  mean.50
  Crop.Harvest$mill.50km[i] <- mean.50
}

##maize loop
for (i in 1:nrow(Crop.Harvest)){
  x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
  pol <- polygons(x)
  m.50 <- mask(maiz.harv.r, pol)
  mean.50 <- cellStats(m.50, 'mean', na.rm=T)
  mean.50
  Crop.Harvest$maiz.50km[i] <- mean.50
}

##Emergence data
emrg <- read.csv("~~/StrigaMacroecologyMS/DataFiles/New.Stand.1.11.20.csv")
CH.dat <- merge(emrg, Crop.Harvest, by="locality", all=TRUE)

##Sorghum
s.1.ch <-lmer(emergence ~ (1 | host.gen), data=CH.dat[CH.dat$host=="sorghum",])
s.2.ch <-lmer(emergence ~ (1 | host.gen) + sorg.50km , data=CH.dat[CH.dat$host=="sorghum",])

anova(s.1.ch, s.2.ch, test="Chisqu")

##Millet
m.1.ch <-lmer(emergence ~ (1 | host.gen), data=CH.dat[CH.dat$host=="millet",])
m.2.ch <-lmer(emergence ~ (1 | host.gen) + mill.50km , data=CH.dat[CH.dat$host=="millet",])

anova(m.1.ch, m.2.ch, test="Chisqu")

##Maize
z.1.ch <-lmer(emergence ~ (1 | host.gen), data=CH.dat[CH.dat$host=="maize",])
z.2.ch <-lmer(emergence ~ (1 | host.gen) + maiz.50km , data=CH.dat[CH.dat$host=="maize",])

anova(z.1.ch, z.2.ch, test="Chisqu")

##approximated coefficents of linear models using Satterwaithes method for crop harvest
##note:: If the error code "[,5] out of bounds"appears, re-run models with "lmerTest" which 
#includes approximates more model parameters (including p-values) than "lme4"

require(lmerTest)

##Sorghum
#extract coefficients
coef.s.ch <- data.frame(coef(summary(s.2.ch)))
#use normal distribution to approximate p-value
coef.s.ch$p.z <- 2 * (1 - pnorm(abs(coef.s.ch$t.value)))
#get Satterthwaite-approximated degrees of freedom
coef.s$df.Satt <- coef(summary(s.2.ch))[, 3]
# get approximate p-values for a model
coef.s.ch$p.Satt <- coef(summary(s.2.ch))[, 5]
coef.s.ch

#repeat above for other crops 
##Millet
coef.m.ch <- data.frame(coef(summary(m.2.ch)))
coef.m.ch$p.z <- 2 * (1 - pnorm(abs(coef.m.ch$t.value)))
coef.m.ch$df.Satt <- coef(summary(m.2.ch))[, 3]
coef.m.ch$p.Satt <- coef(summary(m.2.ch))[, 5]
coef.m.ch

##Maize
coef.z.ch <- data.frame(coef(summary(z.2.ch)))
coef.z.ch$p.z <- 2 * (1 - pnorm(abs(coef.z.ch$t.value)))
coef.z.ch$df.Satt <- coef(summary(z.2.ch))[, 3]
coef.z.ch$p.Satt <- coef(summary(z.2.ch))[, 5]
coef.z.ch


