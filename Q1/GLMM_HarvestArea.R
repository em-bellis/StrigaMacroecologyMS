###Updated 1.21.21
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
library(lmerTest)
#require(lmerTest)

##Geotiffs are available through earthstat http://www.earthstat.org/harvested-area-yield-175-crops/
##Download for sorghum, millet, and maize

#setwd("~/Path.To.GeoTiffs/")

### read in the production maps from earthstat
#Sorghum
sorg <- raster('sorghum_HarvAreaYield_Geotiff/sorghum_HarvestedAreaHectares.tif')
sorg.yld <- raster('sorghum_HarvAreaYield_Geotiff/sorghum_YieldPerHectare.tif')
#sorg <- crop(sorg, enm)
#sorg.yld <- crop(sorg.yld, enm)

#Millet
mill <- raster('millet_HarvAreaYield_Geotiff/millet_HarvestedAreaHectares.tif')
mill.yld <- raster('millet_HarvAreaYield_Geotiff/millet_YieldPerHectare.tif')
#mill <- crop(mill, enm)
#mill.yld <- crop(mill.yld, enm)

#Maize
maiz <- raster('maize_HarvAreaYield_Geotiff/maize_HarvestedAreaHectares.tif')
maiz.yld <- raster('maize_HarvAreaYield_Geotiff/maize_YieldPerHectare.tif')
#maiz <- crop(maiz, enm)
#maiz.yld <- crop(maiz.yld, enm)

##Total harvested area for focal hosts
all.harv <- maiz + mill + sorg

##Relative harvested area for focal hosts
maiz.harv.r <- (maiz/all.harv)
mill.harv.r <- (mill/all.harv)
sorg.harv.r <- (sorg/all.harv)

#setwd("~/Path.to.Striga.Macroecology.Repository")

##Dataframe with emergence from empircal studies and constructed 50km resolution ENMs
SI.dat <- read.csv("StrigaMacroecologyMS-master/DataFiles/SI.dat.1.30.20.csv")

##remove ENM columns
Crop.Harvest <- SI.dat[ , !grepl( "ENM" , names(SI.dat) ) ]

##Convert coordinates of Striga provenances for loop
ram <- cbind.data.frame(Crop.Harvest$lon, Crop.Harvest$lat)
colnames(ram) <-c("Lon","Lat")
coordinates(ram) <- ~Lon + Lat

##Visulize raster data with striga provenances from emperical studies 
##Confirm "ram" is plotting correctly
plot(mill)
points(ram)

##Extracting Crop Harvest Data and binding it to Crop.Harvest dataframe
##This takes awhile
#Sorghum loop
for (i in 1:nrow(Crop.Harvest)){
  x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
  pol <- polygons(x) #polygon
  m.50 <- mask(sorg.harv.r, pol) #polygon in relative crop space
  mean.50 <- cellStats(m.50, 'mean', na.rm=T)
  mean.50 #mean crop harvested in 50km radius polygon
  Crop.Harvest$sorg.50km[i] <- mean.50 #bind to data frame
}

#Millet loop
for (i in 1:nrow(Crop.Harvest)){
  x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
  pol <- polygons(x)
  m.50 <- mask(mill.harv.r, pol)
  mean.50 <- cellStats(m.50, 'mean', na.rm=T)
  mean.50
  Crop.Harvest$mill.50km[i] <- mean.50
}

#Maize loop
for (i in 1:nrow(Crop.Harvest)){
  x <- circles(ram[i], d=50000, lonlat=T) #sample within radius of 50km
  pol <- polygons(x)
  m.50 <- mask(maiz.harv.r, pol)
  mean.50 <- cellStats(m.50, 'mean', na.rm=T)
  mean.50
  Crop.Harvest$maiz.50km[i] <- mean.50
}

####GLMM####
##Sorghum##
s.1.ch <-lmer(emergence ~ (1 | host.gen), data=Crop.Harvest[Crop.Harvest$host=="sorghum",])
##QQplot
qqnorm(resid(s.1.ch), main = "Sorghum")
qqline(resid(s.1.ch))

##crop harvest as fixed effect 
s.2.ch <-lmer(emergence ~ (1 | host.gen) + sorg.50km , data=Crop.Harvest[Crop.Harvest$host=="sorghum",])
##QQplot
qqnorm(resid(s.2.ch), main = "Sorghum + Crop Harvest")
qqline(resid(s.2.ch))

anova(s.1.ch, s.2.ch, test="Chisqu") #P-value 0.03109 *

##Millet##
m.1.ch <-lmer(emergence ~ (1 | host.gen), data=Crop.Harvest[Crop.Harvest$host=="millet",])

##QQplot
qqnorm(resid(m.1.ch), main = "Millet")
qqline(resid(m.1.ch))

##crop harvest as fixed effect 
m.2.ch <-lmer(emergence ~ (1 | host.gen) + mill.50km , data=Crop.Harvest[Crop.Harvest$host=="millet",])
##QQplot
qqnorm(resid(m.2.ch), main = "Millet + Crop Harvest")
qqline(resid(m.2.ch))

anova(m.1.ch, m.2.ch, test="Chisqu") #P-value 3.884e-07 ***

##Maize##
z.1.ch <-lmer(emergence ~ (1 | host.gen), data=Crop.Harvest[Crop.Harvest$host=="maize",])
##QQplot
qqnorm(resid(z.1.ch), main = "Maize")
qqline(resid(z.1.ch))

##crop harvest as fixed effect 
z.2.ch <-lmer(emergence ~ (1 | host.gen) + maiz.50km , data=Crop.Harvest[Crop.Harvest$host=="maize",])
##QQplot
qqnorm(resid(z.2.ch), main = "Maize + Crop Harvest")
qqline(resid(z.2.ch))

anova(z.1.ch, z.2.ch, test="Chisqu") #P-value 0.02004 *

##approximated coefficents of linear models using Satterwaithes method for crop harvest
##note:: If the error code "[,5] out of bounds" appears, re-run above models with "lmerTest" which approximates more model parameters (including p-values) than "lme4"

##Sorghum##
#extract coefficients
coef.s.ch <- data.frame(coef(summary(s.2.ch))) #use normal distribution to approximate p-value
coef.s.ch$p.z <- 2 * (1 - pnorm(abs(coef.s.ch$t.value))) #get Satterthwaite-approximated degrees of freedom
coef.s$df.Satt <- coef(summary(s.2.ch))[, 3] # get approximate p-values for a model
coef.s.ch$p.Satt <- coef(summary(s.2.ch))[, 5]
coef.s.ch

##Millet##
coef.m.ch <- data.frame(coef(summary(m.2.ch)))
coef.m.ch$p.z <- 2 * (1 - pnorm(abs(coef.m.ch$t.value)))
coef.m.ch$df.Satt <- coef(summary(m.2.ch))[, 3]
coef.m.ch$p.Satt <- coef(summary(m.2.ch))[, 5]
coef.m.ch

##Maize##
coef.z.ch <- data.frame(coef(summary(z.2.ch)))
coef.z.ch$p.z <- 2 * (1 - pnorm(abs(coef.z.ch$t.value)))
coef.z.ch$df.Satt <- coef(summary(z.2.ch))[, 3]
coef.z.ch$p.Satt <- coef(summary(z.2.ch))[, 5]
coef.z.ch


####Crop Harvest Panes of the Specialization Figure####
library(cowplot)
library(ggplot2)

occ <- read.csv("StrigaMacroecologyMS-master/DataFiles/OccurrenceData.5.11.20.csv")
Specificity.Dat <- left_join(SI.dat, occ, by="locality")
##Add crop harvest
Specificity.Dat <-cbind(Specificity.Dat, Crop.Harvest[,9:11]) #rows 9:11 must be in order sorg.50km, mill.50km, maiz.50km

# subset data and create new object based on column query 
Specificity.sorg <- Specificity.Dat[Specificity.Dat$host=="sorghum",]
Specificity.sorg <- Specificity.sorg[c(-1,-9,-10,-12,-13,-14,-15,-16,-17,-19, -21,-22)]
colnames(Specificity.sorg) <- c("locality", "emergence","host", "host.gen", "lon", "lat", "ENM", "Study", "Occurrence", "Crop.Harvest")

Specificity.mill <- Specificity.Dat[Specificity.Dat$host=="millet",]
Specificity.mill <- Specificity.mill[c(-1,-8,-10,-12,-13,-14,-15,-16,-18,-19,-20,-22)]
colnames(Specificity.mill) <- c("locality", "emergence","host", "host.gen", "lon", "lat", "ENM", "Study", "Occurrence", "Crop.Harvest")

Specificity.maize <- Specificity.Dat[Specificity.Dat$host=="maize",]
Specificity.maize <- Specificity.maize[c(-1,-8,-9,-12,-13,-14,-15,-16,-17,-18,-20,-21)]
colnames(Specificity.maize) <- c("locality", "emergence","host", "host.gen", "lon", "lat", "ENM", "Study", "Occurrence", "Crop.Harvest")

new <-rbind(Specificity.sorg,Specificity.mill,Specificity.maize)

#Sorghum Crop Area Harvested vs Emergence
c.s <-ggplot(new, aes(x=Crop.Harvest, y=emergence, fill=host, col=host)) + 
  xlab("") + ylab("Mean Relative Emergence")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_jitter(data=subset(new,host=="sorghum"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=0.41061,slope=0.25071), color='sienna3',lwd=1.3) + #sorghum
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values=c('sienna3','gold2','plum'), name="Host")+ 
  scale_color_manual(values=c('sienna3','gold3','plum'), name="Host")

#Millet Crop Area Harvested vs Emergence
c.m <-ggplot(new, aes(x=Crop.Harvest, y=emergence, fill=host, col=host)) + 
  xlab("Relative Crop Area Harvested") + ylab("")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_jitter(data=subset(new,host=="millet"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=0.01575,slope=0.84720), color='plum',lwd=1.3) + #millet
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values=c('plum','sienna3','gold2'), name="Host")+ 
  scale_color_manual(values=c('plum','sienna3','gold3'), name="Host")

#Maize Crop Area Harvested vs Emergence
c.z <-ggplot(new, aes(x=Crop.Harvest, y=emergence, fill=host, col=host)) + 
  xlab("") + ylab("")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_jitter(data=subset(new,host=="maize"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=0.38379,slope=0.25254), color='gold2',lwd=1.3) + #maize
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values=c('gold2','plum','sienna3'), name="Host")+ 
  scale_color_manual(values=c('gold3','plum','sienna3'), name="Host")

#All Crop Area Harvested vs Emergence
c.all <- ggplot(new, aes(x=Crop.Harvest, y=emergence, fill=host, col=host)) + 
  geom_point(alpha=0.5, pch=21, size=2) + 
  xlab("Relative Crop Area Harvested") + ylab("Mean Relative Emergence") + 
  ylim(c(0,1)) + xlim(0,1) +
  geom_abline(aes(intercept=0.01575,slope=0.84720), color='plum',lwd=1.3) + #millet
  geom_abline(aes(intercept=0.41061,slope=0.25071), color='sienna3',lwd=1.3) + #sorghum
  geom_abline(aes(intercept=0.38379,slope=0.25254), color='gold2',lwd=1.3) + #maize
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") + 
  scale_fill_manual(values=c('gold2','plum','sienna3'), name="Host")+ 
  scale_color_manual(values=c('gold3','plum','sienna3'), name="Host")

#Figure compiled
fig.ch <-plot_grid(c.s,c.m,c.z, align="v", axis="r", labels=c('A','B', 'C'), cols=3)+
  theme(plot.background = element_rect(color = "black"))
