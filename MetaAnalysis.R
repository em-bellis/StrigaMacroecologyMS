##this script is for the Meta-analysis of empirical studies and statistical analysis
##Updated 1.30.20

library(lme4)
library(readr)

setwd("~/Desktop/Research/Lasky/StrigaMacroecology/ModelWD")

New_Stand_1_6_20 <- read_csv("New.Stand.1.6.20.csv")
New_Stand_1_11_20 <- read_csv("New.Stand.1.11.20.csv")

##Making data tabke only including locaitons with comparative host species

New.Stand <- New_Stand_1_11_20
ENM.all <- New_Stand_1_6_20

Emerg.dat <- data.frame(emergence=New.Stand$emergence, host=New.Stand$host,
                        host.gen=New.Stand$host.gen, locality=New.Stand$locality)

SMZ.dat <- data.frame(locality= ENM.all$locality,lat= ENM.all$lat, lon= ENM.all$lon, ENM_a_s50km=ENM.all$ENM_a_s50km, 
                      ENM_a_m50km=ENM.all$ENM_a_m50km, ENM_a_z50km=ENM.all$ENM_a_z50km)

SI.dat <- merge(Emerg.dat, SMZ.dat, by="locality", all=TRUE)

##Sorghum model

mod.sorghum.ENM <-lmer(emergence ~ (1 | host.gen) + ENM_a_s50km, data=SI.dat[SI.dat$host=="sorghum",])
mod.sorghum <-lmer(emergence ~ (1 | host.gen), data=SI.dat[SI.dat$host=="sorghum",])

anova.sorghum <-anova(mod.sorghum.ENM, mod.sorghum, test=("Chisq"))

##Maize model

mod.maize.ENM <-lmer(emergence ~ (1 | host.gen) + ENM_a_z50km, data=SI.dat[SI.dat$host=="maize",])
mod.maize<-lmer(emergence ~ (1 | host.gen), data=SI.dat[SI.dat$host=="maize",])

anova.maize <-anova(mod.maize.ENM, mod.maize, test=("Chisq"))

##Millet model

mod.millet.ENM <-lmer(emergence ~ (1 | host.gen) + ENM_a_m50km, data=SI.dat[SI.dat$host=="millet",])
mod.millet <-lmer(emergence ~ (1 | host.gen), data=SI.dat[SI.dat$host=="millet",])

anova.millet <- anova(mod.millet.ENM, mod.millet, test=("Chisq"))

## summarize number of unique characters 
library(dplyr) 
SI.dat %>%
  summarise_each(funs(n_distinct))

##Looking at other host ENMs to determine if a signifciant predictors for maize emergence
##Sorghum ENM for maize emergence 

mod.maize.sENM <-lmer(emergence ~ (1 | host.gen) + ENM_a_s50km, data=SI.dat[SI.dat$host=="maize",])

anova.maize.s <-anova(mod.maize.sENM, mod.maize, test=("Chisq"))

##Millet ENM for maize emergence 

mod.maize.mENM <-lmer(emergence ~ (1 | host.gen) + ENM_a_z50km, data=SI.dat[SI.dat$host=="maize",])

anova.maize.m <- anova(mod.maize.mENM, mod.maize, test=("Chisq"))

##Map of the locations used from empirical studies

library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap)
plot(newmap,
     xlim = c(-20, 59),
     ylim = c(-15, 40),
     asp = 1 )
plot(newmap,
     xlim = range(SI.dat$lat),
     ylim = range(SI.dat$lon),
     asp = 1
)
points(SI.dat$lat, SI.dat$lon, col = "red", cex = .6)
