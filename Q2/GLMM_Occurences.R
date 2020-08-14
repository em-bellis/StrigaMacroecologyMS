##Updated 8.13.20
###Herbarium occurence data as metric of specialization
library(lme4)
library(readr)
library(dplyr)

#setwd("~/path.to.file.download/")

##Emergence Data
SI.dat <- read.csv("StrigaMacroecologyMS-master/DataFiles/SI.dat.1.30.20.csv")
##Occurence Data 
occ <- read.csv("StrigaMacroecologyMS-master/DataFiles/OccurrenceData.5.11.20.csv")

##Combining all Striga provenaces that overlap with Striga herbarium occurences 
occ.all <- left_join(SI.dat, occ, by="locality")
occ.all <- na.omit(occ.all)

##Sorghum
s.1.occ <-lmer(emergence~ (1 | host.gen), data=occ.all[occ.all$host=="sorghum",])
s.2.occ <-lmer(emergence ~ (1 | host.gen) + sorg.p , data=occ.all[occ.all$host=="sorghum",])

anova(s.1.occ, s.2.occ, test="Chisqu")

#plot(occ.all$emergence, occ.all$sorg.p)

##Millet
m.1.occ <-lmer(emergence~ (1 | host.gen), data=occ.all[occ.all$host=="millet",])
m.2.occ <-lmer(emergence ~ (1 | host.gen) + mil.p , data=occ.all[occ.all$host=="millet",])

anova(m.1.occ, m.2.occ, test="Chisqu")

##Maize
z.1.occ <-lmer(emergence ~ (1 | host.gen), data=occ.all[occ.all$host=="maize",])
z.2.occ <-lmer(emergence ~ (1 | host.gen) + maiz.p , data=occ.all[occ.all$host=="maize",])

anova(z.1.occ, z.2.occ, test="Chisqu")



