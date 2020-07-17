##Updated 5.20.20
###Herbarium occurence data as metric of specialization
library(lme4)
library(readr)
library(dplyr)

#setwd("~/Desktop/Research/Lasky/StrigaMacroecology/ModelWD")

SI.dat <- read.csv("SI.dat.master.csv")
SI.dat$X <-NULL
##Occurence Data 
occ <- read_csv("Occurance_Data_5.11.20.csv")

occ.all <- left_join(SI.dat, occ, by="locality")
na.omit(occ.all)

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

#plot(emergence, maiz.p, data=occ.all[occ.all$host=="maize",])
#plot(ranef(s.2))
#plot(ranef(s.1))
#plot(s.1)
#plot(s.2)
