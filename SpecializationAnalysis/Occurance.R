###Herbarium occurence data as metric of specialization
library(lme4)
library(readr)
library(dplyr)

#setwd("~/Desktop/Research/Lasky/StrigaMacroecology/ModelWD")

SD <- read_csv("SI.dat.4.2.20_StDev.csv")
##Occurence Data 
occ <- read_csv("Occurance_Data_5.11.20.csv")

occ.all <- SD %>% right_join (occ)

##Sorghum
s.1 <-lmer(emergence~ (1 | host.gen), data=occ.all[occ.all$host=="sorghum",])
s.2 <-lmer(emergence ~ (1 | host.gen) + sorg.p , data=occ.all[occ.all$host=="sorghum",])

anova(s.1, s.2, test="Chisqu")

#plot(occ.all$emergence, occ.all$sorg.p)

##Millet
m.1 <-lmer(emergence~ (1 | host.gen), data=occ.all[occ.all$host=="millet",])
m.2 <-lmer(emergence ~ (1 | host.gen) + mil.p , data=occ.all[occ.all$host=="millet",])

anova(m.1, m.2, test="Chisqu")

##Maize
z.1 <-lmer(emergence ~ (1 | host.gen), data=occ.all[occ.all$host=="maize",])
z.2 <-lmer(emergence ~ (1 | host.gen) + maiz.p , data=occ.all[occ.all$host=="maize",])

anova(z.1, z.2, test="Chisqu")

#plot(emergence, maiz.p, data=occ.all[occ.all$host=="maize",])
#plot(ranef(s.2))
#plot(ranef(s.1))
#plot(s.1)
#plot(s.2)

