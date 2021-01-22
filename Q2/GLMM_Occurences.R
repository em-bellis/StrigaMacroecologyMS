##Updated 1.21.21
###Herbarium occurrence data as metric of specialization
library(lme4)
library(readr)
library(dplyr)

#setwd("~/Path.to.Striga.Macroecology.Repository/")

##Emergence Data
SI.dat <- read.csv("StrigaMacroecologyMS-master/DataFiles/SI.dat.1.30.20.csv")

##Occurence Data, this dataframe has missing data for many localities of interest, so a new dataframe must be made with no missing occurence data
##Within this dataframe "host.p" for a given host is the proportion of occurences at a given locaiton for that specific hosts 
occ <- read.csv("StrigaMacroecologyMS-master/DataFiles/OccurrenceData.5.11.20.csv")

##Combining all Striga provenaces that overlap with Striga herbarium occurencesand reducing the data to only locaitions that have occurence data 
occ.all <- left_join(SI.dat, occ, by="locality")
occ.all <- na.omit(occ.all)

####GLMM####
##Sorghum##
s.1.occ <-lmer(emergence ~ (1 | host.gen), data=occ.all[occ.all$host=="sorghum",])
##QQplot
qqnorm(resid(s.1.occ), main = "Sorghum")
qqline(resid(s.1.occ))

##Herbarium occurences as fixed effect 
s.2.occ <-lmer(emergence ~ (1 | host.gen) + sorg.p , data=occ.all[occ.all$host=="sorghum",])
##QQplot
qqnorm(resid(s.2.occ), main = "Sorghum + HerbariumOccurrence")
qqline(resid(s.2.occ))

anova(s.1.occ, s.2.occ, test="Chisqu") #P-value  0.8794

##Millet##
m.1.occ <-lmer(emergence ~ (1 | host.gen), data=occ.all[occ.all$host=="millet",])
##QQplot
qqnorm(resid(m.1.occ), main = "Millet")
qqline(resid(m.1.occ))

##Herbarium occurences as fixed effect 
m.2.occ <-lmer(emergence ~ (1 | host.gen) + mil.p , data=occ.all[occ.all$host=="millet",])
##QQplot
qqnorm(resid(m.2.occ), main = "Millet + Herbarium Occurrence")
qqline(resid(m.2.occ))

anova(m.1.occ, m.2.occ, test="Chisqu") #P-value 0.07091 .

##Maize##
z.1.occ <-lmer(emergence ~ (1 | host.gen), data=occ.all[occ.all$host=="maize",])
##QQplot
qqnorm(resid(z.1.occ), main = "Maize")
qqline(resid(z.1.occ))

##Herbarium occurences as fixed effect 
z.2.occ <-lmer(emergence ~ (1 | host.gen) + maiz.p , data=occ.all[occ.all$host=="maize",])
##QQplot
qqnorm(resid(z.2.occ), main = "Maize + Herbarium Occurrence")
qqline(resid(z.2.occ))

anova(z.1.occ, z.2.occ, test="Chisqu") #P-value  0.006173 **

####Occurrence Panes of the Specialization figure####
library(cowplot)
library(ggplot2)

Specificity.Dat <- left_join(SI.dat, occ, by="locality")

#Subset data and create new object based on column query
Specificity.sorg <- Specificity.Dat[Specificity.Dat$host=="sorghum",]
Specificity.sorg <- Specificity.sorg[c(-1,-9,-10,-12,-13,-14,-15,-16,-17,-19, -21,-22)]
colnames(Specificity.sorg) <- c("locality", "emergence","host", "host.gen", "lon", "lat", "ENM", "Study", "Occurrence")

Specificity.mill <- Specificity.Dat[Specificity.Dat$host=="millet",]
Specificity.mill <- Specificity.mill[c(-1,-8,-10,-12,-13,-14,-15,-16,-18,-19,-20,-22)]
colnames(Specificity.mill) <- c("locality", "emergence","host", "host.gen", "lon", "lat", "ENM", "Study", "Occurrence")

Specificity.maize <- Specificity.Dat[Specificity.Dat$host=="maize",]
Specificity.maize <- Specificity.maize[c(-1,-8,-9,-12,-13,-14,-15,-16,-17,-18,-20,-21)]
colnames(Specificity.maize) <- c("locality", "emergence","host", "host.gen", "lon", "lat", "ENM", "Study", "Occurrence")

new <-rbind(Specificity.sorg,Specificity.mill,Specificity.maize)

#Sorghum Occurrence vs Emergence
o.s <-ggplot(new, aes(x=Occurrence, y=emergence, fill=host, col=host)) + 
  xlab("") + ylab("Mean Relative Emergence")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_jitter(data=subset(new,host=="sorghum"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=0.55006,slope=0.00555), color='sienna3',lwd=1.3) + #sorghum
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values=c('sienna3','gold2','plum'), name="Host")+ 
  scale_color_manual(values=c('sienna3','gold3','plum'), name="Host")

#Millet Occurrence vs Emergence
o.m <-ggplot(new, aes(x=Occurrence, y=emergence, fill=host, col=host)) + 
  xlab("Herbarium Occurrences") + ylab("")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_jitter(data=subset(new,host=="millet"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=0.20044,slope=0.21885), color='plum',lwd=1.3) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values=c('plum','sienna3','gold2'), name="Host")+ 
  scale_color_manual(values=c('plum','sienna3','gold3'), name="Host")

#Maize Occurrence vs Emergence
o.z <-ggplot(new, aes(x=Occurrence, y=emergence, fill=host, col=host)) + 
  xlab("") + ylab("")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_jitter(data=subset(new,host=="maize"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=0.40074,slope=0.26711), color='gold2',lwd=1.3) + #maize
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values=c('gold2','plum','sienna3'), name="Host")+ 
  scale_color_manual(values=c('gold3','plum','sienna3'), name="Host")

#All Occurrence vs Emergence
o.all <- ggplot(new, aes(x=Occurrence, y=emergence, fill=host, col=host)) + 
  geom_point(alpha=0.55, pch=21, size=2) + 
  xlab("Herbarium Occurrences") + ylab("Mean Relative Emergence")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_abline(aes(intercept=0.20044,slope=0.21885), color='plum',lwd=1.3) + #millet
  geom_abline(aes(intercept=0.55006,slope=0.00555), color='sienna3',lwd=1.3) + #sorghum
  geom_abline(aes(intercept=0.40074,slope=0.26711), color='gold2',lwd=1.3) + #maize
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") + 
  scale_fill_manual(values=c('gold2','plum','sienna3'), name="Host")+ 
  scale_color_manual(values=c('gold3','plum','sienna3'), name="Host")

#Figure compiled
fig.occ <-plot_grid(o.s,o.m,o.z, align="v", axis="r", labels=c('D','E','F'), cols=3)+
  theme(plot.background = element_rect(color = "black"))


