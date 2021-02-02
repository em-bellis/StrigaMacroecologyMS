##Updated 1.29.21
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

##Create "region" column 
occ.all$region <- with(occ.all,
                       ifelse(lon.x >= 15 , "EAST" , ifelse( lon.x< 0, "WEST", "CENTRAL"))
)

####LMM####
##Sorghum##
mod.s1.o <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region), data=occ.all[occ.all$host=="sorghum",])
qqnorm(resid(mod.s1.o), main = "Sorghum")
qqline(resid(mod.s1.o))

##Herbarium occurences as fixed effect 
mod.s2.o  <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region)+ sorg.p , data=occ.all[occ.all$host=="sorghum",])
qqnorm(resid(mod.s2.o), main = "Sorghum + Herbarium Occurrences")
qqline(resid(mod.s2.o))

S.occ.chi<-anova(mod.s1.o,mod.s2.o, test="Chisq") #P-value  0.5707

##Millet##
mod.m1.o <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region), data=occ.all[occ.all$host=="millet",])
qqnorm(resid(mod.m1.o), main = "Millet")
qqline(resid(mod.m1.o))

##Herbarium occurences as fixed effect 
mod.m2.o  <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region)+ mil.p , data=occ.all[occ.all$host=="millet",])
qqnorm(resid(mod.m2.o), main = "Millet + Herbarium Occurrences")
qqline(resid(mod.m2.o))

M.occ.chi<-anova(mod.m1.o,mod.m2.o, test="Chisq") #P-value 0.06438 .

##Maize##

mod.z1.o <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region), data=occ.all[occ.all$host=="maize",])
qqnorm(resid(mod.z1.o), main = "Maize")
qqline(resid(mod.z1.o))

##Herbarium occurences as fixed effect 
mod.z2.o  <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region)+ maiz.p , data=occ.all[occ.all$host=="maize",])
qqnorm(resid(mod.z2.o), main = "Maize + Herbarium Occurrences")
qqline(resid(mod.z2.o))

Z.occ.chi<-anova(mod.z1.o,mod.z2.o, test="Chisq") #P-value 0.07125 .


####Occurrence Panes of the Specialization figure####
library(cowplot)
library(ggplot2)

Specificity.Dat <- left_join(SI.dat, occ, by="locality")

# subset data and create new object based on column query
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
  geom_point(data=subset(new,host=="sorghum"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=  0.54539,slope=0.05495), color='sienna3',lwd=1.3) + #sorghum
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
  geom_point(data=subset(new,host=="millet"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=0.07054  ,slope=0.21098), color='plum',lwd=1.3) +  #millet
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
  geom_point(data=subset(new,host=="maize"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept= 0.36205,slope=0.17845 ), color='gold2',lwd=1.3) + #maize
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
  geom_jitter(width = 0.1, height = 0.1)+
  geom_abline(aes(intercept=0.07054  ,slope=0.21098), color='plum',lwd=1.3) +  #millet
  geom_abline(aes(intercept=  0.54539,slope=0.05495), color='sienna3',lwd=1.3) + #sorghum
  geom_abline(aes(intercept= 0.36205,slope=0.17845 ), color='gold2',lwd=1.3) + #maize
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") + 
  scale_fill_manual(values=c('gold2','plum','sienna3'), name="Host")+ 
  scale_color_manual(values=c('gold3','plum','sienna3'), name="Host")

#Figure compiled
fig.occ <-plot_grid(o.s,o.m,o.z, align="v", axis="r", labels=c('D','E','F'), cols=3)+
  theme(plot.background = element_rect(color = "black"))

####Moran's I for Spatial autocorrelaiton####
library(geoMap)
library("ape")

#Sorghum
Resid.sorg <- Specificity.sorg[!is.na(Specificity.sorg$Occurrence), ]
Resid.sorg <- Resid.sorg[!is.na(Resid.sorg$host.gen), ]
Resid.sorg$residuals <- residuals(mod.s2.o)

dists <- as.matrix(dist(cbind(Resid.sorg$lon, Resid.sorg$lat)))
dists.inv <- 1/dists 
diag(dists.inv) <- 0
dists.inv[is.infinite(dists.inv)] <- 0

Moran.I(Resid.sorg$residuals, dists.inv) #P-value 0.7939897

#Millet
Resid.mill <- Specificity.mill[!is.na(Specificity.mill$Occurrence), ]
Resid.mill <- Resid.mill[!is.na(Resid.mill$host.gen), ]
Resid.mill$residuals <- residuals(mod.m2.o)

dists <- as.matrix(dist(cbind(Resid.mill$lon, Resid.mill$lat)))
dists.inv <- 1/dists 
diag(dists.inv) <- 0
dists.inv[is.infinite(dists.inv)] <- 0

Moran.I(Resid.mill$residuals, dists.inv) #P-value 0.3291543

#Maize
Resid.maize <- Specificity.maize[!is.na(Specificity.maize$Occurrence), ]
Resid.maize <- Resid.maize[!is.na(Resid.maize$host.gen), ]
Resid.maize$residuals <- residuals(mod.z2.o)

dists <- as.matrix(dist(cbind(Resid.maize$lon, Resid.maize$lat)))
dists.inv <- 1/dists 
diag(dists.inv) <- 0
dists.inv[is.infinite(dists.inv)] <- 0

Moran.I(Resid.maize$residuals, dists.inv) #P-value 0.6761561
