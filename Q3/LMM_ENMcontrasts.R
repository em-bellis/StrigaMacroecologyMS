##Updated 1.29.21
##ENMs as metric of specialization
library(lme4)
library(readr)
library(lmerTest)
require(lme4)

#setwd("~/Path.to.Striga.Macroecology.Repository")

##Dataframe with emergence from empircal studies and constructed 50km resolution ENMs
SI.dat <- read.csv("StrigaMacroecologyMS-master/DataFiles/SI.dat.1.30.20.csv")

##Create "region" column 
SI.dat$region <- with(SI.dat,
                      ifelse(lon >= 15 , "EAST" , ifelse( lon< 0, "WEST", "CENTRAL"))
)

####LMM####
##Sorghum##
mod.s1.e <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region), data=SI.dat[SI.dat$host=="sorghum",])
qqnorm(resid(mod.s1.e), main = "Sorghum")
qqline(resid(mod.s1.e))

##ENM as fixed effect 
mod.s2.e<-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region)+ ENM_a_s50km, data=SI.dat[SI.dat$host=="sorghum",])
qqnorm(resid(mod.s2.e), main = "Sorghum + ENM")
qqline(resid(mod.s2.e))

S.enm.chi<-anova(mod.s1.e,mod.s2.e, test="Chisq") #P-value  0.006045 **

##Millet##
mod.m1.e <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region), data=SI.dat[SI.dat$host=="millet",])
qqnorm(resid(mod.m1.e), main = "Millet")
qqline(resid(mod.m1.e))

##ENM as fixed effect 
mod.m2.e <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region) + ENM_a_m50km, data=SI.dat[SI.dat$host=="millet",])
qqnorm(resid(mod.m2.e), main = "Millet + ENM")
qqline(resid(mod.m2.e))

M.enm.chi<-anova(mod.m1.e,mod.m2.e, test="Chisq") #P-value 4.465e-05 ***

##Maize##
mod.z1.e <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region), data=SI.dat[SI.dat$host=="maize",])
qqnorm(resid(mod.z1.e), main = "Maize")
qqline(resid(mod.z1.e))

##ENM as fixed effect 
mod.z2.e  <-lmer(emergence ~ (1 | host.gen) + (1|Study) + (1 | region)+ ENM_a_z50km, data=SI.dat[SI.dat$host=="maize",])
qqnorm(resid(mod.z2.e), main = "Maize + ENM")
qqline(resid(mod.z2.e))

Z.enm.chi<-anova(mod.z1.e,mod.z2.e, test="Chisq") #P-value  0.1976


####ENM Panes of the Specialization figure####
library(cowplot)
library(ggplot2)

occ <- read.csv("StrigaMacroecologyMS-master/DataFiles/OccurrenceData.5.11.20.csv")

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

#Sorghum ENM vs Emergence
e.s <-ggplot(new, aes(x=ENM, y=emergence, fill=host, col=host)) + 
  xlab("") + ylab("Mean Relative Emergence")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_jitter(data=subset(new,host=="sorghum"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept= 0.5223 ,slope=0.5030), color='sienna3',lwd=1.3) + #sorghum
  scale_x_reverse() + 
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values=c('sienna3','gold2','plum'), name="Host")+ 
  scale_color_manual(values=c('sienna3','gold3','plum'), name="Host")

#Millet ENM vs Emergence
e.m <-ggplot(new, aes(x=ENM, y=emergence, fill=host, col=host)) + 
  xlab("Host-Specific ENM Contrasts") + ylab("")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_point(data=subset(new,host=="millet"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=0.2632,slope=0.6834 ), color='plum',lwd=1.3) + #millet 
  scale_x_reverse() + 
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values=c('plum','sienna3','gold2'), name="Host")+ 
  scale_color_manual(values=c('plum','sienna3','gold3'), name="Host")

#Maize ENM vs Emergence
e.z <-ggplot(new, aes(x=ENM, y=emergence, fill=host, col=host)) + 
  xlab("") + ylab("")  +
  ylim(c(0,1)) + xlim(0,1) +
  geom_jitter(data=subset(new,host=="maize"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept= 0.4341,slope=-0.1542), color='gold2',lwd=1.3) + #maize
  scale_x_reverse() + 
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") +
  scale_fill_manual(values=c('gold2','plum','sienna3'), name="Host")+ 
  scale_color_manual(values=c('gold3','plum','sienna3'), name="Host")

#All ENM vs Emergence
e.all <- ggplot(new, aes(x=ENM, y=emergence, fill=host, col=host)) + 
  geom_point(alpha=0.5, pch=21, size=2) +
  xlab("Host-Specific ENM Contrasts") + ylab("Mean Relative Emergence") + 
  theme_minimal()+
  geom_jitter(width = 0.1, height = 0.1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none") + 
  ylim(c(0,1)) + 
  ##IMPORTANT THE SLOPES ARE *-1 SINCE NEGATIVE ENM CORRESPONDS TO SPECILAIZTION AND THE X_AXIS FLPPING DOES NOT ALSO FLIP THE MANUAL PLOTTED REGRESSION LINES
  geom_abline(aes(intercept= 0.5223 ,slope=0.5030), color='sienna3',lwd=1.3) + #sorghum
  geom_abline(aes(intercept=0.2632,slope=0.6834 ), color='plum',lwd=1.3) + #millet 
  geom_abline(aes(intercept= 0.4341,slope=-0.1542), color='gold2',lwd=1.3) + #maize
  scale_x_reverse() + 
  scale_fill_manual(values=c('gold2','plum','sienna3'), name="Host")+ 
  scale_color_manual(values=c('gold3','plum','sienna3'), name="Host")
#more negative values correspons to higher specialization 

#Figure compiled
fig.enm <-plot_grid(e.s,e.m,e.z, align="v", axis="r", labels=c('G','H','I'), cols=3)+
  theme(plot.background = element_rect(color = "black"))

####Moran's I for Spatial autocorrelaiton####
library(geoMap)
library("ape")

#Sorghum
Resid.sorg <- Specificity.sorg[!is.na(Specificity.sorg$host.gen), ]
Resid.sorg$residuals <- residuals(mod.s2.e)

dists <- as.matrix(dist(cbind(Resid.sorg$lon, Resid.sorg$lat)))
dists.inv <- 1/dists 
diag(dists.inv) <- 0
dists.inv[is.infinite(dists.inv)] <- 0

Moran.I(Resid.sorg$residuals, dists.inv) #P-value 0.9123421

for(i in 1:length(Resid.sorg$lat)){
  loc1 <- c(0,0)
  loc2 <- c(Resid.sorg$lat[i],Resid.sorg$lon[i])
  Resid.sorg$distance[i] <- haversine(loc1, loc2)
}

##Millet
Resid.mill <- Specificity.mill[!is.na(Specificity.mill$host.gen), ]
Resid.mill$residuals <- residuals(mod.m2.e)

dists <- as.matrix(dist(cbind(Resid.mill$lon, Resid.mill$lat)))
dists.inv <- 1/dists 
diag(dists.inv) <- 0
dists.inv[is.infinite(dists.inv)] <- 0

Moran.I(Resid.mill$residuals, dists.inv) #P-value 0.4528346

##Maize
Resid.maize <- Specificity.maize[!is.na(Specificity.maize$host.gen), ]
Resid.maize$residuals <- residuals(mod.z2.e)

dists <- as.matrix(dist(cbind(Resid.maize$lon, Resid.maize$lat)))
dists.inv <- 1/dists 
diag(dists.inv) <- 0
dists.inv[is.infinite(dists.inv)] <- 0

Moran.I(Resid.maize$residuals, dists.inv) #P-value 0.7605874
