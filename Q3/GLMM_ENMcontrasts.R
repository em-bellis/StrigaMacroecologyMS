##Updated 1.21.21
##ENMs as metric of specialization
library(lme4)
library(readr)
library(lmerTest)
require(lme4)

#setwd("~/Path.To.StrigaMacroecology.Repo/")

##Dataframe with emergence from empircal studies and constructed 50km resolution ENMs
SI.dat <- read.csv("StrigaMacroecologyMS-master/DataFiles/SI.dat.1.30.20.csv")

####GLMM####
##Sorghum##
s.1.enm <-lmer(emergence ~ (1 | host.gen), data=SI.dat[SI.dat$host=="sorghum",])
##QQplot
qqnorm(resid(s.1.enm), main = "Sorghum")
qqline(resid(s.1.enm))

##ENM as fixed effect 
s.2.enm <-lmer(emergence ~ (1 | host.gen) + ENM_a_s50km, data=SI.dat[SI.dat$host=="sorghum",])
##QQplot
qqnorm(resid(s.2.enm), main = "Sorghum + ENM")
qqline(resid(s.2.enm))

anova(s.1.enm, s.2.enm, test="Chisqu") #p-value 0.003375 **

##Millet##
m.1.enm <-lmer(emergence ~ (1 | host.gen), data=SI.dat[SI.dat$host=="millet",])
##QQplot
qqnorm(resid(m.1.enm), main = "Millet")
qqline(resid(m.1.enm))

##ENM as fixed effect 
m.2.enm <-lmer(emergence ~ (1 | host.gen) + ENM_a_m50km, data=SI.dat[SI.dat$host=="millet",])
##QQplot
qqnorm(resid(m.2.enm), main = "Millet + ENM")
qqline(resid(m.2.enm))

anova(m.1.enm, m.2.enm, test="Chisqu") #p-value 2.483e-05 ***

##Maize##
z.1.enm <-lmer(emergence ~ (1 | host.gen), data=SI.dat[SI.dat$host=="maize",])
qqnorm(resid(z.1.enm), main = "Maize")
##QQplot
qqline(resid(z.1.enm))

##ENM as fixed effect 
z.2.enm <-lmer(emergence ~ (1 | host.gen) + ENM_a_z50km, data=SI.dat[SI.dat$host=="maize",])
##QQplot
qqnorm(resid(z.2.enm), main = "Maize + ENM")
qqline(resid(z.2.enm))

anova(z.1.enm, z.2.enm, test="Chisqu") #p-value 0.8167

##approximated coefficents of linear models using Satterwaithes method for ENMs
##note:: If the error code "[,5] out of bounds"appears, re-run the above models with "lmerTest" which 
#includes more columns (approximate model parameters e.g. p-values) than "lme4"

##Sorghum
#extract coefficients
coef.s <- data.frame(coef(summary(s.2.enm))) #use normal distribution to approximate p-value
coef.s$p.z <- 2 * (1 - pnorm(abs(coef.s$t.value))) #get Satterthwaite-approximated degrees of freedom
coef.s$df.Satt <- coef(summary(s.2.enm))[, 3] # get approximate p-values for model
coef.s$p.Satt <- coef(summary(s.2.enm))[, 5]
coef.s

##Millet
coef.m <- data.frame(coef(summary(m.2.enm)))
coef.m$p.z <- 2 * (1 - pnorm(abs(coef.m$t.value)))
coef.m$df.Satt <- coef(summary(m.2.enm))[, 3]
coef.m$p.Satt <- coef(summary(m.2.enm))[, 5]
coef.m

##Maize
coef.z <- data.frame(coef(summary(z.2.enm)))
coef.z$p.z <- 2 * (1 - pnorm(abs(coef.z$t.value)))
coef.z$df.Satt <- coef(summary(z.2.enm))[, 3]
coef.z$p.Satt <- coef(summary(z.2.enm))[, 5]
coef.z

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
  geom_abline(aes(intercept=0.49894,slope=0.52307), color='sienna3',lwd=1.3) + #sorghum
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
  geom_jitter(data=subset(new,host=="millet"), alpha=0.55, pch=21, size=2, width = 0.1, height = 0.1) +
  geom_abline(aes(intercept=0.34115,slope=0.70247), color='plum',lwd=1.3) + #millet 
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
  geom_abline(aes(intercept=0.44049,slope=-0.02442), color='gold2',lwd=1.3) + #maize
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
  geom_abline(aes(intercept=0.34115,slope=0.70247), color='plum',lwd=1.3) + #millet 
  geom_abline(aes(intercept=0.49894,slope=0.52307), color='sienna3',lwd=1.3) + #sorghum
  geom_abline(aes(intercept=0.44049,slope=-0.02442), color='gold2',lwd=1.3) + #maize
  scale_x_reverse() + 
  scale_fill_manual(values=c('gold2','plum','sienna3'), name="Host")+ 
  scale_color_manual(values=c('gold3','plum','sienna3'), name="Host")
#more negative values correspons to higher specialization 

#Figure compiled
fig.enm <-plot_grid(e.s,e.m,e.z, align="v", axis="r", labels=c('G','H','I'), cols=3)+
  theme(plot.background = element_rect(color = "black"))
