##Updated 7.1.20
##ENMs as metric of specialization

library(lme4)
library(readr)

setwd("~/Desktop/Research/Lasky/StrigaMacroecology/ModelWD/")

##Making data table only including locaitons with comparative host species
New.Stand <- read.csv("New.Stand.1.11.20.csv")
ENM.all <- read.csv("New.Stand.1.6.20.csv")

##Relative emergence of empirical studies
Emerg.dat <- data.frame(emergence=New.Stand$emergence, host=New.Stand$host,
                        host.gen=New.Stand$host.gen, locality=New.Stand$locality)

##ENMs with 50km resolution
SMZ.dat <- data.frame(locality= ENM.all$locality,lat= ENM.all$lat, lon= ENM.all$lon, ENM_a_s50km=ENM.all$ENM_a_s50km, 
                      ENM_a_m50km=ENM.all$ENM_a_m50km, ENM_a_z50km=ENM.all$ENM_a_z50km)

SI.dat <- merge(Emerg.dat, SMZ.dat, by="locality", all=TRUE)

##Sorghum model
s.1.enm <-lmer(emergence ~ (1 | host.gen), data=SI.dat[SI.dat$host=="sorghum",])
s.2.enm <-lmer(emergence ~ (1 | host.gen) + ENM_a_s50km, data=SI.dat[SI.dat$host=="sorghum",])

anova(s.1.enm, s.2.enm, test="Chisqu")

##Millet model
m.1.enm <-lmer(emergence ~ (1 | host.gen), data=SI.dat[SI.dat$host=="millet",])
m.2.enm <-lmer(emergence ~ (1 | host.gen) + ENM_a_m50km, data=SI.dat[SI.dat$host=="millet",])
anova(m.1.enm, m.2.enm, test="Chisqu")

##Maize model
z.1.enm <-lmer(emergence ~ (1 | host.gen), data=SI.dat[SI.dat$host=="maize",])
z.2.enm <-lmer(emergence ~ (1 | host.gen) + ENM_a_z50km, data=SI.dat[SI.dat$host=="maize",])

anova(z.1.enm, z.2.enm, test="Chisqu")

###Looking at other host ENMs to determine if a signifciant predictors for maize emergence###
##Sorghum ENM for maize emergence 
z.2.Senm <- lmer(emergence ~ (1 | host.gen) + ENM_a_s50km, data=SI.dat[SI.dat$host=="maize",])

anova(z.1, z.2.Senm, test=("Chisq"))

##Millet ENM for maize emergence 
z.2.Menm <- lmer(emergence ~ (1 | host.gen) + ENM_a_m50km, data=SI.dat[SI.dat$host=="maize",])
anova(z.1, z.2.Menm, test=("Chisq"))

###################################
##ENM Standard Deviation
##new dataframe with Standard Deviations of ENM for locations, already has all other data columns
SD <- read_csv("SI.dat.4.2.20_StDev.csv")

##Sorghum
##Fixed effect of SD
s.ENM.SD.f <-lmer(emergence ~ (1 | host.gen) + ENM_a_s50km + ENM_avs_sd, data=SD[SD$host=="sorghum",])
##same models as previous section, had to be recreated because with a new dataset
s.ENM <-lmer(emergence ~ (1 | host.gen) + ENM_a_s50km,  data=SD[SD$host=="sorghum",])
s <-lmer(emergence ~ (1 | host.gen),  data=SD[SD$host=="sorghum",])

##NO SD, confirm same output
s.anova<- anova(s, s.ENM, test=("Chisq"))
s.anova
##anova comparing emergence vs emergence with ENM and SD as fixed effects
anova.s.SD <-anova(s.ENM.SD.f, s, test=("Chisq"))
anova.s.SD

##Millet
m.ENM.SD.f <-lmer(emergence ~ (1 | host.gen) + ENM_a_m50km + ENM_avm_sd, data=SD[SD$host=="millet",])
m.ENM <-lmer(emergence ~ (1 | host.gen) + ENM_a_m50km,  data=SD[SD$host=="millet",])
m <-lmer(emergence ~ (1 | host.gen),  data=SD[SD$host=="millet",])

###NO SD 
m.anova <- anova(m, m.ENM, test=("Chisq"))
m.anova
###including SD and ENM as fixed 
anova.m.SD <-anova(m, m.ENM.SD.f, test=("Chisq"))
anova.m.SD

##Maize
z.ENM.SD.f <-lmer(emergence ~ (1 | host.gen) + ENM_a_z50km + ENM_avz_sd, data=SD[SD$host=="maize",])
z.ENM <-lmer(emergence ~ (1 | host.gen) + ENM_a_z50km,  data=SD[SD$host=="maize",])
z <-lmer(emergence ~ (1 | host.gen),  data=SD[SD$host=="maize",])

##NO SD
z.anova <-anova(z, z.ENM, test=("Chisq"))
z.anova

##Including ENM and SD
anova.z.SD <-anova(z, z.ENM.SD.f, test=("Chisq"))
anova.z.SD

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

##########################################
##approximated coefficents of linear models using Satterwaithes method for crop harvest
##note:: If the error code "[,5] out of bounds"appears, re-run models with "lmerTest" which 
#includes approximates more model parameters (including p-values) than "lme4"

require(lmerTest)

##Sorghum
#extract coefficients
coef.s <- data.frame(coef(summary(s.2.enm)))
#use normal distribution to approximate p-value
coef.s$p.z <- 2 * (1 - pnorm(abs(coef.s$t.value)))
#get Satterthwaite-approximated degrees of freedom
coef.s$df.Satt <- coef(summary(s.2.enm))[, 3]
# get approximate p-values for model
coef.s$p.Satt <- coef(summary(s.2.enm))[, 5]
coef.s

##Repeat above with other host crops
##Millet
coef.m <- data.frame(coef(summary(m.2.enm)))
coef.m$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coef.m$df.Satt <- coef(summary(m.2.enm))[, 3]
coef.m$p.Satt <- coef(summary(m.2.enm))[, 5]
coef.m

##Maize
coef.z <- data.frame(coef(summary(z.2.enm)))
coef.z$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coef.z$df.Satt <- coef(summary(z.2.enm))[, 3]
coef.z$p.Satt <- coef(summary(z.2.enm))[, 5]
coef.z
