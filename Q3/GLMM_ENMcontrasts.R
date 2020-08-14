##Updated 8.13.20
##ENMs as metric of specialization

library(lme4)
library(readr)
library(lmerTest)
#require(lmerTest)

#setwd("~/path.to.file.download/")

##Dataframe with emergence from empircal studies and constructed 50km resolution ENMs
SI.dat <- read.csv("StrigaMacroecologyMS-master/DataFiles/SI.dat.1.30.20.csv")

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

###Looking at other host ENMs to determine if signifciant predictors for maize emergence
##Sorghum ENM for maize emergence 
z.2.Senm <- lmer(emergence ~ (1 | host.gen) + ENM_a_s50km, data=SI.dat[SI.dat$host=="maize",])
anova(z.1.enm, z.2.Senm, test=("Chisq"))

##Millet ENM for maize emergence 
z.2.Menm <- lmer(emergence ~ (1 | host.gen) + ENM_a_m50km, data=SI.dat[SI.dat$host=="maize",])
anova(z.1.enm, z.2.Menm, test=("Chisq"))

##approximated coefficents of linear models using Satterwaithes method for ENMs
##note:: If the error code "[,5] out of bounds"appears, re-run the above models with "lmerTest" which 
#includes more columns (approximate model parameters e.g. p-values) than "lme4"

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

