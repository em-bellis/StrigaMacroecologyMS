##1.29.20 Exploring environmental variation among ENMs
##1.30.20 Added clay

library(raster)
library(viridis)
library(maps)

#all <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/all.CLY250.tif')
maiz <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/maiz.CLY250.tif')
sorg <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/sorg.CLY250.tif')
mill <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/mill.CLY250.tif')

shgeo <- read.csv('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/curated_occ_ESB_7.26.18.csv')

sh.mill <- subset(shgeo, millet==1)
sh.sorg <- subset(shgeo, sorghum==1)
sh.maiz <- subset(shgeo, maize==1)

##if need a cut-off, hs >0.5 should be good
mill.hs <- extract(mill, cbind(sh.mill$lon,sh.mill$lat))
hist(mill.hs)
length(mill.hs[mill.hs> 0.5])/length(mill.hs) ##82%

sorg.hs <- extract(sorg, cbind(sh.sorg$lon,sh.sorg$lat))
hist(sorg.hs)
length(sorg.hs[sorg.hs> 0.5])/length(sorg.hs) ##79%

maiz.hs <- extract(maiz, cbind(sh.maiz$lon,sh.maiz$lat))
hist(maiz.hs)
length(maiz.hs[maiz.hs> 0.5])/length(maiz.hs) ##76%

all.hs <- extract(all, cbind(shgeo$lon,shgeo$lat))
hist(all.hs)
length(all.hs[all.hs> 0.5])/length(all.hs)  ##67%


########visualize niche breadth?
##from other MS
library(dismo)
library(raster)
library(gdalUtils)
library(rgdal)
library(ggplot2)

#core habitat
core.mill <- calc(mill, fun=function(x){ x[x < 0.2] <- NA; return(x)} )
core.sorg <- calc(sorg, fun=function(x){ x[x < 0.2] <- NA; return(x)} )
core.maiz <- calc(maiz, fun=function(x){ x[x < 0.2] <- NA; return(x)} )

#get data for each environment, mask based on HSS
env <- raster("/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/env7.rs.tif")
env <- crop(env, core.mill)
env.core <- mask(env, core.mill)
env.core.sorg <- mask(env, core.sorg)
env.core.maiz <- mask(env, core.maiz)

#quantile(env.core, p=c(0.1,0.9,0.5))
##millet, env12:  441 1164  742 
##millet, env8:  248 289 266 
#quantile(env.core.sorg, p=c(0.1,0.9,0.5))
##sorghum, env12:  509 1197  861
##sorg, env8: 217 278 251 
#quantile(env.core.maiz, p=c(0.1,0.9,0.5))
##maize, env12: 854 1537 1166
##maize, env8: 204 264 243 

freq(maiz)

#####change to dataframe
env.core.df <- as.data.frame(env.core, na.rm =T)
colnames(env.core.df) <- "env"
env.core.sorg.df <- as.data.frame(env.core.sorg, na.rm =T)
colnames(env.core.sorg.df) <- "env"
env.core.maiz.df <- as.data.frame(env.core.maiz, na.rm =T)
colnames(env.core.maiz.df) <- "env"

pdf("/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig2/Fig2A.pdf", width = 1.7, height =2, pointsize=16)
ggplot()+ geom_density(data=env.core.df,aes(x=env/10), fill="purple",col="purple", size=0.5, alpha=0.3) + geom_density(data=env.core.sorg.df,aes(x=env/10), fill="green",col="darkgreen", size=0.5, alpha=0.3)+ geom_density(data=env.core.maiz.df,aes(x=env/10), fill="yellow", col="darkorange", size=0.5, alpha=0.3)+ theme_classic() + xlab("bioclim8") + ylab("Grid Cells") +xlim(c(15,35))+ theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
dev.off()

pdf("/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig2/Fig2B.pdf", width = 1.7, height =2, pointsize=16)
ggplot()+ geom_density(data=env.core.df,aes(x=env), fill="purple",col="purple", size=0.5, alpha=0.3) + geom_density(data=env.core.sorg.df,aes(x=env), fill="green",col="darkgreen", size=0.5, alpha=0.3)+ geom_density(data=env.core.maiz.df,aes(x=env), fill="yellow", col="darkorange", size=0.5, alpha=0.3)+ theme_classic() + xlab("bioclim12") + ylab("Grid Cells") +xlim(c(0,2200))+ theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
dev.off()

pdf("/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig2/Fig2C.pdf", width = 1.7, height =2, pointsize=16)
ggplot()+ geom_density(data=env.core.df,aes(x=env), fill="purple",col="purple", size=0.5, alpha=0.3) + geom_density(data=env.core.sorg.df,aes(x=env), fill="green",col="darkgreen", size=0.5, alpha=0.3)+ geom_density(data=env.core.maiz.df,aes(x=env), fill="yellow", col="darkorange", size=0.5, alpha=0.3)+ theme_classic() + xlab("soil N") + ylab("Grid Cells") +xlim(c(0,2000))+ theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
dev.off()

pdf("/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig2/Fig2D.pdf", width = 1.7, height =2, pointsize=16)
ggplot()+ geom_density(data=env.core.df,aes(x=env), fill="purple",col="purple", size=0.5, alpha=0.3) + geom_density(data=env.core.sorg.df,aes(x=env), fill="green",col="darkgreen", size=0.5, alpha=0.3)+ geom_density(data=env.core.maiz.df,aes(x=env), fill="yellow", col="darkorange", size=0.5, alpha=0.3)+ theme_classic() + xlab("clay") + ylab("Grid Cells") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
dev.off()
