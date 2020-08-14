## This script is used to recreate Figure 4A-D and the composite Fig. 4
## please contact ebellis@astate.edu with any questions!

library(raster)
library(viridis)
library(maps)
library(cowplot)

############## load ENM output
maiz <- raster('maiz.CLY250.tif')
sorg <- raster('sorg.CLY250.tif')
mill <- raster('mill.CLY250.tif')

############## load occurrence records
shgeo <- read.csv('/Users/emilywork/Downloads/pnas.1908707117.sd03.csv', header=T) # supplement from https://doi.org/10.1073/pnas.1908707117 

sh.mill <- subset(shgeo, millet==1)
sh.sorg <- subset(shgeo, sorghum==1)
sh.maiz <- subset(shgeo, maize==1)

############ determine a cutoff for the 'core' habitat; hs >0.5 should be fine since most observations found in locations w/hs >0.5
mill.hs <- raster::extract(mill, cbind(sh.mill$lon,sh.mill$lat))
hist(mill.hs)
length(mill.hs[mill.hs> 0.5])/length(mill.hs) ##82%

sorg.hs <- raster::extract(sorg, cbind(sh.sorg$lon,sh.sorg$lat))
hist(sorg.hs)
length(sorg.hs[sorg.hs> 0.5])/length(sorg.hs) ##79%

maiz.hs <- raster::extract(maiz, cbind(sh.maiz$lon,sh.maiz$lat))
hist(maiz.hs)
length(maiz.hs[maiz.hs> 0.5])/length(maiz.hs) ##76%

######## visualize niche breadth
##from other MS
library(dismo)
library(raster)
library(gdalUtils)
library(rgdal)
library(ggplot2)

#core habitat
core.mill <- calc(mill, fun=function(x){ x[x < 0.5] <- NA; return(x)} )
core.sorg <- calc(sorg, fun=function(x){ x[x < 0.5] <- NA; return(x)} )
core.maiz <- calc(maiz, fun=function(x){ x[x < 0.5] <- NA; return(x)} )

#get data for each environment, mask based on HSS
env <- raster("env7.rs.tif") #clay layer from SoilGrids
env <- raster("env8.tif") #nitrogen layer from AfSoilGrids
env <- raster("CHELSA_bio10_08.tif") # mean temp of wettest quarter from CHELSA
env <- raster("CHELSA_bio10_12.tif") # annual rainfall from CHELSA

# mask to striga-prone area
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

### cowplot  
#bio12
A <- ggplot()+ geom_density(data=env.core.df,aes(x=env), fill="plum",col="plum", size=0.5, alpha=0.3) + geom_density(data=env.core.sorg.df,aes(x=env), fill="sienna3",col="sienna3", size=0.5, alpha=0.3)+ geom_density(data=env.core.maiz.df,aes(x=env), fill="gold3", col="gold3", size=0.5, alpha=0.3)+ theme_classic() + xlab("bio12") + ylab("Grid Cells") +scale_x_continuous(limits=c(0,2000),breaks=c(0,1000,2000))+ theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
##bio8
B <- ggplot()+ geom_density(data=env.core.df,aes(x=env/10), fill="plum",col="plum", size=0.5, alpha=0.3) + geom_density(data=env.core.sorg.df,aes(x=env/10), fill="sienna3",col="sienna3", size=0.5, alpha=0.3)+ geom_density(data=env.core.maiz.df,aes(x=env/10), fill="gold3", col="gold3", size=0.5, alpha=0.3)+ theme_classic() + xlab("bio8") + ylab("") +xlim(c(15,35))+ theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
#soil N
D <- ggplot()+ geom_density(data=env.core.df,aes(x=env), fill="plum",col="plum", size=0.5, alpha=0.3) + geom_density(data=env.core.sorg.df,aes(x=env), fill="sienna3",col="sienna3", size=0.5, alpha=0.3)+ geom_density(data=env.core.maiz.df,aes(x=env), fill="gold3", col="gold3", size=0.5, alpha=0.3)+ theme_classic() + xlab("soil N") + ylab("") +scale_x_continuous(limits=c(0,2000),breaks=c(0,1000,2000))+ theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
##clay; place in last spot; has legend
C <- ggplot()+ geom_density(data=env.core.df,aes(x=env, fill="millet",col="millet"), size=0.5, alpha=0.3) + geom_density(data=env.core.sorg.df,aes(x=env, fill="sorghum",col="sorghum"), size=0.5, alpha=0.3)+ geom_density(data=env.core.maiz.df,aes(x=env, fill="maize", col="maize"), size=0.5, alpha=0.3)+ theme_classic() + xlab("clay") + ylab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+ scale_fill_manual(name="Host", values=c("maize"="gold3","millet"="plum","sorghum"="sienna3"), labels=c("maize","millet","sorghum") )+ scale_color_manual(name="Host", values=c("maize"="gold3","millet"="plum","sorghum"="sienna3"), labels=c("maize","millet","sorghum")) + theme(legend.position = c(0.78,0.6))

# here p and q should already have been created by running 'Fig4EF_Productivity.R'
bottom_row <- plot_grid(q, p, labels = c('E', 'F'), label_size = 12, rel_widths = c(1, 0.8))
p4 <- plot_grid(A,B,D,C, cols=4, rel_widths = c(0.85,0.85,0.85,1), labels = c('A','B','C','D'), label_size = 12)

pdf(file="NicheFig.pdf", height=4, width=6.81)
plot_grid(p4, bottom_row, ncol = 1)
dev.off()

