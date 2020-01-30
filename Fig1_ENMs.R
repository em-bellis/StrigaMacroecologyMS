#updated 1.28.20
#this script is to make a figure of the 4 ENMs of habitat suitability

library(raster)
library(viridis)
library(maps)

all <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/all.CLY250.tif')
maiz <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/maiz.CLY250.tif')
sorg <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/sorg.CLY250.tif')
mill <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/mill.CLY250.tif')

shgeo <- read.csv('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/curated_occ_ESB_7.26.18.csv')

sh.mill <- subset(shgeo, millet==1)
sh.sorg <- subset(shgeo, sorghum==1)
sh.maiz <- subset(shgeo, maize==1)

##part B, millet
pdf('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/B_mill.pdf')
#map(database="world", xlim=c(-20,60),ylim=c(-40,45))
plot(mill, col=viridis(n=100), legend=F, xaxt='n', yaxt='n')
map(database="world", xlim=c(-20,60),ylim=c(-40,45),axes=FALSE, add=T, col="grey50", lwd=0.5)
points(sh.mill$lon, sh.mill$lat, col=rgb(1, 192/255, 203/255, alpha=0.7),cex=0.6, pch=19)
points(sh.mill$lon, sh.mill$lat, col=rgb(0, 0, 0, alpha=0.7),cex=0.7, pch=21)
scalebar(1000, xy = c(35,-33), type = "line", divs = 1, lonlat = TRUE)
dev.off()

##part C, sorg
pdf('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/C_sorg.pdf')
#map(database="world", xlim=c(-20,60),ylim=c(-40,45))
plot(sorg, col=viridis(n=100), legend=F, axes=F, xaxt='n', yaxt='n')
map(database="world", xlim=c(-20,60),ylim=c(-40,45),axes=FALSE, add=T, col="grey50", lwd=0.5)
points(sh.sorg$lon, sh.sorg$lat, col=rgb(1, 192/255, 203/255, alpha=0.7),cex=0.6, pch=19)
points(sh.sorg$lon, sh.sorg$lat, col=rgb(0, 0, 0, alpha=0.7),cex=0.7, pch=21)
scalebar(1000, xy = c(35,-33), type = "line", divs = 1, lonlat = TRUE)
dev.off()

##part D, maiz
pdf('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/D_maiz.pdf')
#map(database="world", xlim=c(-20,60),ylim=c(-40,45))
plot(maiz, col=viridis(n=100), legend=F,xaxt='n', yaxt='n')
map(database="world", xlim=c(-20,60),ylim=c(-40,45),axes=FALSE, add=T, col="grey50", lwd=0.5)
points(sh.maiz$lon, sh.maiz$lat, col=rgb(1, 192/255, 203/255, alpha=0.7),cex=0.6, pch=19)
points(sh.maiz$lon, sh.maiz$lat, col=rgb(0, 0, 0, alpha=0.7),cex=0.7, pch=21)
scalebar(1000, xy = c(35,-33), type = "line", divs = 1, lonlat = TRUE)
dev.off()

##part A, all
pdf('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/A_all.pdf')
#map(database="world", xlim=c(-20,60),ylim=c(-40,45))
plot(all, col=viridis(n=100), legend=F, xaxt='n', yaxt='n')
map(database="world", xlim=c(-20,60),ylim=c(-40,45),axes=FALSE, add=T, col="grey50", lwd=0.5)
points(shgeo$lon, shgeo$lat, col=rgb(1, 192/255, 203/255, alpha=0.7),cex=0.6, pch=19)
points(shgeo$lon, shgeo$lat, col=rgb(0, 0, 0, alpha=0.7),cex=0.7, pch=21)
scalebar(1000, xy = c(35,-33), type = "line", divs = 1, lonlat = TRUE)
dev.off()