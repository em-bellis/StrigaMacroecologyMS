#updated 3.9.20
#this script is to determine how much habitat suitability changes for future predictions

library(raster)
library(viridis)
library(maps)
library(pals)

all <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/all.CLY250.tif')
maiz <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/maiz.CLY250.tif')
sorg <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/sorg.CLY250.tif')
mill <- raster('/Users/ebellis/Desktop/Projects/StigaxSorghum/MacroecologyMS/Fig1/mill.CLY250.tif')

mods <- c('all','maiz','mill','sorg')
rcps <- c('rcp45.','rcp85.')
rcps.path <- paste('/Users/ebellis/Desktop/Projects/StigaxSorghum/ENMs/future/',c("rcp45","rcp85"),'/', sep="")

for (k in 1:2) {
	for (j in 1:length(mods)) {
 		fut <- paste(rcps.path[k],list.files(rcps.path[k], pattern=mods[j]),sep="")
 
 		acc <- raster(fut[1])
 		ces <- raster(fut[2])
 		cmc <- raster(fut[3])
 		mir <- raster(fut[4])
 		mpi <- raster(fut[5])
 
 		fut.mean <- mean(stack(acc,ces,cmc,mir,mpi))
 		diff <- fut.mean - mill
 	
 	#mask based on minimum habitat suitability in the future
 		core <- calc(fut.mean, fun=function(x){ x[x < 0.2] <- NA; return(x)} )
 		diff.core <- mask(diff, core)
 	
 		out.file <- paste(rcps.path[k],rcps[k],mods[j],'.pdf', sep="")

 		pdf(out.file)
 		plot(diff.core, breaks=seq(-1,1,by=0.1),col=coolwarm(n=20), legend=T,  xaxt='n', yaxt='n')
 		map(database="world", xlim=c(-20,60),ylim=c(-40,45),axes=FALSE, add=T, col="grey50", lwd=0.5)
 		scalebar(1000, xy = c(35,-33), type = "line", divs = 1, lonlat = TRUE)
 		dev.off()
	}
}
