## used to generate dataframe of standard deviation of habitat suitability within radius of X km

library(rgdal)
library(dplyr)
library(raster)
library(dismo)
source("radius_function.R")

###load/clean data points from empirical studies
SI <- read.csv('SI.dat.1.30.20.csv', header=T)
SI <- subset(SI, emergence != "NA") %>% dplyr::select(c('locality','lon','lat','emergence','host','host.gen'))

###load raster layers 
all <- raster('/Users/emilybellis/Desktop/striga_specificity/sorg_mill_map/all.CLY250.tif')
maiz <- raster('/Users/emilybellis/Desktop/striga_specificity/sorg_mill_map/maiz.CLY250.tif')
mill <- raster('/Users/emilybellis/Desktop/striga_specificity/sorg_mill_map/mill.CLY250.tif')
sorg <- raster('/Users/emilybellis/Desktop/striga_specificity/sorg_mill_map/sorg.CLY250.tif')

### subtract layers
avz <- all - maiz
avm <- all - mill
avs <- all - sorg

### loop over radius of 10 to 200 km and create dataframe
#radii <- seq(10000, 200000, 40000)
radii <- 50000
df <- NULL

for (i in 1:length(radii)) {
	df.new <- cbind(SI, radius=0, ENM_avz = 0, ENM_avm = 0, ENM_avs = 0)
	
	for (j in 1:length(unique(SI$locality))) {
		df.new[df.new$locality==unique(SI$locality)[j],]$radius <- radii[i]
		pt <- cbind(df.new[df.new$locality==unique(SI$locality)[j],]$lon[1],df.new[df.new$locality==unique(SI$locality)[j],]$lat[1])
		df.new[df.new$locality==unique(SI$locality)[j],]$ENM_avs <- radius_stats(avs, radii[i],pt)$sd
		df.new[df.new$locality==unique(SI$locality)[j],]$ENM_avz <- radius_stats(avz, radii[i],pt)$sd
		df.new[df.new$locality==unique(SI$locality)[j],]$ENM_avm <- radius_stats(avm, radii[i],pt)$sd
	}
		print(df.new)
		df <- rbind.data.frame(df, df.new)
}