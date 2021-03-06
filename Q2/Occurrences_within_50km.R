## This script is used to calculate number of occurrences from natural history collections within a 50km radius of locations from experimental studies
## please contact ebellis@astate.edu with any questions!

library(raster)
library(dplyr)
library(rgdal)
library(dismo)
library(maps)
library(tidyr)

############### load occurrence points
shgeo <- read.csv('/Users/emilywork/Downloads/pnas.1908707117.sd03.csv', header=T)
whost <- subset(shgeo, millet == 1 | sorghum == 1 | maize==1)
coordinates(whost) <- ~lon+lat
projection(whost) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84')

################  load gps points for experimental studies
meta <- read.csv('DataFiles/SI.dat.1.30.20.csv')
meta <- subset(meta, emergence != "NA") # 27 localitiesm
meta.coords <- meta %>% dplyr::select(lat,lon,locality) %>% unique()
coordinates(meta.coords) <- ~lon + lat
projection(meta.coords) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84')

###### polygons within radius of metaanalyses
x.50 <- circles(meta.coords, d=50000, lonlat=T) #sample within radius of 200km
pol.50 <- polygons(x.50)
ovr <- over(whost, pol.50)
i <- which(is.na(ovr))##in ocean
whost <- subset(shgeo, millet == 1 | sorghum == 1 | maize==1)
shgeo.50km <- subset(whost, !(row(whost)[,1] %in% i)) # 72 observations

coordinates(whost) <- ~lon+lat
projection(whost) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84')

whost.df <- subset(shgeo, millet == 1 | sorghum == 1 | maize==1)
whost.df$meta <- NA

meta.coords$mill <- NA
meta.coords$sorg <- NA
meta.coords$maiz <- NA

###### for each experimental locality, count up the number of occurrences on each host in the vicinity 
for (i in 1:length(meta.coords)){
	poi <- cbind.data.frame(meta.coords$lon[i],meta.coords$lat[i])
	poi.50 <- circles(poi, d=50000, lonlat=T)
	poi.pol <- polygons(poi.50)
	ovr <- over(whost, poi.pol)
	i.ovr <- which(is.na(ovr))
	meta.coords$mill[i] <- sum(subset(whost.df, !(row(whost)[,1] %in% i.ovr))$millet) 
	meta.coords$sorg[i] <- sum(subset(whost.df, !(row(whost)[,1] %in% i.ovr))$sorghum) 
	meta.coords$maiz[i] <- sum(subset(whost.df, !(row(whost)[,1] %in% i.ovr))$maize) 
}

write.table(as.data.frame(meta.coords), file="Occurrences_within_50km.csv", quote=F, sep=",", row.names=F)
	