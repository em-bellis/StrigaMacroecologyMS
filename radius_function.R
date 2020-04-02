radius_stats <- function(enm, radius, location) {
	#location given as SpatialPoints object
	#enm is raster layer
	#radius is integer (number of meters)
	
	x <- circles(location, d=radius, lonlat=T)
	pol <- polygons(x)
	masked <- mask(enm, pol)
	mean.enm <- cellStats(masked, 'mean', na.rm=T)
	sd.enm <- cellStats(masked, 'sd', na.rm=T)
	my_list <- list("mean" = mean.enm, "sd" = sd.enm) 
	#my_list <- list("mean" = mean.enm)
	return(my_list)
}
