## example script for building ENMs with ENMeval 
## contact ebellis@astate.edu with questions 

options(java.parameters = "-Xmx100G")
library(rgdal)
library(dismo)
library(ENMeval)
library(Hmisc)
library(sp)

############### load environmental variables
e <- extent(-18.16667, 61.99167, -37.425, 37.4)
# climate 
env1 <- raster('~/scratch/data/env/CHELSA/CHELSA_bio10_12.tif', header=T)
env1 <- crop(env1, e)
env2 <- raster('~/scratch/data/env/CHELSA/CHELSA_bio10_08.tif', header=T)
env2 <- crop(env2, e)
env3 <- raster('~/scratch/data/env/CHELSA/CHELSA_bio10_03.tif', header=T)
env3 <- crop(env3, e)
env4 <- raster('~/scratch/data/env/ENVIREM/current_30arcsec_topoWet.tif', header=T)
env4 <- crop(env4, e)
env5 <- raster('~/scratch/data/env/ENVIREM/current_30arcsec_annualPET.tif', header=T)
env5 <- crop(env5, e)

# soil
env7 <- raster('~/scratch/data/env/SoilGrids250m/CLYPPT_M_sl2_250m_ll.tif', header=T)
env7 <- crop(env7, e)
env7.rs <- resample(env7, env3, method='bilinear')

env8 <- raster('~/scratch/data/env/AfSoilGrids/af250m_nutrient_n_m_agg30cm.tif', header=T)
env8 <- crop(env8, e)
env8.rs <- resample(env8, env3, method='bilinear')

env6 <- raster('~/scratch/data/env/AfSoilGrids/af250m_nutrient_p_t_m_agg30cm.tif', header=T)
env6 <- crop(env6, e)
env6.rs <- resample(env6, env3, method='bilinear')

env <- stack(env1, env2, env3, env4, env5, env6.rs, env7.rs, env8.rs)

################# generate background points; following https://rspatial.org/raster/sdm/3_sdm_absence-background.html  
shgeo <- read.csv('/Users/emilywork/Downloads/pnas.1908707117.sd03.csv', header=T) # supplement from https://doi.org/10.1073/pnas.1908707117 
coordinates(shgeo) <- ~lon + lat
x <- circles(shgeo, d=500000, lonlat=TRUE)
pol <- polygons(x)
samp1 <- spsample(pol, 10000, type='random', iter=25)
cells <- cellFromXY(env1, samp1)
xy <- xyFromCell(env1, cells)

write.table(xy, file='bg.10k.500km.txt', sep="\t", row.names=F, quote=F)


############### check correlation among variables 
bg <- read.table('bg.10k.500km.txt', header=T) 
bioclims <- extract(env, bg)
rcorr(as.matrix(bioclims))$r

############### subset occurrence records; just showing code for sorghum here but can be edited for other hosts 
shgeo <- read.csv('/Users/emilywork/Downloads/pnas.1908707117.sd03.csv', header=T) # supplement from https://doi.org/10.1073/pnas.1908707117 
shgeo <- subset(shgeo, sorghum==1) #269
coordinates(shgeo) <- ~lon + lat
r <- raster(shgeo)
res(r) <- 0.01
r <- extend(r, extent(r)+1)
acsel <- gridSample(shgeo, r, n=1) 
bob.sorg <- ENMevaluate(occ=acsel, env=env, method='checkerboard2', parallel=F, bg.coords=bg, algorithm='maxent.jar')

############### evaluate model; code adapted from https://cran.r-project.org/web/packages/ENMeval/vignettes/ENMeval-vignette.html  
bob.sorg@results[which(bob.sorg@results$delta.AICc==0),]
aic.opt <- bob.56@models[[11]] ## choose model index based on optimal AICc

############### project logistic output 
predict(aic.opt, env, filename = "sorg.tif", format="GTiff", outputformat="logistic", overwrite=TRUE, progress='text')
