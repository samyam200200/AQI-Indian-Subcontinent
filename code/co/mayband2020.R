setwd("D:/HDF/hdfco")
library(gdalUtils)
library(rgdal)
library("rhdf5")
library(raster)
library(sp)
library(raster)
library(sf) ##for reading shp file
library(raster) ##creating rasters
library(rasterVis) ## To visiualize the rasters
library(sommer) ## for theme

shp_path <- "D:/HDF/shp"
shp_name <- "World_Countries.shp"
shp_file <- paste(shp_path, shp_name, sep="/")
world_shp <- read_sf(shp_file)
world_outline <- as(st_geometry(world_shp), Class="Spatial")


sds <- get_subdatasets("AIRS.2020.05.01.L3.RetStd_IR031.v6.0.31.1.G20159215652.hdf")
View(sds)
gdal_translate(sds[150], dst_dataset = "1502020May.tif")
gdal_translate(sds[328], dst_dataset = "3282020May.tif")
rast <- raster("1502020May.tif", band=6)
rast1 <- raster("3282020May.tif", band=6)
stacked<-raster::stack(rast,rast1)
mean <- stackApply(stacked, indices =  rep(1,nlayers(stacked)), fun = "mean")

mean@data@values<-mean@data@values*28.9644 /28.0101*10^9

new<-extent(60, 100, 5, 40)
mean<-crop(mean, new)

my.at <- seq(0, 300, 4)
mapTheme <- rasterTheme(region=jet.colors(256,alpha = 1)) ## raster theme that will help provide the best plot (the theme is from sommer package)
plt <- levelplot(mean, #dataset
                 margin=FALSE, #provides well deliniated margins
                 #cuts for the legend
                 at=my.at,
                 par.settings=mapTheme, #mapTheme provided previously
                 main="NO2 Index - March 2nd 2019 - Molecules/cm^2", #providing the title
                 interpolate=T) #minimal interpolation if there are missing values
plt + layer(sp.lines(world_outline, col="black", lwd=1.0))

