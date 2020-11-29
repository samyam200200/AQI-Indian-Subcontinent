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


sds <- get_subdatasets("AIRS.2019.04.01.L3.RetStd_IR030.v6.0.31.1.G19121173037.hdf")
View(sds)
gdal_translate(sds[150], dst_dataset = "1502019Apr.tif")
gdal_translate(sds[328], dst_dataset = "3282019Apr.tif")
rast <- raster("1502019Apr.tif", band=6)
rast1 <- raster("3282019Apr.tif", band=6)
stacked<-raster::stack(rast,rast1)
mean2 <- stackApply(stacked, indices =  rep(1,nlayers(stacked)), fun = "mean")

mean2@data@values<-mean2@data@values*28.9644 /28.0101*10^9


new<-extent(60, 100, 5, 40)
mean2<-crop(mean2, new)

my.at <- seq(0, 300, 4)
mapTheme <- rasterTheme(region=jet.colors(256,alpha = 1)) ## raster theme that will help provide the best plot (the theme is from sommer package)
plt <- levelplot(mean2, #dataset
                 margin=FALSE, #provides well deliniated margins
                 #cuts for the legend
                 at=my.at,
                 par.settings=mapTheme, #mapTheme provided previously
                 main="NO2 Index - March 2nd 2019 - Molecules/cm^2", #providing the title
                 interpolate=T) #minimal interpolation if there are missing values
plt + layer(sp.lines(world_outline, col="black", lwd=1.0))

diffrence<-mean-mean2
diffrence<- disaggregate(diffrence, 5, method='bilinear')
my.at <- seq(-50, 50, 4)
mapTheme <- rasterTheme(region=jet.colors(256,alpha = 1)) ## raster theme that will help provide the best plot (the theme is from sommer package)
plt <- levelplot(diffrence, #dataset
                 margin=FALSE, #provides well deliniated margins
                 #cuts for the legend
                 at=my.at,
                 par.settings=mapTheme, #mapTheme provided previously
                 main="CO Index - April (2020-2019) - 500hpa", #providing the title
                 interpolate=T) #minimal interpolation if there are missing values
plt + layer(sp.lines(world_outline, col="black", lwd=1.0))

mean(mean@data@values, na.rm=TRUE)
sd(mean@data@values, na.rm = TRUE)
qnorm(.975)*(sd(mean@data@values, na.rm=TRUE)/sqrt(length(mean@data@values)))

mean(mean2@data@values, na.rm=TRUE)
sd(mean2@data@values, na.rm = TRUE)
qnorm(.975)*(sd(mean2@data@values, na.rm=TRUE)/sqrt(length(mean2@data@values)))
