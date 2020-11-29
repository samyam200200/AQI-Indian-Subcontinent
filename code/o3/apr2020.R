setwd("D:/HDF/he5so2/april2020")
library(sf)
library(rhdf5)
library(raster)
library(rasterVis)
library(sommer)

shp_path <- "D:/HDF/shp"
shp_name <- "World_Countries.shp"
shp_file <- paste(shp_path, shp_name, sep="/")
world_shp <- read_sf(shp_file)
world_outline <- as(st_geometry(world_shp), Class="Spatial")


files<-dir(pattern = ".he5")
files[1]
h5readAttributes(files[1], name = "/HDFEOS/GRIDS/OMI Total Column Amount SO2/Data Fields/ColumnAmountO3")

x0 = -179.875
Lon = seq(x0, 179.875, 0.25)
nlon <- length(Lon)
nlon

y0 = -89.875
Lat = seq(y0, 89.875, 0.25)
nlat <- length(Lat)
nlat
h3<- h5read(files[1], "/HDFEOS/GRIDS/OMI Total Column Amount SO2/Data Fields/ColumnAmountO3")
h3[-10>h3]<-NA
UVI1 <- raster(h3)
UVI1 <- flip(raster(t(h3), xmn=Lon[1], xmx=Lon[nlon], ymn=Lat[1], ymx=Lat[nlat]), "y")

for(i in 2:length(files)){
  h1 <- h5read(files[i], "/HDFEOS/GRIDS/OMI Total Column Amount SO2/Data Fields/ColumnAmountO3")
  h1[-10>h1]<-NA
  UVI <- raster(h1)
  UVI <- flip(raster(t(h1), xmn=Lon[1], xmx=Lon[nlon], ymn=Lat[1], ymx=Lat[nlat]), "y")
  UVI1<-stack(UVI,UVI1)
}


mean <- stackApply(UVI1, indices =  rep(1,nlayers(UVI1)), fun = "mean")

new<-extent(60, 100, 5, 40)
mean<-crop(mean, new)


mapTheme <- rasterTheme(region=jet.colors(256,alpha = 1)) ## raster theme that will help provide the best plot (the theme is from sommer package)
my.at<-seq(100,500,4)
plt <- levelplot(mean, #dataset
                 margin=FALSE, #provides well deliniated margins
                 #cuts for the legend
                 at=my.at,
                 par.settings=mapTheme, #mapTheme provided previously
                 main="NO2 Index - March 2nd 2019 - Molecules/cm^2", #providing the title
                 interpolate=T) #minimal interpolation if there are missing values
plt + layer(sp.lines(world_outline, col="black", lwd=1.0))
