setwd("D:/HDF/he5/may2020")
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



View(h5ls(hdf_file))
files<-dir(pattern = ".he5")
files[1]
h5readAttributes(files[1], name = "/HDFEOS/GRIDS/ColumnAmountNO2/Data Fields/ColumnAmountNO2")

x0 = -179.875
Lon = seq(x0, 179.875, 0.25)
nlon <- length(Lon)
nlon

y0 = -89.875
Lat = seq(y0, 89.875, 0.25)
nlat <- length(Lat)
nlat
"CloudScreened"
h3<- h5read(files[1], "/HDFEOS/GRIDS/ColumnAmountNO2/Data Fields/ColumnAmountNO2CloudScreened")
h3[h3<0]<-NA
UVI1 <- raster(h3)
UVI1 <- flip(raster(t(h3), xmn=Lon[1], xmx=Lon[nlon], ymn=Lat[1], ymx=Lat[nlat]), "y")
""
for(i in 2:29){
  h1 <- h5read(files[i], "/HDFEOS/GRIDS/ColumnAmountNO2/Data Fields/ColumnAmountNO2CloudScreened")
  h1[h1<0]<-NA
  UVI <- raster(h1)
  UVI <- flip(raster(t(h1), xmn=Lon[1], xmx=Lon[nlon], ymn=Lat[1], ymx=Lat[nlat]), "y")
  UVI1<-stack(UVI,UVI1)
}

mean <- stackApply(UVI1, indices =  rep(1,nlayers(UVI1)), fun = "mean")

mean1<-mean
#mean1<- disaggregate(mean1, 3, method='bilinear')
new<-extent(60, 100, 5, 40)
mean1<-crop(mean1, new)
my.at <- seq(0, 10e+15, 0.25e+15)
mapTheme <- rasterTheme(region=jet.colors(256,alpha = 1))
plt <- levelplot(mean1, margin=FALSE,at=my.at,cuts=100 ,par.settings=mapTheme, main="NO2 Index - May 2020 - Molecules/cm^2",shrink=c(1,1), interpolate=T)
plt + layer(sp.lines(world_outline, col="black", lwd=1.2))

datamay2020<-mean1@data@values
mean(datamay2020, na.rm=TRUE)
sd(datamay2020, na.rm=TRUE)
qnorm(.975)*(sd(datamay2020, na.rm=TRUE)/sqrt(202080))
datamay2020<-data.frame("data"=datamay2020, "x"=rep("May 2020", times=length(datamay2020)))


datamay2019<-mean2@data@values
mean(datamay2019, na.rm=TRUE)
sd(datamay2019, na.rm=TRUE)
qnorm(.975)*(sd(datamay2019, na.rm=TRUE)/sqrt(202080))
datamay2019<-data.frame("data"=datamay2019, "x"=rep("May 2019", times=length(datamay2019)))
data20202019may<-rbind(datamay2019,datamay2020)
data20202019C<-data.frame(data20202019may, "group"="C")

subtract<-mean1-mean2
subtract<-subtract*10^-15
my.at <- seq(-5, +5, 0.25)
subtract[is.na(subtract[])] <- 0 
mapTheme <- rasterTheme(region=jet.colors(256,alpha = 1))
plt <- levelplot(subtract, margin=FALSE,par.settings=mapTheme,at=my.at, cuts=20,main="NO2 Index - Diffrence May(2020-2019)",shrink=c(1,1), interpolate=T)
plt + layer(sp.lines(world_outline, col="black", lwd=1.2))
