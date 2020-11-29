setwd("D:/HDF/he5so2/may2020")
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
h5readAttributes(files[1], name = "/HDFEOS/GRIDS/OMI Total Column Amount SO2/Data Fields/ColumnAmountSO2")

x0 = -179.875
Lon = seq(x0, 179.875, 0.25)
nlon <- length(Lon)
nlon

y0 = -89.875
Lat = seq(y0, 89.875, 0.25)
nlat <- length(Lat)
nlat
h3<- h5read(files[1], "/HDFEOS/GRIDS/OMI Total Column Amount SO2/Data Fields/ColumnAmountSO2")
h3[h3<0]<-NA
UVI1 <- raster(h3)
UVI1 <- flip(raster(t(h3), xmn=Lon[1], xmx=Lon[nlon], ymn=Lat[1], ymx=Lat[nlat]), "y")

for(i in 2:length(files)){
  h1 <- h5read(files[i], "/HDFEOS/GRIDS/OMI Total Column Amount SO2/Data Fields/ColumnAmountSO2")
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
my.at <- seq(0, 0.8, 0.01)
newcol<-colorRampPalette(c("#FEFFED","#FBE6D6" , "green","cyan", "blue", "#FE01B1","#fc0000"))

mapTheme <- rasterTheme(region=newcol(256))
plt <- levelplot(mean1, margin=FALSE, cuts=100, par.settings=mapTheme, at=my.at,main="SO2 Index - June 2020 DU",shrink=c(1,1), interpolate=T)
plt + layer(sp.lines(world_outline, col="black", lwd=1))

subtract<-mean1-mean2
subtract[is.na(subtract[])]<-0
subtract<- disaggregate(subtract, 5, method='bilinear')
my.at <- seq(-1, +1, 0.1)
newcol<-colorRampPalette(c("#FEFFED","#FBE6D6" , "green","cyan", "blue", "#FE01B1","#fc0000"))
par(mfrow=c(1,3))
mapTheme <- rasterTheme(region=jet.colors(256,alpha = 1))
plt <- levelplot(subtract, margin=FALSE,cuts=100,at=my.at,par.settings=mapTheme,main="SO2 Index - Diffrence May(2020-2019)",shrink=c(1,1), interpolate=T, cex=2)
plt + layer(sp.lines(world_outline, col="black", lwd=1.2))

mean1<-mean
#mean1<- disaggregate(mean1, 3, method='bilinear')
new<-extent(60, 100, 5, 40)
mean1<-crop(mean1, new)
dataapr2020<-mean1@data@values
mean(dataapr2020, na.rm=TRUE)
sd(dataapr2020, na.rm=TRUE)
qnorm(.975)*(sd(dataapr2020, na.rm=TRUE)/sqrt(length(dataapr2020)))
dataapr2020<-data.frame("data"=dataapr2020, "x"=rep("April 2020", times=length(dataapr2020)))

mean2<-mean3
#mean1<- disaggregate(mean1, 3, method='bilinear')
new<-extent(60, 100, 5, 40)
mean2<-crop(mean2, new)
dataapr2019<-mean2@data@values
mean(dataapr2019, na.rm=TRUE)
sd(dataapr2019, na.rm=TRUE)
qnorm(.975)*(sd(dataapr2019, na.rm=TRUE)/sqrt(length(dataapr2020)))
