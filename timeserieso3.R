setwd("D:/HDF/he5so2/all")
library(sf)
library(rhdf5)
library(raster)
library(rasterVis)
library(sommer)
library(ggplot2)
library(lubridate)

files<-dir(pattern = ".he5")

x0 = -179.875
Lon = seq(x0, 179.875, 0.25)
nlon <- length(Lon)
nlon

y0 = -89.875
Lat = seq(y0, 89.875, 0.25)
nlat <- length(Lat)
nlat
new<-extent(60, 100, 5, 40)
new2<-extent(Lon[1], Lon[nlon], Lat[1], Lat[nlat])

dataO3<-NULL

for(i in 1:length(files)) {
  files[i]
  med<- h5read(files[i], "/HDFEOS/GRIDS/OMI Total Column Amount SO2/Data Fields/ColumnAmountO3")
  med[med<=0]<-NA
  med <- raster(med)
  med<-t(med)
  med<-setExtent(med, new2)
  med <- flip(med, "y")
  med<-crop(med, new)
  average<-mean(med@data@values, na.rm = T)
  maximum<-max(med@data@values, na.rm = T)
  minimum<-min(med@data@values, na.rm=T)
  day<-substr(files[i],20,28)
  day<-gsub("m", "-", day)
  n <- 8
  day<-paste(substr(day, 1, n-1), "-", substr(day, n, nchar(day)), sep = "")
  day<-as.Date(day)
  first<-data.frame(average, maximum, minimum, day)
  dataO3<-rbind(dataO3, first)
}

dataO3$group<-ifelse(grepl("2020", dataO3$day)==1,2020,2019)

dataO3$day<-as.Date(dataO3$day)

dataO3$onlyC<-format(as.Date(dataO3$day), "%m-%d")
dataO3$id<-c(seq(1:184),seq(1:184))
breaks<-c("March 1", "March 20", "April 9", "April 29", "May 19", "June 8", "June 28","July 18", "August 7", "August 27", "August")

# ggplot(data=dataNO2)+
#   geom_line(aes(x=id,
#                 y=minimum,group=group,
#                 lty=factor(group), col="Minimum"),
#             lwd=1)+
#   scale_x_continuous(n.breaks = 10,
#                      labels = breaks)+
#   geom_line(aes(x=id,y=maximum,
#                 group=group,
#                 lty=factor(group),
#                 col="Maximum"),
#             lwd=1)+
#   geom_line(aes(x=id,
#                 y=average,
#                 group=group,
#                 lty=factor(group),
#                 col="Mean"),
#             lwd=1)+
#   scale_y_continuous(n.breaks = 10
#                      )+
#   scale_linetype_manual(labels=c("2019","2020"),
#                         values = c(2,1),
#                         name="Year")+
#   scale_color_manual(labels=c("Maximum", "Mean", "Minimum"), values = c("Red", "forestgreen", "Blue"), name="Levels of NO2")+
#   labs(x="Date & Month 2019/2020", y="Level of NO2 - Molecules/cm2", title = "Minimum, Maxinum, and Mean level of NO2 for Year 2019 and 2020")+
#   theme_minimal()+
#   theme(text = element_text(size=15),
#         axis.text.x = element_text(angle=45, hjust=1))

library(reshape)
newdataO3<-melt(data=dataO3, id.vars=c("day", "group","id", "onlyC"), measure.vars = c("maximum","average", "minimum"))

newdataO3$variable_f = factor(newdataO3$variable, levels=c('maximum','average','minimum'))
breaks<-c("March 1", "March 20", "April 9", "April 29", "May 19", "June 8", "June 28","July 18", "August 7", "August 27", "August")
ggplot()+
  geom_line(data=newdataO3, aes(x=id, 
                                 y=value,
                                 group=group, 
                                 col=factor(group)), 
            lwd=1)+
  geom_smooth(data=newdataO3[newdataO3$variable=="average",],
              aes(x=id, 
                  y=value, 
                  group=group, 
                  col=factor(group)),
              formula=y~poly(x, 1))+
  facet_grid(variable_f~., 
             scales = "free_y")+ scale_color_manual(
               values = c("Red", "Blue"), 
               name="Year")+
  labs(x="Days during 2019/2020")+
  ylab(expression(paste("Level of O"[3]," DU")))+
  scale_x_continuous(n.breaks = 10, 
                     labels = breaks)+
  theme_minimal()+
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(angle=45, hjust=1))


write.csv(newdataO3, "newdataO3.csv")
