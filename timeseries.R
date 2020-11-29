setwd("D:/HDF/he5/all")
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

files<-dir(pattern = ".he5")

dataNO2<-NULL

for(i in 1:length(files)) {
files[i]
med<- h5read(files[i], "/HDFEOS/GRIDS/ColumnAmountNO2/Data Fields/ColumnAmountNO2CloudScreened")
med[med<0]<-NA
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
dataNO2<-rbind(dataNO2, first)
}

dataNO2$group<-ifelse(grepl("2020", dataNO2$day)==1,2020,2019)

dataNO2$day<-as.Date(dataNO2$day)

dataNO2$onlyC<-format(as.Date(dataNO2$day), "%m-%d")
dataNO2$id<-c(seq(1:184),seq(1:181))
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
newdataNO2<-melt(data=dataNO2, id.vars=c("day", "group","id", "onlyC"), measure.vars = c("average", "maximum", "minimum"))

newdataNO2$variable_f = factor(newdataNO2$variable, levels=c('maximum','average','minimum'))
breaks<-c("March 1", "March 20", "April 9", "April 29", "May 19", "June 8", "June 28","July 18", "August 7", "August 27", "August")
ggplot()+
  geom_line(data=newdataNO2, aes(x=id, 
                y=value,
                group=group, 
                col=factor(group)), 
            lwd=1)+
  geom_smooth(data=newdataNO2[newdataNO2$variable=="average",],
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
  ylab(expression(paste("Log Level of NO"[2],"- Molecules/cm"^2)))+
  scale_x_continuous(n.breaks = 10, 
                     labels = breaks)+
  theme_minimal()+
  theme(text = element_text(size=15), 
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_y_log10()+
  theme(text=element_text(family="Times New Roman", size=12))
expression("Log Level of NO"[2],"- Molecules/cm2")
write.csv(newdataNO2, "newdataNO2.csv")
