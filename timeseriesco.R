setwd("D:/HDF/hdfco")
library(tidyverse)
library(gdalUtils)
library(rgdal)
library("rhdf5")
library(raster)
library(sp)
library(raster)
library(sf) ##for reading shp file
library(lubridate)
files<-dir(pattern = ".tif")
new<-extent(60, 100, 5, 40)
rast <- readAll(raster("1502020Apr.tif"))
rast1 <- raster("3282020Apr.tif", band=6)

dataCO<-NULL

for(i in 1:length(files)){
  rast <- readAll(raster(files[i], band=6))
  mont <- substr(files[i],3,10)
  rast[rast<0]<-NA
  rast<-crop(rast, new)
  average<-mean(rast@data@values, na.rm = T)
  sd<-sd(rast@data@values,na.rm=T)
  n<-length(rast@data@values)
  se=sd/sqrt(n)
  ci=qt(0.975, df=n-1)*(sd/sqrt(n))
  maximum<-max(rast@data@values, na.rm = T)
  minimum<-min(rast@data@values, na.rm=T)
  first<-data.frame(average, maximum, minimum, mont, sd,n,se,ci)
  dataCO<-rbind(dataCO, first)
}
dataCO$month<-substr(dataCO$mont,2,8)
dataCO$ascdsc<-substr(dataCO$mont,1,1)
dataCO$ascdsc<-ifelse(dataCO$ascdsc==0,"ascending","descending")
dataCO[1:3]<-dataCO[1:3]*28.9644 /28.0101*10^9
dataCO[c(5,7,8)]<-dataCO[c(5,7,8)]*28.9644 /28.0101*10^9
options(digits = 5)
dataCO<-dataCO%>%group_by(month)%>%summarise(avg=round(mean(average),3), min=round(mean(minimum),3), max=round(mean(maximum),3), stdd=mean(sd), num=mean(n), error=mean(se), conf=mean(ci))
dataCO$month<-paste0(substr(dataCO$month, 5,8),"-",substr(dataCO$month, 1,4))
dataCO$month<-parse_date_time(dataCO$month, "my")
dataCO<-dataCO%>%arrange(month)

dataCO$month_s<-format(dataCO$month, "%m")
dataCO$year<-format(dataCO$month, "%y")

#showing the error using standard error
# ggplot(dataCO, aes(x=month_s, 
#                    y=avg, 
#                    group=year, 
#                    col=factor(year)))+
#   geom_point(size=3)+
#   geom_line()+
#   geom_errorbar(aes(ymin=avg-error, 
#                     ymax=avg+error), 
#                 width=0.2)

library(reshape)
newdataCO<-melt(data=as.data.frame(dataCO), id.vars=c('stdd', 'error', 'month_s','year'), measure.vars = c('avg', 'min','max'))
newdataCO$variable_f = factor(newdataCO$variable, levels=c('max','avg', 'min'))
breaks=c("March", "April", "May", "June", "July", "August")
ggplot(data = newdataCO, aes(x=month_s, y=value,color=year, group=year))+
  geom_point()+
  geom_line(lwd=1)+
  facet_grid(variable_f~., scales = "free_y")+
  theme_minimal()+
  theme(text = element_text(size=15), 
        axis.text.x = element_text(angle=45, hjust=1))+scale_color_manual(
          values = c("Red", "Blue"), 
          name="Year", labels=c("2019", "2020"))+
  scale_x_discrete(labels = breaks)+
  labs(x="Months - 2019/2020", 
       y="Mean Levels of CO (500 VMR) - 2019/2020")+
  theme(text=element_text(family="Times New Roman", size=12))


