install.packages("RSQLite")
install.packages("XML")
install.packages("ggplot2")
install.packages(c("maps","mapproj"))
#install.packages("stalkR_0.02.zip", repos=NULL, type="source")
#install.packages("C:/Users/ermias/Documents/stalkR.zip", repos=NULL, type="source")
#install.packages("C:/Users/ermias/Documents/stalkR_0.02.zip",lib="C:/Users/ermias/Documents/R/win-library/2.13", repos=NULL, type="source")
install.packages('RgoogleMaps')


library(RgoogleMaps)
Df<-read.csv("pmap3.csv",sep=";",header=FALSE)
names(Df)<-c("Latitude", "Longitude","key")

bb <- qbbox(lat=range(Df$Latitude), lon=range(Df$Longitude))
m <- c(mean(Df$Latitude), mean(Df$Longitude))
zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="mobile",
NEWMAP=TRUE, destfile="tempmap.jpg", RETURNIMAGE=TRUE, GRAYSCALE=TRUE)
tmp <- PlotOnStaticMap(lat=Df$Latitude, lon=Df$Longitude,
cex=.7,pch=20,col="red", MyMap=Map, NEWMAP=FALSE)

pm=read.csv("pm10.txt",sep=",",header=TRUE)
pm=pm[,c(1,2,5)]
bb <- qbbox(lat=range(pm$Lat), lon=range(pm$Long))
m <- c(mean(pm$Lat), mean(pm$Long))
zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="mobile",
NEWMAP=TRUE, destfile="tempmap.jpg", RETURNIMAGE=TRUE, GRAYSCALE=TRUE)
tmp <- PlotOnStaticMap(lat=pm$Lat, lon=pm$Long,
cex=1,pch=20,col="red", MyMap=Map, NEWMAP=FALSE)
##OR
tmp <- PlotOnStaticMap(lat=pm$Lat, lon=pm$Long,
cex=1,pch=20,col=heat.colors(nrow(pm), alpha = pm$pm10), MyMap=Map, NEWMAP=FALSE)

### meusezinc data
library(rgdal)
Df=read.csv("meusezinc.csv",sep=" ",header=TRUE)
coordinates(Df)=~ x + y
proj4string(Df) = CRS("+init=epsg:28992")
bbox(Df)
Df1 = spTransform(Df, CRS("+init=epsg:4326"))
bbox(Df1)
Df2=as.data.frame(Df1)
names(Df2)<-c("zinc","Longitude","Latitude")
bb <- qbbox(lat=range(Df2$Latitude), lon=range(Df2$Longitude))
m <- c(mean(Df2$Latitude), mean(Df2$Longitude))
zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="mobile",
NEWMAP=TRUE, destfile="tempmap.jpg", RETURNIMAGE=TRUE, GRAYSCALE=TRUE)
## rescale=(0.5*(Df2$zinc+max(Df2$zinc))-min(Df2$zinc))/(max(Df2$zinc)-min(Df2$zinc))
rescaled=(Df2$zinc-min(Df2$zinc))/(max(Df2$zinc)-min(Df2$zinc))
tmp <- PlotOnStaticMap(lat=Df2$Latitude, lon=Df2$Longitude,
cex=2,pch=20,col=heat.colors(nrow(Df2), alpha = rescaled), MyMap=Map, NEWMAP=FALSE)

#### Meuse data coordinate transformation
library(gstat)
library(rgdal) 
data(meuse) 
coordinates(meuse) =~ x + y 
proj4string(meuse) = CRS("+init=epsg:28992") 
bbox(meuse) 
meuse1 = spTransform(meuse, CRS(paste("+proj=stere +lat_0=90", 
   "+lon_0=0.0 +lat_ts=60.0 +a=6378388 +b=6356912 +x_0=0 +y_0=0"))) 
bbox(meuse1) 
meuse2 <- spTransform(meuse1, CRS("+init=epsg:28992")) 
bbox(meuse2) 
all.equal(bbox(meuse), bbox(meuse2)) 

###########################################3
#### Meuse data plot on google
library(gstat)
library(rgdal)
library(ReadImages)
library(RgoogleMaps)
data(meuse) 
coordinates(meuse) =~ x + y 
proj4string(meuse) = CRS("+init=epsg:28992") 
bbox(meuse) 
meuse1 = spTransform(meuse, CRS("+init=epsg:4326")) 
bbox(meuse1)
meuse2=as.data.frame(meuse1)
mzn=meuse2[,c(14,13,4)]
names(mzn)<-c("Latitude","Longitude","zinc")
bb <- qbbox(lat=range(mzn$Latitude), lon=range(mzn$Longitude))
m <- c(mean(mzn$Latitude), mean(mzn$Longitude))
zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="mobile",
NEWMAP=TRUE, destfile="tempmap.jpg", RETURNIMAGE=TRUE, GRAYSCALE=TRUE)
## rescale=(0.5*(mzn$zinc+max(mzn$zinc))-min(mzn$zinc))/(max(mzn$zinc)-min(mzn$zinc))
rescaled=(mzn$zinc-min(mzn$zinc))/(max(mzn$zinc)-min(mzn$zinc))
tmp <- PlotOnStaticMap(lat=mzn$Latitude, lon=mzn$Longitude,
cex=2,pch=20,col=heat.colors(nrow(mzn), alpha = rescaled), MyMap=Map, NEWMAP=FALSE)



##### OPEN STREET MAP
library(RgoogleMaps)
png(filename="GetMap.OSM_%03d_med.png", width=480, height=480)
CologneMap <- GetMap.OSM(lonR= c(6.89, 7.09), latR = c(50.87, 51), scale = 150000, destfile = "Cologne.png");
  	PlotOnStaticMap(CologneMap, mar=rep(4,4), NEWMAP = FALSE, TrueProj = FALSE, axes= TRUE);
		
		PrincetonMap <- GetMap.OSM(lonR= c(-74.67102, -74.63943), latR = c(40.33804,40.3556), scale = 12500, destfile = "Princeton.png");
		png("PrincetonWithAxes.png", 1004, 732)
        PlotOnStaticMap(PrincetonMap, axes = TRUE, mar = rep(4,4));
        dev.off()
        
###### Meuse on OSM

library(gstat)
library(rgdal)
library(ReadImages)
library(RgoogleMaps)
data(meuse) 
coordinates(meuse) =~ x + y 
proj4string(meuse) = CRS("+init=epsg:28992") 
bbox(meuse) 
meuse1 = spTransform(meuse, CRS("+init=epsg:4326")) 
bbox(meuse1)
meuse2=as.data.frame(meuse1)
mzn=meuse2[,c(14,13,4)]
names(mzn)<-c("Latitude","Longitude","zinc")
bb <- qbbox(lat=range(mzn$Latitude), lon=range(mzn$Longitude))
m <- c(mean(mzn$Latitude), mean(mzn$Longitude))
zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="mobile", ## or satellite
NEWMAP=TRUE, destfile="tempmap.jpg", RETURNIMAGE=TRUE, GRAYSCALE=FALSE)
##rescaled=(0.5*(mzn$zinc+max(mzn$zinc))-min(mzn$zinc))/(max(mzn$zinc)-min(mzn$zinc))
rescaled=(mzn$zinc-min(mzn$zinc))/(max(mzn$zinc)-min(mzn$zinc))
tmp <- PlotOnStaticMap(lat=mzn$Latitude, lon=mzn$Longitude,
cex=2,pch=20,col=heat.colors(nrow(mzn), alpha = rescaled), MyMap=Map, NEWMAP=TRUE)

### with OSM
MMap <- GetMap.OSM(bb$lonR, bb$latR, scale = 12500, destfile = "tempmap.png", NEWMAP = TRUE, RETURNIMAGE=TRUE);
tmp <- PlotOnStaticMap(lat=mzn$Latitude, lon=mzn$Longitude,
cex=2,pch=20,col=heat.colors(nrow(mzn), alpha = rescaled),axes = TRUE, MyMap=MMap, mar = rep(4,4), NEWMAP=FALSE)


####### Coso major faults
#install.packages("geomapdata")
library(rgdal)
library(ReadImages)
library(RgoogleMaps)
library(geomapdata)
data(cosomap)
bb <- qbbox(lon=cosomap$POINTS$lon-360,lat=cosomap$POINTS$lat)
MyMap <- GetMap.bbox(bb$lonR, bb$latR,destfile = "Coso.png",
maptype="satellite",zoom=11)
tmp <- PlotOnStaticMap(MyMap,lon=cosomap$POINTS$lon-360,
lat=cosomap$POINTS$lat, pch=20,cex = .5,col='red',verbose=0);

dev.off()
############ earthquake data
library(rgdal)
library(ReadImages)
library(RgoogleMaps)
#setwd("F:././Documents/R/")
pm<-read.csv("eqs7day-M1.txt", header=TRUE)
bb <- qbbox(lat=range(pm$Lat), lon=range(pm$Lon))
m <- c(mean(pm$Lat), mean(pm$Lon))
zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="mobile",
NEWMAP=TRUE, destfile="tempmap.jpg", RETURNIMAGE=TRUE, GRAYSCALE=TRUE)
tmp <- PlotOnStaticMap(lat=pm$Lat, lon=pm$Long,
cex=1,pch=20,col="red", MyMap=Map, NEWMAP=FALSE)
##OR

tmp <- PlotOnStaticMap(lat=pm$Lat, lon=pm$Long,
cex=1,pch=20,col=heat.colors(nrow(pm), alpha = pm$Magnitude), MyMap=Map, NEWMAP=FALSE)

######## Geological Data

library(rgdal)
library(ReadImages)
library(RgoogleMaps)
setwd("F:././Documents/R/")
geoch<-read.csv("020093942011521183834.csv", header=TRUE)
geo<-geoch[,c(6,7,8,9)]
names(geo)<-c("Lat","Lon","LatMax","LonMax")
geo<-geo[-(99:109),]
pm<-geo
bb <- qbbox(lat=range(pm$Lat), lon=range(pm$Lon))
m <- c(mean(pm$Lat), mean(pm$Lon))
zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="mobile",
NEWMAP=TRUE, destfile="tempmap.jpg", RETURNIMAGE=TRUE, GRAYSCALE=TRUE)
tmp <- PlotOnStaticMap(lat=pm$Lat, lon=pm$Long,
cex=1,pch=20,col="red", MyMap=Map, NEWMAP=FALSE)
##OR

tmp <- PlotOnStaticMap(lat=pm$Lat, lon=pm$Long,
cex=1,pch=20,col=heat.colors(nrow(pm), alpha = pm$Magnitude), MyMap=Map, NEWMAP=FALSE)




