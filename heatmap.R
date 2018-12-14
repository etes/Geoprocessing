# Load required packages
#install.packages("stalkR_0.02.zip", repos=NULL, type="source")
#install.packages("C:/Users/ermias/Documents/stalkR.zip", repos=NULL, type="source")
#install.packages("C:/Users/ermias/Documents/stalkR_0.02.zip",lib="C:/Users/ermias/Documents/R/win-library/2.13", repos=NULL, type="source")

pkgs = c("RSQLite","XML","ggplot2", "maps", "mapproj", "RgoogleMaps")

loadPackages <- function(x){
    new.x <- x[!(x %in% installed.packages()[, "Package"])]
    if (length(new.x))
        install.packages(new.x, dependencies = TRUE)
    sapply(x, require, character.only = TRUE)
}
loadPackages(pkgs)

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
#### Meuse data: plot with basemap
library(gstat)
library(rgdal)
library(sf)
library(ggplot2)
library(OpenStreetMap)
data(meuse)
coordinates(meuse) = ~x+y
proj4string(meuse) = CRS("+init=epsg:28992")
meuse1 = spTransform(meuse, CRS("+init=epsg:4326"))
bb <- st_bbox(meuse1)
meuse2=as.data.frame(meuse1)
mzn=meuse2[,c(14,13,4)]
names(mzn)<-c("Latitude","Longitude","zinc")
m <- c(mean(mzn$Latitude), mean(mzn$Longitude))
rescaled <- (mzn$zinc-min(mzn$zinc))/(max(mzn$zinc)-min(mzn$zinc))
mzn <- cbind(mzn, rescaled)
#mzn.point <- st_as_sf(x = mzn, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84")

basemap <- openmap(c(bb$ymin, bb$xmin)-.003, c(bb$ymax, bb$xmax)+.001, zoom = NULL, 'stamen-terrain')
mzn.coords <- cbind(mzn, projectMercator(mzn$Latitude,mzn$Longitude))

maplayout <- theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
                  axis.text.x = element_blank(), axis.text.y = element_blank(),
                  axis.title.x = element_blank(), axis.title.y = element_blank(),
                  axis.ticks = element_blank())
breaks <- c(100,200,400,800,1600)
autoplot(basemap) +
  geom_point(data=mzn.coords, aes(x, y, size=zinc, color=zinc)) +
  scale_color_gradient(low="blue", high="red", breaks=breaks, guide="legend") +
  scale_size_continuous(breaks=breaks) +
  maplayout

# We can also create WGS84 map but we need to reproject the basemap from webmercator

basemap.latlon <- openproj(basemap, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
autoplot(basemap.latlon) +
  geom_point(data=mzn.coords, aes(Longitude, Latitude, size=zinc, color=zinc)) +
  scale_color_gradient(low="blue", high="red", breaks=breaks, guide="legend") +
  scale_size_continuous(breaks=breaks) +
  maplayout

###### Meuse data: plot on OSM
basemap.osm <- openmap(c(bb$ymin, bb$xmin)-.003, c(bb$ymax, bb$xmax)+.001, zoom = NULL, 'osm')
autoplot(basemap.osm) +
	geom_point(data=mzn.coords, aes(x, y, size=zinc, color=zinc)) +
	scale_color_gradient(low="blue", high="red", breaks=breaks, guide="legend") +
	scale_size_continuous(breaks=breaks) + maplayout

####### Coso major faults
#install.packages("geomapdata")
library(rgdal)
library(sf)
library(geomapdata)
library(ggplot2)
library(OpenStreetMap)
data(cosomap)
cp <- as.data.frame(cosomap$POINTS)
cp$lon <- cp$lon - 360
cosomap.points <- st_as_sf(x = cp, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")
bb <- st_bbox(cosomap.points)
basemap <- openmap(c(bb$ymin, bb$xmin)-.003, c(bb$ymax, bb$xmax)+.003, zoom = NULL, 'bing')
cp.coords <- cbind(cp, projectMercator(cp$lat,cp$lon))
autoplot(basemap) +
	geom_point(data=cp.coords, aes(x, y), size=1, color='red') +
	maplayout

basemap.latlon <- openproj(basemap, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
autoplot(basemap.latlon) +
	 geom_point(data=cp, aes(lon, lat), color="red") +
	 geom_segment(aes(x = LON1-360, y = LAT1, xend = LON2-360, yend = LAT2), color="yellow", data=as.data.frame(cosomap$STROKES))


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
