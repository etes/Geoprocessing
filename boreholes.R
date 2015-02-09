# Load required packages
if (!require("rgdal")) install.packages("rgdal", repos="http://cran.rstudio.com/")
if (!require("RgoogleMaps")) install.packages("RgoogleMaps")

boreholes<- read.csv("data/boreholes.csv", sep=",", header=TRUE)
bh_grd <- SpatialPoints(boreholes[,c("East","North")],
                        proj4string=CRS("+proj=utm +zone=32 +datum=WGS84"))
bholes<- SpatialPointsDataFrame(bh_grd,boreholes[,-c(2,3)])
bholes<-bholes[,-c(2,3)]
bh_geo<- spTransform(bholes, CRS("+proj=longlat +datum=WGS84"))

North<-coordinates(bh_geo)[,2]
East<-coordinates(bh_geo)[,1]
bb <- qbbox(lat=range(North), lon=range(East))
m <- c(mean(North), mean(North))
zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
Map <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="mobile",
                   NEWMAP=F, destfile="map.png", RETURNIMAGE=T);
tmp <- PlotOnStaticMap(Map,lat=North, lon=East,
                       cex=1.5,pch=20,col="red", add=F, verbose=0);
tmp <- TextOnStaticMap(Map,lat=North, lon=East+.0007, cex=.8, font=4,
                       col="black", labels= boreholes$Number, add=T, verbose=0);
tmp <- PlotArrowsOnStaticMap(Map, lat0=min(North),lon0=max(East)+.006,
                             lat1=min(North)+.0006,lon1=max(East)+.006,lwd=6,col="black",add=T);


#Satellite image as a base map
SMap <- GetMap.bbox(bb$lonR, bb$latR, zoom=zoom, maptype="satellite",
                   NEWMAP=F, destfile="map.png", RETURNIMAGE=TRUE);
tmp <- PlotOnStaticMap(SMap,lat=North, lon=East,
                       cex=1.5,pch=20,col="red", add=F, verbose=0);
tmp <- TextOnStaticMap(SMap,lat=North, lon=East+.0007, cex=.8, font=4,
                       col="yellow", labels= boreholes$Number, add=T, verbose=0);
tmp <- PlotArrowsOnStaticMap(SMap, lat0=min(North),lon0=max(East)+.006,
                             lat1=min(North)+.0006,lon1=max(East)+.006,lwd=6,col="yellow",add=T);
