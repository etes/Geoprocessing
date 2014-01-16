# get spatial data for NORWAY on regional level
region<-load("NOR_adm1.RData")
# generate random color for each of the region
color = rainbow(length(levels(gadm$NAME_1)))
# plot the regions of Eritrea with the random color fills
arrow = list("SpatialPolygonsRescale", layout.north.arrow(), offset=c(6,70),scale=1)
spplot(gadm,"NAME_1",lwd=0.6,col="black", col.regions=color,colorkey=FALSE,sp.layout=list(arrow),scales=list(draw=T), main="Norway Regions")

commune<-load("NOR_adm2.RData")
color = rainbow(length(levels(gadm$NAME_2)))
spplot(gadm,"NAME_2",lwd=0.6,col="black", col.regions=color,colorkey=FALSE,sp.layout=list(arrow),scales=list(draw=T), main="Norway Commune")
