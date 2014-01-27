
x$x1 = x$Lat
x$y1=x$Long
x$x2 = c(x$Lat[-1],NA)
x$y2 = c(x$Long[-1],NA)
x$t2 = c(x$t[-1],NA)
x = na.omit(x)

distance = function(x) {
	spDistsN1(as.matrix(x[,7:8]), x[,c(9,10)],longlat = T)
}
as.matrix(x[,1],x[,2],nrow(x),2)
x[,1]
summary(x)
dist = apply(x, 1, distance)
distance = function(x) {
		spDistsN1(matrix(c(x[1],x[3]),1,2), c(x[2],x[4]), longlat = TRUE)
}
dist = apply(x[c("x1", "x2", "y1", "y2")], 1, distance)


apply(pm3[,c(1,2)], 1, function(i) spDistsN1(as.matrix(pm3[1,c(1,2)]), i, longlat=F))

x$t = as.POSIXct(
		strptime(do.call(paste, x[c("Date", "Time")]), "%Y/%m/%d %H:%M:%S"))


x$dt = x$t2 - x$t
x$dt[x$dt < 1] = 1
x$speed = x$d / as.numeric(x$dt)

Line4 = function(x) {
	Line(cbind(c(x[1],x[2]),c(x[3],x[4])))
}



apply(pm3[c("x1","x2")], 1, dist)

strptime(t1,"%Y/%m/%d %H:%M:%S")


#x = read.csv("3_1a.txt")
#x = read.csv("All4_3a.txt") => diese Daten werden an Rserve geschickt

# select 10% of all data, regular:
x = x[seq(1,nrow(x),by=10),]

# deselect some extreme pm10 values:
x = x[x$pm10 < .2, ]

library(sp)

x$x1 = x$Long
x$y1 = x$Lat
x$t = as.POSIXct(
		strptime(do.call(paste, x[c("Date", "Time")]), "%Y/%m/%d %H:%M:%S"))

# copy x/y/ of next point:
x$x2 = c(x$Long[-1],NA)
x$y2 = c(x$Lat[-1],NA)
x$t2 = c(x$t[-1],NA)

# remove records with NA
x = na.omit(x)
mydist = function(x) {
	#wrong: sqrt((x[1]-x[2])^2+(x[3]-x[4])^2)
	spDistsN1(matrix(c(x[1],x[3]),1,2), c(x[2],x[4]), longlat = TRUE)
}
x$d = apply(x[c("x1", "x2", "y1", "y2")], 1, mydist)
x$dt = x$t2 - x$t
x$dt[x$dt < 1] = 1
x$speed = x$d / as.numeric(x$dt)

Line4 = function(x) {
	Line(cbind(c(x[1],x[2]),c(x[3],x[4])))
}
llist = lapply(apply(x[c("x1", "x2", "y1", "y2")],1,Line4), Lines, ID = "foo")
# create unique ids:
IDs = paste("ID", 1:length(llist), sep="")
#for(i in seq(along=llist))
#	llist[[i]]@ID=IDs[i]
llist = lapply(1:length(llist), 
		function(x) { llist[[x]]@ID = IDs[x]; llist[[x]] })
sl = SpatialLines(llist, CRS("+proj=longlat"))
sldf = SpatialLinesDataFrame(sl, x, match.ID=FALSE)
# dieser Plot soll als Bild von Rserve wieder an den JavaClient geschickt werden
jpeg("test.jpg", quality=90)
p=spplot(sldf["pm10"], lwd=5, 
		col.regions=bpy.colors(), scales = list(draw=TRUE),
		colorkey=list(space="bottom"))
plot(p)
dev.off()
spplot(sldf["speed"], lwd=5, 
		col.regions=bpy.colors(), scales = list(draw=TRUE),
		colorkey=list(space="bottom"))







