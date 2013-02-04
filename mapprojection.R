install.packages("akima")
install.packages("mapproj")
#interpolation of xyz data and projection onto a map
map.xyz <- function(xyz_data, file_name="map.xyz.png",
 
projection="stereographic", orientation=NULL,
 
par=NULL, fill=FALSE, nside=40, breaks=100, res=100,
 
xlim = NULL, ylim = NULL
 
){
 
 
 if(length(which(rowSums(is.na(xyz_data)) != 0)) > 0){
 
  xyz_data <- xyz_data[-which(rowSums(is.na(xyz_data)) != 0),]
 
 }
 
 
 require(akima)
 
 require(maps)
 
 require(mapproj)
 
 
 
 temp<-interp(
 
  xyz_data[,1], xyz_data[,2], xyz_data[,3],  
 
  xo=seq(min(xyz_data[,1]), max(xyz_data[,1]), length = nside), 
 
  yo=seq(min(xyz_data[,2]), max(xyz_data[,2]), length = nside, extrap=TRUE)
 
 )
 
 
 
 polys<-vector("list", length(as.vector(temp$z)))
 
 for(i in 1:length(polys)){
 
  lonx <- pos2coord(pos=i, mat=temp$z)$coord[1]
 
  laty <- pos2coord(pos=i, mat=temp$z)$coord[2]
 
  ifelse(laty < length(temp$y), neigh_y<-c(laty+1,lonx), neigh_y<-c(laty-1,lonx))
 
  ifelse(lonx < length(temp$x), neigh_x<-c(laty,lonx+1), neigh_x<-c(laty,lonx-1)) 
 
  dist_y <- earth.dist(temp$x[lonx], temp$y[laty], temp$x[neigh_y[2]], temp$y[neigh_y[1]])
 
  dist_x <- earth.dist(temp$x[lonx], temp$y[laty], temp$x[neigh_x[2]], temp$y[neigh_x[1]])
 
  s1 = new.lon.lat(temp$x[lonx], temp$y[laty], 180, dist_y/2)
 
  s3 = new.lon.lat(temp$x[lonx], temp$y[laty], 0, dist_y/2)
 
  s2 = new.lon.lat(temp$x[lonx], temp$y[laty], 270, dist_x/2)
 
  s4 = new.lon.lat(temp$x[lonx], temp$y[laty], 90, dist_x/2)
 
  polys[[i]] = cbind(c(s2[1], s2[1], s4[1], s4[1]), c(s1[2], s3[2], s3[2], s1[2]))
 
 }
 
 
 
 M.range=range(temp$z, na.rm=TRUE)
 
 M.breaks <- pretty(M.range, n=breaks)
 
 M.cols=colorRampPalette(c("blue","cyan", "yellow", "red"),space="rgb")
 
 colorlut <- M.cols(length(M.breaks)) # color lookup table
 
 colorvalues <- colorlut[((as.vector(temp$z)-M.breaks[1])/(range(M.breaks)[2]-range(M.breaks)[1])*length(M.breaks))+1] # assign colors to heights for each point
 
 if(is.null(xlim)){xlim <- range(xyz_data[,1], na.rm=TRUE)}
 
 if(is.null(ylim)){ylim <- range(xyz_data[,2], na.rm=TRUE)}
 
 png(filename=file_name, res=res)
 
 map("world",projection=projection, orientation=orientation, par=par, fill=fill, xlim=xlim, ylim=ylim)
 
 for(i in 1:length(polys)){
 
  polygon(mapproject(x=polys[[i]][,1], y=polys[[i]][,2]), col=colorvalues[i], border=NA)
 
  print(i)
 
 }
 
 map("world", add=TRUE, projection="", par=par, fill=fill, xlim=xlim, ylim=ylim)
 
 map.grid(c(-180, 180, -80, 80), nx=10, ny=18, labels=FALSE, col="grey")
 
 dev.off()
 
}

pos2coord<-function(pos=NULL, coord=NULL, mat=NULL){
 
 if(is.null(pos) & is.null(coord) & is.null(mat)){
 
  stop("must supply either 'pos' or 'coord', and 'mat'")
 
 }
 
 if(is.null(pos) & !is.null(coord) & !is.null(mat)){
 
  pos <- ((coord[2]-1)*dim(mat)[1])+coord[1] 
 
 }
 
 if(!is.null(pos) & is.null(coord) & !is.null(mat)){
 
  coord <- NA*c(1:2)
 
  coord[1] <- ((pos-1) %% dim(mat)[1]) +1
 
  coord[2] <- ((pos-1) %/% dim(mat)[1])+1
 
 }
 
 list(pos=pos, coord=coord)
 
}

#distance in kilometers between two long/lat positions (from "fossil" package)
earth.dist <- function (long1, lat1, long2, lat2) 
{
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
}

#new long/lat position given a starting lon/lat and degree bearing (from "fossil" package)
new.lon.lat <- function (lon, lat, bearing, distance) {
    rad <- pi/180
    a1 <- lat * rad
    a2 <- lon * rad
    tc <- bearing * rad
    d <- distance/6378.145
    nlat <- asin(sin(a1) * cos(d) + cos(a1) * sin(d) * cos(tc))
    dlon <- atan2(sin(tc) * sin(d) * cos(a1), cos(d) - sin(a1) * 
        sin(nlat))
    nlon <- ((a2 + dlon + pi)%%(2 * pi)) - pi
    npts <- c(nlon/rad, nlat/rad)
    return(npts)
}

#degree bearing between two long/lat positions (from "fossil" package)
earth.bear <- function (long1, lat1, long2, lat2) 
{
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    bear <- atan2(sin(dlon) * cos(b1), cos(a1) * sin(b1) - sin(a1) * 
        cos(b1) * cos(dlon))
    deg <- (bear%%(2 * pi)) * (180/pi)
    return(deg)
}

lon.lat.filter <- function (lon_vector, lat_vector, west, east, north, south) 
{
 if(west>east) {
  lon_vector_new=replace(lon_vector, which(lon_vector<0), lon_vector[which(lon_vector<0)]+360)
  east_new=east+360
 } else {
  lon_vector_new=lon_vector
  east_new=east
 }
  hits=which(lon_vector_new < east_new & lon_vector_new > west & lat_vector < north & lat_vector > south)
 return(hits)
} 