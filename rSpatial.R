########################################################
##################### ERITREA###########################
library(sp)
#install.packages("RColorBrewer")
n=20
plot(1:n, pch=CIRCLE<-16, cex=1:n, col=rainbow(n))

# get spatial data for ERITREA on regions level
con <- url("http://www4.uji.es/~al143368/ERI_adm2.RData")
print(load(con))
close(con)
# plot Germany with random colors
col = rainbow(length(levels(gadm$NAME_2)))
spplot(gadm, "NAME_2", col.regions=col, main="Eritrean Regions",
       colorkey = FALSE, lwd=.4, col="white")



################### NASA TEMPERATURE ANOMALY ############
setwd("C:\\Users\\ermias\\Documents\\IFGI\\Geostatistics_SS2010")
#giss<-read.table("AnnualMeanSurfaceAirTemperatureChange.txt",header=T)

library(pwr)
library(ggplot2)

URL <- "http://www4.uji.es/"
PATH <- "~al143368/"
FILE <- "AnnualMeanSurfaceAirTemperatureChange.txt"
#download.file(paste(URL,PATH,FILE,sep=""),"AirTemp.txt")
#giss<-read.table(file = "AirTemp.txt",header=T)
airTemp<-url(paste(URL, PATH, FILE, sep = ""),open = "rt")
giss<-read.table(airTemp,header=T)
### OR
airTemp<-url(paste("http://www4.uji.es/~al143368/AnnualMeanSurfaceAirTemperatureChange.txt"),open = "rt")
giss<-read.table(airTemp,header=T)

#download.file("http://www4.uji.es/~al143368/AnnualMeanSurfaceAirTemperatureChange.txt","AirTemp.txt")
#giss<-read.table(file = "AirTemp2.txt",header=T)

head(giss, 2)
##   Year Annual_Mean X5_Year_Mean
## 1 1880       -0.28           NA
## 2 1881       -0.21           NA

colnames(giss)[1]= "Year"
colnames(giss)[2]= "Annual_Mean"
colnames(giss)[3]= "X5_Year_Mean"

## calculate power for different sample sizes
n <- 130 # most recent year index
m <- 100 # oldest year index
f2 <- 0.35 # cohen's f2 0.02, 0.15, and 0.35 represent small, medium, and large effect sizes.
alpha <- 0.05

pwr.graph <- function(n, m, f2, alpha) {
  giss.n <- (m+4):n-m+1 # sample size
  giss.pwr <- cbind(giss.n, 0, 0, 0)
  for (i in giss.n - 4) {
    giss.data <- giss[(n-i-4+1):n,]
    giss.lm <- lm(Annual_Mean ~ Year, data = giss.data)
    giss.pwr[i,3] <- giss.r2 <- summary(giss.lm)$r.squared
    giss.pwr[i,2] <- pwr.f2.test(summary(giss.lm)$f[2], summary(giss.lm)$f[3],
                                 giss.r2 / (1 - giss.r2), alpha)$power
    giss.pwr[i,4] <- as.numeric(tail(giss.data, 1)[1])
  }
  giss.pwr
}

n <- 130
giss.pwr <- pwr.graph(n, n-30, f2, alpha)
for (n in rev(seq(70, n-1, by = 1))) {
  giss.pwr <- rbind(giss.pwr, pwr.graph(n,n-30,f2,alpha))
}
colnames(giss.pwr) <- c("Sample Size", "Power", "R^2", "Latest Year")

fac.name <- function(value, name) {
  x <- value
  names(x) <- name
  x
}

Latest.Year <- giss.pwr[,4]

num.cuts <- 7 # cut(unique(Latest.Year), num.cuts) should be whole numbers
giss.legend.name <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", unique(cut(unique(Latest.Year), num.cuts))))
names(giss.legend.name) <- unique(cut(unique(Latest.Year), num.cuts))

p1 <- ggplot(aes(Sample.Size, Power), data = data.frame(giss.pwr)) +
  geom_point(aes(fill = cut(Latest.Year, num.cuts), col = cut(Latest.Year, num.cuts)), alpha = I(.2), size = I(3), position = "jitter") +
  stat_smooth(aes(fill = cut(Latest.Year, num.cuts), col = cut(Latest.Year, num.cuts)), fullrange = T) +
  stat_summary(aes(group = 1), fun.data = "mean_cl_boot", geom = "errorbar", color = "white", size = I(1.1)) +
  scale_colour_manual(name = "Latest Year", values = as.factor(fac.name(rgb(seq(1,0,len=num.cuts),0,0), names(giss.legend.name)))) + #, breaks = giss.legend.name, labels = names(giss.legend.name)) +
  scale_fill_manual(name = "Latest Year", values = as.factor(fac.name(rgb(seq(1,0,len=num.cuts),0,0), names(giss.legend.name)))) +#, breaks = giss.legend.name, labels = names(giss.legend.name))
  opts(title = expression(paste("GISS Temps Power Analysis of Trends: Effect Size Cohen's ", f^2))) +
  ylim(0,1.1) + xlab("Sample Size (subtract from Latest Year to get Start Year)")

p2 <- ggplot(aes(Year, Annual_Mean), data = giss) +
  scale_fill_gradient(low = "black", high = "red") +
  opts(panel.grid.minor = theme_line(colour = NA),
       panel.grid.major = theme_line(colour = NA),
       panel.border = theme_rect(fill = NA, colour = NA),
       panel.background = theme_rect(fill = NA, colour = NA),
       plot.background = theme_rect(fill = NA, colour = NA),
       legend.position = "none")

p2 <- p2 + geom_polygon(aes(c(Year, rev(Year)),
                            c(pmin(Annual_Mean, min(giss$Annual_Mean, na.rm = T), na.rm = T),
                              rev(pmax(Annual_Mean, max(giss$Annual_Mean, na.rm = T), na.rm = T))),
                            fill = Year,
                            group = c(cut(Year, num.cuts), rev(cut(Year, num.cuts)))),
                        subset = .(Year > min(giss.legend.name) - num.cuts),
                        alpha = I(.5)) + geom_point(col = I("white"))

# theoretical values w/ strong effect size .35
s <- 5:30 # sample sizes
s.p <- pwr.f2.test(1, s-2, .35, .05)$power
s.df <- data.frame(s, s.p)
p3 <- geom_line(aes(s, s.p), data = s.df, col = I("yellow"))

grid.newpage()
library(Hmisc)
p1 + opts(panel.border = theme_blank()) + p3
print(p2, vp = viewport(just = c("left", "top"), x = unit(.05, "npc"), y = unit(.96, "npc"), width = .4, height = .3))




################## MAPPING PACKAGE #####################
library(maps)
getDocNodeVal=function(doc, path)
{
   sapply(getNodeSet(doc, path), function(el) xmlValue(el))
}


gGeoCode=function(str)
{
  library(XML)
  u=paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
  doc = xmlTreeParse(u, useInternal=TRUE)
  str=gsub(' ','%20',str)
  lng=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lat")
  lat=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lng")
  c(lat,lng)
}


bullseyeEtc=function(str)
{
  for (i in 1:10){points(loc[1],loc[2],col=heat.colors(10)[i],cex=i)}
  for (i in 1:10){points(loc[1],loc[2],col=heat.colors(10)[i],cex=i*.5)}
  title(main='The R User Conference 2010', sub='July 20-23, 2010')
  mtext("NIST: Gaithersburg, Maryland, USA")
  mtext("http://user2010.org",4)
}


loc=gGeoCode('100 Bureau Drive Gaithersburg, MD, 20899, USA')


# World
map('world', plot=TRUE, fill=TRUE);bullseyeEtc()

X11()
# USA
map('usa', plot=TRUE, fill=TRUE);bullseyeEtc()

# State
map('state','Maryland', plot=TRUE, fill=TRUE);bullseyeEtc()

########################################################
################# STACK OVERFLOW SOLUTION ##############
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}
# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}


############### edited StackOverflow manager

.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.prettysize <- sapply(obj.size, function(r) prettyNum(r, big.mark = ",") )
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size,obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
        out <- out[c("Type", "PrettySize", "Rows", "Columns")]
        names(out) <- c("Type", "Size", "Rows", "Columns")
    if (head)
        out <- head(out, n)
    out
}
##############COLORS############
n <- 20
plot(1:n, pch=CIRCLE<-16, cex=1:n, col=rainbow(n))


################## World bank data##################
#install.packages("WDI")
library(WDI)
library(ggplot2)
DF <- WDI(country=c("US","CA","MX"), indicator="NY.GDP.MKTP.KD.ZG", start=1990, end=2008)
ggplot(DF, aes(year, NY.GDP.MKTP.KD.ZG, color=country))
+geom_line(stat="identity")+theme_bw()
+xlab("Year")+opts(title="Annual GDP Growth rate (%)")+ylab("")


#################### World Bank 2 ###################3
library('XML')

plotWorldBank= function (country='US',
indicator='NY.GDP.MKTP.CD',
start_year=2002,
end_year=2008,
color='blue'
){
# Construct the URL
url = paste('http://open.worldbank.org/countries/',
   country,
  '/indicators/',
  indicator,
  '?date=',
  start_year,
  ':',
  end_year,
  sep='')

# Print URL for reference
print(url)

# Parse the XML
doc = xmlTreeParse(url, useInternal = TRUE)

# Extract the relevant values
indicator = xmlValue(getNodeSet(doc, "//wb:indicator")[[1]])
countryName = xmlValue(getNodeSet(doc, "//wb:country ")[[1]])
values = sapply(getNodeSet(doc, "//wb:value") , function(el) xmlValue(el))
dates = sapply(getNodeSet(doc, "//wb:date") , function(el) xmlValue(el))
names(values)=dates

# Plot the data
par(las=2,mar=c(4, 8, 1, 2) + 0.1)
barplot(t(rev(values)), main=paste(countryName ,indicator),
col=color)
}


#####################GERMANY UNEMPLOYMENT########################
library(sp)
library(RColorBrewer)

# get spatial data for Germany on county level
con <- url("http://gadm.org/data/rda/DEU_adm3.RData")
print(load(con))
close(con)
# plot Germany with random colors
col = rainbow(length(levels(gadm$NAME_3)))
spplot(gadm, "NAME_3", col.regions=col, main="German Regions",
       colorkey = FALSE, lwd=.4, col="white")
	   
	   
###############################################################
###############################################################
### DATA PREP ###
# loading the unemployment data
setwd("C:\\Users\\ermias\\Documents\\IFGI\\Geostatistics_SS2010")

unempl <- read.delim2(file="data_germany_unemployment_by_county.txt", header = TRUE, sep = "\t",
                     dec=",", stringsAsFactors=F)

# due to Mac OS encoding, otherwise not needed
gadm_names <- gadm$NAME  
# fuzzy matching of data: quick & dirty
# caution: this step takes some time ~ 2 min.

# parsing out "Städte"
gadm_names_n <- gsub("Städte", "", gadm_names) 

total <- length(gadm_names_n)
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3) 
order <- vector()
for (i in 1:total){  
   order[i] <- agrep(gadm_names_n[i], unempl$Landkreis, 
                     max.distance = 0.2)[1]
 setTxtProgressBar(pb, i)               # update progress bar
}
# choose color by unemployment rate
col_no <- as.factor(as.numeric(cut(unempl$Wert[order],
                    c(0,2.5,5,7.5,10,15,100))))
levels(col_no) <- c(">2,5%", "2,5-5%", "5-7,5%",
                    "7,5-10%", "10-15%", ">15%")
gadm$col_no <- col_no
myPalette<-brewer.pal(6,"Purples")

# plotting
spplot(gadm, "col_no", col=grey(.9), col.regions=myPalette,
main="Unemployment in Germany by district")

###############################################################
###############################################################
library(sp)
library(maptools)

nc1 <- readShapePoly("./data/DEU_adm/DEU_adm1.dbf",
                    proj4string=CRS("+proj=longlat +datum=NAD27"))
nc3 <- readShapePoly("./data/DEU_adm/DEU_adm3.dbf",
                    proj4string=CRS("+proj=longlat +datum=NAD27"))

# col_no comes from the calculations above
par(mar=c(0,0,0,0))
plot(nc3, col=myPalette[col_no], border=grey(.9), lwd=.5)
plot(nc1, col=NA, border=grey(.5), lwd=1, add=TRUE)

###############################################################
###############################################################


################### READ RDATA###########
#########################################
> infilepath <- "C:/R/workdir/projectname/lab.RData" 
> outfilepath <- "C:/R/workdir/projectname/lab.txt" 
> load(infilepath) 

##now, assume that there is a single table in the .RData file called my.data.

>ls() 

     my.data 
>write.table(my.data, file = outfilepath) 
#that should do the trick. of course, this is a bit different if you are trying to output something other than a table. if you want to print out summaries exactly how they appear in R, use capture.output, see

> ?capture.output 


################ Weekend ART

# More aRt
par(bg="white")
par(mar=c(0,0,0,0))
plot(c(0,1),c(0,1),col="white",pch=".",xlim=c(0,1),ylim=c(0,1))
iters = 500
for(i in 1:iters) {
	center = runif(2)
	size = 1/rbeta(2,1,3)
 
	# Let's create random HTML-style colors
	color = sample(c(0:9,"A","B","C","D","E","F"),12,replace=T)
	fill = paste("#", paste(color[1:6],collapse=""),sep="")
	brdr = paste("#", paste(color[7:12],collapse=""),sep="")
 
	points(center[1], center[2], col=fill, pch=20, cex=size)
	points(center[1], center[2], col=fill, pch=21, cex=size,lwd=runif(1,1,4))
}

############### 3D volcanic terrain view
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

z0 <- min(z) - 20
z <- rbind(z0, cbind(z0, z, z0), z0)
x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
y <- c(min(y) - 1e-10, y, max(y) + 1e-10)

fill <- matrix("green3", nr = nrow(z)-1, nc = ncol(z)-1)
fill[ , i2 <- c(1,ncol(fill))] <- "gray"
fill[i1 <- c(1,nrow(fill)) , ] <- "gray"

par(bg = "lightblue",mar=c(.5,.5,2.5,.5))
persp(x, y, z, theta = 120, phi = 15, col = fill, scale = FALSE, axes = FALSE)
title(main = "ALID VOLCANO\nOne of the Eritrean volcanoes at East African Rift System.",font.main = 4)

x11()
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

z0 <- min(z) - 20
z <- rbind(z0, cbind(z0, z, z0), z0)
x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
y <- c(min(y) - 1e-10, y, max(y) + 1e-10)

fill <- matrix("green3", nr = nrow(z)-1, nc = ncol(z)-1)
fill[ , i2 <- c(1,ncol(fill))] <- "gray"
fill[i1 <- c(1,nrow(fill)) , ] <- "gray"

par(bg = "slategray",mar=rep(.5,4))
persp(x, y, z, theta = 135, phi = 30, col = fill, scale = FALSE,
      ltheta = -120, lphi = 15, shade = 0.65, axes = FALSE)
title(main = "ALID VOLCANO\nOne of the Eritrean volcanoes at East African Rift System.",font.main = 4)


############### TERRAIN WITHOUT COLOR ################
x11()
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
par(mar=rep(.5,4))
persp(x, y, z, theta = 120, phi = 15, scale = FALSE, axes = FALSE)

############## sea wave
install.packages("seewave")
require(seewave)

data(pellucens)
par(bg = "black", col = "white")
pellucenszoom <- cutw(pellucens, f = 22050, from = 1, plot = FALSE)
spectro(pellucenszoom, f = 22050, wl = 512, ovlp = 85, collevels = seq(-25,
    0, 0.5), osc = TRUE, colgrid = "white", palette = rev.heat.colors,
    colwave = "white", colaxis = "white", collab = "white", colline = "white")

########## SIN WAVE #######

x<-seq(0,20,by=0.001)
y<-2*sin(2*pi*.5*x) #amplitude =2, frequency=0.5
plot(x,y,type="l")

http://pbil.univ-lyon1.fr/ADE-4/ade4-html/add.scatter.html
######## Plotting with poisson
x = seq(.001,50,.001)
 par(bg="black")
 par(mar=c(0,0,0,0)) 
 plot(x,sin(1/x)*rpois(length(x),x),pch=20,col="blue")
#install.packages(c("Rcpp","RInside","inline"))


conv <- function ( a , b )
.C( " convolve " ,
as.double ( a ) ,
as.integer ( length ( a ) ) ,
as.double ( b ) ,
as.integer ( length ( b ) ) ,
ab = double ( length ( a ) + length ( b ) - 1) ) $ab


###### Code and brief instruction for graphing Twitter with R
# Load twitteR package
library(twitteR)

# Load igraph package
library(igraph)

# Set up friends and followers as vectors. This, along with some stuff below, is not really necessary, but the result of my relative inability to deal with the twitter user object in an elegant way. I'm hopeful that I will figure out a way of shortening this in the future

friends <- as.character()
followers <- as.character()

# Start an Twitter session. Note that the user through whom the session is started doesn't have to be the one that your search for in the next step. I'm using myself (coffee001) in the code below, but you could authenticate with your username and then search for somebody else.

sess <- initSession('ermiasbet', 'ermi243')

# Retrieve a maximum of 500 friends for user 'coffee001'.

friends.object <- userFriends('ermiasbet', n=500, sess)

# Retrieve a maximum of 500 followers for 'coffee001'. Note that retrieving many/all of your followers will create a very busy graph, so if you are experimenting it's better to start with a small number of people (I used 25 for the graph below).

followers.object <- userFollowers('ermiasbet', n=500, sess)

# This code is necessary at the moment, but only because I don't know how to slice just the "name" field for friends and followers from the list of user objects that twitteR retrieves. I am 100% sure there is an alternative to looping over the objects, I just haven't found it yet. Let me know if you do...

for (i in 1:length(friends.object))
{
friends <- c(friends, friends.object[[i]]@name);
}

for (i in 1:length(followers.object))
{
followers <- c(followers, followers.object[[i]]@name);
}


# Create data frames that relate friends and followers to the user you search for and merge them.

relations.1 <- data.frame(User='ermiasbet', Follower=friends)
relations.2 <- data.frame(User=followers, Follower='ermiasbet')
relations <- merge(relations.1, relations.2, all=T)

# Create graph from relations.

g <- graph.data.frame(relations, directed = T)

# Assign labels to the graph (=people's names)

V(g)$label <- V(g)$name

# Plot the graph.
plot(g)

############# Zone of Instability
# Take a wacky walk, return the final "track" steps
wackyWalk <- function(iters, track=iters) {
	locations = c()
	mean2use = 0
	sd2use = 1
 
	for (i in 1:iters) {
		mean2use = rnorm(1,mean2use,sd2use) 
 
		# The farther from the center, the smaller the variance
		sd2use = abs(1/mean2use)
		if(track > (iters - i) ) {
	 		locations = c(locations, mean2use)
	 	}
	}
	return(locations)
}
 
# How many steps to take
iters = 300
track = 300
locations = wackyWalk(iters,track)
 
# Start us off with a plot
plot(0,0,xlim=c(min(locations),max(locations)),ylim=c(0,iters),pch=20,col="white")
 
for (i in 1:track) {
	points(locations[i],i,pch=20,col="blue")
 
	# To create a pseudo animation, take a break between plotting points
	Sys.sleep(.10)
}

# How does the number of steps compare with distance from center
meta = c()
for (j in 1:20) {
	iters = 2^j
	track = 1
	meta = c(meta, wackyWalk(iters,track))
}
 
plot(1:20, abs(meta), pch=20, col="blue",xlab="2^x",ylab="abs value of final number in sequence")



######################################################################################################


################ FRACTALS ###############
#########################################
library(ggplot2)

max_iter=25
cl=colours()
step=seq(-2,0.8,by=0.005)
points=array(0,dim=c(length(step)^2,3))
t=0

for(a in step)
{
  for(b in step+0.6)
  {
    x=0;y=0;n=0;dist=0
    while(n<max_iter & dist<4)
    {
      n=n+1
      newx=a+x^2-y^2
      newy=b+2*x*y
      dist=newx^2+newy^2
      x=newx;y=newy
    }

    if(dist<4)
    { 
      color=24 # black
    }
    else
    {
      color=n*floor(length(cl)/max_iter)
    }

    t=t+1
    points[t,]=c(a,b,color)
  }
}

df=as.data.frame(points)	

# Can change the colors by fiddling with the following.
# last_plot() + scale_colour_manual(values=sort(c("#00000000", rainbow(23)), decreasing=FALSE))

ggplot(data=df, aes(V1, V2, color=cl[V3]))+ 
geom_point() + 
opts(panel.background=theme_blank(), 
      panel.grid.major=theme_blank(), 
      panel.grid.minor=theme_blank(), 
      axis.ticks=theme_blank(), 
      axis.text.x=theme_blank(), 
      axis.text.y=theme_blank(), 
      axis.title.x=theme_blank(), 
      axis.title.y=theme_blank(), legend.position = 'none')  

ggsave('mandelbrot_ggplot2.png')

print('Image Saved.')
dev.off()

####################################################
##################### MANDELBROT SET ##
####################

Limits=c(-2,0.8)
MaxIter=25
cl=colours()
Step=seq(Limits[1],Limits[2],by=0.005)
S=floor(length(cl)/MaxIter)
Dist=0
PointsMatrix=array(0,dim=c(length(Step)*length(Step),3))
t=0


for(a in Step)
{
	for(b in Step+0.6)
	{
		x=0;y=0;n=0;Dist=0
		while(n<MaxIter & Dist<4)
		{
			n=n+1
			newx=a+x^2-y^2
			newy=b+2*x*y
			Dist=newx^2+newy^2
			x=newx;y=newy
		}
		if(Dist<4) colour=24 # black colour
		else colour=n*S
		t=t+1
		PointsMatrix[t,]=c(a,b,colour)
	}
}

X11()

plot(PointsMatrix[,1], PointsMatrix[,2], xlim=Limits, ylim=Limits+0.6, col=cl[PointsMatrix[,3]], pch=".")

#########################################
################ WEEKEND ART ############

# Circle lengths
j = seq(0.1,1.9,.08)
 
par(bg = "black")
plot(-2,-2,pch=".",xlim=c(-2,2),ylim=c(-2,2),col="white")
 
# How many dots around the circle?
dots = 1000
 
# Create an offkilter circle
rads = seq(0,2*pi,2*pi/dots)
 
for(aLength in j) {
	# Pick a random color
	myCol = paste("#",paste(sample(c(1:9,"A","B","C","D","E","F"),6,replace=T),collapse=""),collapse="",sep="")
 
	# Start at length = 1, then walk.
	myLength = rep(aLength,dots)
 
	for(i in 2:dots) {
		myLength[i] = myLength[(i-1)] + rnorm(1,0,sd=.005)
 
		# Closer we are to end, faster we return to where started so circle closes
		dist = aLength - myLength[i]
		myLength[i] = aLength - (dist*((dots-(i/4))/(dots)))
	}
 
 
 
	for(i in 1:dots) {
		cat(myLength[i]*cos(rads[i]),myLength[i]*sin(rads[i]),"\n")
		points(myLength[i]*cos(rads[i]),myLength[i]*sin(rads[i]),col=myCol,pch=20,cex=2)
	}
}


