# TODO: Add comment
# 
# Author: ERMIAS
###############################################################################


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