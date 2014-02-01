# import data
library(gstat)
library(automap)
tab = read.table("/data/gtestsim1.csv", header=TRUE)
coordinates(tab) = ~x+y
v = as.data.frame(variogram(data~1, tab, cloud = TRUE, cutoff = Inf))
# nach Distanzen sortierte Variogrammwolke => liefert keine anderen Ergebnisse mit fit.variogram.gls
v.sort = v[order(v$dist),]
av=autofitVariogram(data~1, tab, model=c("Sph"))
model = av$var_model

i = v.sort$left
j = v.sort$right
y = v.sort$gamma
h0 = v.sort$dist
dists = spDists(tab)
n = length(i)




# function to compute spherical variogram

gamsph<-function(h,th=rbind(1,1,1)){
if (h==0) {
return(0)
}
if (h>0 && h<= th[3]) {
return(th[1]+th[2]*(3/2*(h/th[3])-1/2*(h/th[3])^3))
}
if (h>th[3]) {
return(th[1]+th[2])
}
}

estvar <- function(h0, y, iter=50, tolerance=0.0002, trace=1, th0=rbind(0,1,1)) 
	{

	#EJP added:
	#stop("this function requires nlregb (an S-Plus proprietary function) to work")
	
	n<-ceiling(sqrt(2*length(h0)))
	
	#Vorbereitung für covgamma
	n1<-n*(n-1)/2
	#1. index der gamma[i,j] matrix
	i1<-matrix(1:n,n,n)
	#1. teil des zeilenindex der covgamma gamma matrix
	k1<-matrix(i1[row(i1)<col(i1)],n1,n1)
	#2. teil des zeilenindex der covgamma gamma matrix
	k2<-matrix(t(i1)[row(i1)<col(i1)],n1,n1)
	#1. teil des spaltenindex der covgamma gamma matrix
	k3<-t(k1)
	#2. teil des spaltenindex der covgamma gamma matrix
	k4<-t(k2)
	
	if(!missing(th0)) {
		#EJP outcommented:
		#opt<-nlregb(n*(n-1)/2,cbind(0,max(y/2),max(h0)),fts,y=y^0.25,h1=h0,cv1=diag(n1),lower=cbind(0,0,0))
		opt<-optim(par = c(0,max(y/2),max(h0)), ftsOpt,
			lower=cbind(0,0,0), method = "L-BFGS-B",
			y=y^0.25, h1=h0, cv1=diag(n1))
		th1 <- opt$par
	}
	else
		th1<-th0
	th1<-cbind(0,max(y/2),max(h0))
	#th0<-th1_c(3.72635248595876, 15.5844183738953, 1.22109233789852)
	#th1<-c(0.0000000,7.6516077,0.7808538)
	for (i in 1:iter) {
		if(trace>0) 
			print(i)
		gg<-sqrt(2*gamsph(h0,th1))
		#Spalte 1, Spalte 2, ...
		#gamma vektor wird als matrix dargestellt
		tt<-matrix(gg[(t(i1)-2)*(t(i1)-1)/2+i1],n,n)
		#symmetrisierung
		tt1<-tt
		tt1[row(tt1)>col(tt1)]<-t(tt)[row(tt1)>col(tt1)]
		#diagonale löschen
		tt1[row(tt1)==col(tt1)]<-0
		#covgamma wird berechnet
		cg<-matrix(tt1[(k4-1)*n+k1]+tt1[(k2-1)*n+k3]-tt1[(k3-1)*n+k1]-tt1[(k4-1)*n+k2],n1,n1)
		cgcg<-outer(gg,gg,"*")
		corg<-sqrt(cgcg)*ficorr((cg*lower.tri(cg))/cgcg)
		corg<-sqrt(2)*(sqrt(pi)-gamma(0.75)^2)/pi*(corg+t(corg)+diag(gg))
		infm<-solve(corg);
		cv<-chol((infm+t(infm))/2);
		#sc<-cbind(1/th1[2],1/th1[2],1/th1[3])
		#EJP outcommented:
		#opt<-nlregb(n*(n-1)/2,th1,fts,y=y^0.25,h1=h0,cv1=cv,lower=cbind(0,0,0))
		opt <- optim(par = th1, ftsOpt,
			lower=cbind(0,0,0), method = "L-BFGS-B",
			y=y^0.25,h1=h0,cv1=cv)
		if(trace>0) print(opt$par)
		if(sum(abs((th1-opt$par)/(th1+0.00001)))<=tolerance)
			break
		th1<-opt$par
	}
	print("Fertig")
	v<-list(pars=opt$par)
	v$cg<-corg
	v$res<-y^0.25-(2^0.25*gamma(0.75)/sqrt(pi))*gamsph(h0,v$pars)^0.25
	v$lof<-t(v$res)%*%solve(corg,v$res)
	v
}


