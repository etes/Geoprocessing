library(geoR)
library(gstat)
library(RandomFields)

result <- data.frame(name="", gls=FALSE, wlsnugget=0, wlssill=0, wlsrange=0, olsnugget=0, olssill=0, olsrange=0, mlnugget=0, mlsill=0, mlrange=0, remlnugget=0, remlsill=0, remlrange=0)
for (i in 1:1000) {
file = paste("~/sim_2010_09_24/grid100/gridNsim", i, ".csv", sep="")
pm10 = read.table(file, header=TRUE)
#for geoR use
coord = pm10[,1:2]
dat=pm10[,3]
#for gstat use
coordinates(pm10) = ~x+y
x = coordinates(pm10)[, 1]
y = coordinates(pm10)[, 2]
experimentalVariogram = variogram(data~1, pm10)
experimentalCloud = variogram(data~1, pm10, cloud=TRUE, cutoff=Inf)
y = experimentalCloud$gamma
h0 = experimentalCloud$dist
upperSill = max(y)
upperRange = max(h0)
initial_nugget = min(experimentalVariogram$gamma)
initial_range = (0.1 * sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2))
initial_sill = mean(c(max(experimentalVariogram$gamma), median(experimentalVariogram$gamma)))
f = fitvario(x=coord, data=dat,model= "spherical",param=rep(NA,4), table.format=TRUE, lower=c(0, NA, NA, NA), upper=c(0, upperSill, upperSill,upperRange))
wlsnug = f$self[3]
wlssil = f$self[1]
wlsran = f$self[2]
olsnug = f$plain[3]
olssil = f$plain[1]
olsran = f$plain[2]
mlnug = f$ml[3]
mlsil = f$ml[1]
mlran = f$ml[2]
remlnug = f$reml[3]
remlsil = f$reml[1]
remlran = f$reml[2]
res = data.frame(name=file, gls=FALSE, wlsnugget=wlsnug, wlssill=wlssil, wlsrange=wlsran, olsnugget=olsnug, olssill=olssil, olsrange=olsran, mlnugget=mlnug, mlsill=mlsil, mlrange=mlran, remlnugget=remlnug, remlsill=remlsil, remlrange=remlran)
result = rbind(result, res)
}
write.table(result, "~/sim_2010_11_08/allfitK100_2010_11_03_grid05_05_45_1000.csv")




