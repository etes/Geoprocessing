IPtoXY <- function(x) {
   URL_IP <- paste("http://www.datasciencetoolkit.org//ip2coordinates/",
                   x, sep = "")
   api_return <- readLines(URL_IP, warn = F)
   str_elements <- gsub("[^[:alnum:].]", "",
                        strsplit(api_return, "\"")[[1]])
   return(paste(str_elements[grep("longitude", str_elements)+1],
                str_elements[grep("latitude", str_elements)+1],
                sep = ";"))
}

IPtoXY <- function(x) {
   URL_IP <- paste("http://www.maxmind.com/app/locate_demo_ip?ips=",x, sep = "")
api_return <- as.matrix(readLines(URL_IP, warn = F))[299:300,]
latlong<-gsub("</font></td>","",gsub("<td><font size=\"-1\">","",api_return))
   return(as.numeric(latlong))
}

#Example:
IPtoXY("193.90.155.206")
ip.ad<-read.csv("data/ips.txt",header=F,sep=";")
sapply(ip.ad[,1], FUN = IPtoXY)
