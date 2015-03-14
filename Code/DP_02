setwd("C:/Users/Jéssica/Dropbox/markaki's project")
options(digits=10)

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggmap")) install.packages("ggmap")
if (!require("TSP")) install.packages("TSP")
if (!require("tspmeta")) install.packages("tspmeta")
if (!require("grid")) install.packages("grid")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("plyr")) install.packages("plyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("timeDate")) install.packages("timeDate")
library(ggplot2); library(ggmap); library(TSP); library(grid); 
library(tspmeta); library(grid); library(gridExtra)
library(plyr);library(timeDate); library(dplyr)
#library(rgdal); library(sp);  library(raster)

#rm(list=ls())
setwd("C:/Users/Jéssica/Dropbox/markaki's project")
options(digits=10)
###READ DATA

#market data
market<-read.table("cans.txt",colClasses=c("numeric","character","character","numeric","numeric"))
market$type<-rep("market",nrow(market))
dimmar<-dim(market)[1]
market$V1<-seq(1,dimmar,1)

#small roads data
smallroads<-read.table("smallroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
smallroads$type<-rep("smallroads",nrow(smallroads))
dimmarsm<-dimmar+dim(smallroads)[1]
smallroads$V1<-seq((dimmar+1),dimmarsm,1)


#big roads data
bigroads<-read.table("bigroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
bigroads$type<-rep("bigroads",nrow(bigroads))
dimmarsmbig<-dimmarsm+dim(bigroads)[1]
bigroads$V1<-seq((dimmarsm+1),dimmarsmbig,1)

#joining
data<-rbind(market,smallroads,bigroads)
colnames(data)<-c("ID","xcoordinate","ycoordinate","capacity","time_service","type")  



mergeData<-  ddply(data,c("xcoordinate","ycoordinate","type"),summarise,cap=sum(capacity),service=sum(time_service))

mergeData<- rbind(mergeData,c("468885.1451","4200354.6723","deposit_trucks",NA,NA))
mergeData<-rbind(mergeData,c("464323.2597","4204134.6673","disposal_rub",NA,NA))
mergeData[,1]<-as.numeric(mergeData[,1])
mergeData[,2]<-as.numeric(mergeData[,2])
mergeData$type<-as.factor(mergeData$type)

#ggplot(data=data,aes(x=xcoordinate,y=ycoordinate,col=type))+geom_point()+theme_bw()
n<-nrow(mergeData)
mergeData$ind<-seq(1:n)



### -----------------------------------------------------------------------------
## Now we have the selected cans, lets asign different lamdas to each can and
## compute the fillness in 10 days horizon
### -----------------------------------------------------------------------------
lambdas<-c(1,2,3,4,5,6)
lambdasList<-rep(lambdas,101)
lambdasList<-lambdasList[sample(1:(dim(mergeData)[1]-2))] ## shuffle the assign lambdas in each can
m<-mergeData[1:606,]
Cans<-m
Cans$lambdas<-lambdasList ## assing the lambdas to each can
Demands<-c()
for( i in 1:nrow(Cans)){
  exp<-rpois(n=10, lambda=Cans$lambdas[i])+1 # +1 because if not it could be fill
  # in 0 days and that was a problem
  expP<-round((1/exp),3)
  Demands<-rbind(Demands,expP)
}

Xt<-ifelse(Demands[,1]==1,0,Demands[,1]) #cleaning the ones in the first row
Xt<-Xt+Demands[,2]

ViolatedCans<-ifelse(Xt>=0.9,1,0)
indCans<-which(ViolatedCans==1)
ViolatedCans<-Cans[indCans,] ## table with the data of the violated cans (we have to do the clusters with this)
ggplot(data=ViolatedCans,aes(x=xcoordinate,y=ycoordinate))+geom_point()+theme_bw()
### -----------------------------------------------------------------------------
## getting the manhatann distances of the violated Cans
### -----------------------------------------------------------------------------
Viol.coordenates<-ViolatedCans[,1:2] ## take only the coord.
Viol.distances<-dist(Viol.coordenates, method = "manhattan") # compute the manhattan distances
### -----------------------------------------------------------------------------
## getting the vertical distance to the nearest violated can
### -----------------------------------------------------------------------------
NonVio.cans<-Cans[-indCans,]
## getting the nearest vertical can
dist<-as.data.frame(matrix(0,nrow(NonVio.cans),nrow(ViolatedCans)))
for( i in 1:nrow(NonVio.cans)){
  dist[i,]<-abs(ViolatedCans$ycoordinate-NonVio.cans$ycoordinate[i])+abs(ViolatedCans$xcoordinate-NonVio.cans$xcoordinate[i])
}
                                                     
ind.dist<-as.data.frame(matrix(0,nrow(NonVio.cans),2))
for( i in 1:nrow(NonVio.cans)){
  ind<-which.min(dist[i,])
  ind.dist[i,1]<-ViolatedCans$ind[ind]
  ind.dist[i,2]<-round(dist[i,ind],4)
}
ind.dist$id<-seq(1,dim(ind.dist)[1],1)
colnames(ind.dist)<-c("Violated cans id","Minimum distance to the non-viol","id_nonviol")

#rm(Viol.coordenates)
#rm(vert.dist)
#rm(ind)
#rm(indCans)
### -----------------------------------------------------------------------------
## Plotting the cans that exceed our 0.9 threeshold vs the not
### -----------------------------------------------------------------------------
yesno<-ifelse(Xt>=0.9,1,0)
VioPlot<-Cans
VioPlot$condition<-as.factor(yesno)
rm(yesno)
ggplot(data=VioPlot,aes(x=xcoordinate,y=ycoordinate,colour=condition,col=ind))+geom_point()+theme_bw()




###############################3
#cluster to the violated cans
coordinates<-ViolatedCans[,1:2]
coordinates$klus<-kmeans(coordinates,10)$cluster
coordinates %>% group_by(klus)


matrix<-list()
for(i in 1:10){
  matrix[[i]]<-coordinates[coordinates$klus==i,]
}
