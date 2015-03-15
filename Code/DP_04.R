##-------------------------------------------------------------------------------------------------
##   Getting Ready all the Demands
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to prepare the data for the DP.

## In all the tables related with cans, there is a variable ind that is the ID of the can, in the
## last plot you'll see why it's important.

## Cans............... table of the 45 initial cans for the testing
## NonVio.cans........ table of the subset of cans not violated
## ViolatedCans ...... table of the subset of cans violated
## Demands............ tables have the generating day from day 1 to day 10
## Xt................. is the fullfilness of the first day we are going to study
## ViolatedCans....... is the table we need to use to do the clusters
## ind.dist .......... is the table that says which is the vertical distance and to which violated cans
## Vioplot............ as the Cans table but with an extra column indicating if it's violated or not
## Viol.distances .... a class with the manhatan distances of the violated cans (needed for clustering)

## USEFULL LINKS:
## 

## TO DO:

## Show we put a min of cans in each cluster?



## GENERAL INFO:
# 
##-------------------------------------------------------------------------------------------------
## Directories
##-------------------------------------------------------------------------------------------------

setwd("~/Documents/Box Sync/Current/Stochastics/DP")
#setwd("C:/Users/Jéssica/Dropbox/markaki's project")

##-------------------------------------------------------------------------------------------------
## Libraries
##-------------------------------------------------------------------------------------------------


options(digits=10)
source("mutliplot.R")
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

##-------------------------------------------------------------------------------------------------
## Data
##-------------------------------------------------------------------------------------------------

rm(list=ls())

#market data
market<-read.table("Data/cans.txt",colClasses=c("numeric","character","character","numeric","numeric"))
market$type<-rep("market",nrow(market))

#small roads data
smallroads<-read.table("Data/smallroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
smallroads$type<-rep("smallroads",nrow(smallroads))

#big roads data
bigroads<-read.table("Data/bigroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
bigroads$type<-rep("bigroads",nrow(bigroads))


#joining
data<-rbind(market,smallroads,bigroads)
colnames(data)<-c("ID","xcoordinate","ycoordinate","capacity","time_service","type")  


data$ID<-seq(1,nrow(data),1)#including an ID

mergeData<-  ddply(data,c("xcoordinate","ycoordinate","type"),summarise,
                   cap=sum(capacity),service=sum(time_service))

mergeData<- rbind(mergeData,c("468885.1451","4200354.6723","deposit",Inf,0))
mergeData<-rbind(mergeData,c("464323.2597","4204134.6673","disposal",Inf,0))


mergeData$xcoordinate<-as.numeric(mergeData$xcoordinate)
mergeData$ycoordinate<-as.numeric(mergeData$ycoordinate)
mergeData$type<-as.factor(mergeData$type)
mergeData$cap<-as.numeric(mergeData$cap)
mergeData$service<-as.numeric(mergeData$service)
n<-nrow(mergeData)
mergeData$ind<-seq(1:n)

ggplot(data=mergeData,aes(x=xcoordinate,y=ycoordinate,col=type,label=ind))+geom_point()+
  theme_bw()+geom_text(hjust=0, vjust=0)

### -----------------------------------------------------------------------------
## Turn the city
### -----------------------------------------------------------------------------

turnCity<-function(mergeData){
  point1<-subset(mergeData,ind==42)[1:2]
  point2<-subset(mergeData,ind==162)[1:2]
  alpha<-atan((point2$ycoordinate-point1$ycoordinate)/(point2$xcoordinate-point1$xcoordinate))
  rotatedAlpha<-matrix(c(cos((pi/2)-alpha),sin((pi/2)-alpha),-sin((pi/2)-alpha),cos((pi/2)-alpha)),2,2)
  points<-t(mergeData[1:2])
  rotatedPoints<-rotatedAlpha%*%points
  rotatedPoints<-t(rotatedPoints)
  rotatedPoints<-as.data.frame(rotatedPoints)
  mergeData$xcoordinate<-rotatedPoints$V1
  mergeData$ycoordinate<-rotatedPoints$V2
  return(mergeData)
}

mergeData<-turnCity(mergeData)

ggplot(data=mergeData,aes(x=xcoordinate,y=ycoordinate,col=type))+geom_point()+
  theme_bw()

### -----------------------------------------------------------------------------
## Now we have the selected cans, lets asign different lamdas to each can and
## compute the fillness in 10 days horizon
### -----------------------------------------------------------------------------
# for this part we don't need the disposal and deposit so we'll work without them
Cans<-subset(mergeData, type!="deposit" & type!="disposal")

lambdas<-c(1,2,3,4,5,6)
lambdasList<-rep(lambdas,101)
lambdasList<-lambdasList[sample(1:nrow(Cans))] ## shuffle the assign lambdas in each can

Cans$lambdas<-lambdasList ## assing the lambdas to each can
Demands<-c()
for( i in 1:nrow(Cans)){
  exp<-rpois(n=10, lambda=Cans$lambdas[i])+1 # +1 because if not it could be fill
  # in 0 days and that was a problem
  expP<-round((1/exp),3)
  Demands<-rbind(Demands,expP)
}

### -----------------------------------------------------------------------------
## Create the Xt of the first stage we are going to solve
### -----------------------------------------------------------------------------

Xt<-ifelse(Demands[,1]==1,0,Demands[,1]) #cleaning the ones in the first row
Xt<-Xt+Demands[,2]
Xt<-as.data.frame(cbind(Xt,Cans$ind))
colnames(Xt)<-c("Demand","ID")
### -----------------------------------------------------------------------------
## Plotting the cans that exceed our 0.9 threeshold 
### -----------------------------------------------------------------------------

ViolatedCans<-ifelse(Xt$Demand>=0.9,1,0)
indCans<-which(ViolatedCans==1)
ViolatedCans<-Cans[indCans,] ## table with the data of the violated cans (we have to do the clusters with this)
ggplot(data=ViolatedCans,aes(x=xcoordinate,y=ycoordinate))+geom_point()+theme_bw()

### -----------------------------------------------------------------------------
## getting the manhatann distances of the violated Cans
### -----------------------------------------------------------------------------
Viol.distances<-dist(ViolatedCans[,1:2], method = "manhattan") # compute the manhattan distances


### -----------------------------------------------------------------------------
## Plotting the cans that exceed our 0.9 threeshold vs the not
### -----------------------------------------------------------------------------
yesno<-ifelse(Xt$Demand>=0.9,1,0)
VioPlot<-Cans
VioPlot$condition<-as.factor(yesno)
ggplot(data=VioPlot,aes(x=xcoordinate,y=ycoordinate,colour=condition,col=ind,label=ind))+
  geom_point()+theme_bw()+  geom_text(hjust=0, vjust=0,size=3)



### -----------------------------------------------------------------------------
## Clusters to the violated cans with manhatann distances
### -----------------------------------------------------------------------------
Viol.distances<-dist(ViolatedCans[,1:2], method = "manhattan") # compute the manhattan distances

h.complete<-hclust(Viol.distances, method = "complete", members = NULL)
ViolatedCans$clusters<-cutree(h.complete, 6)

ggplot(data=ViolatedCans,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)))+
  geom_point(size=3)+theme_bw()

### -----------------------------------------------------------------------------
## TSP problem of each cluster Linkage
### -----------------------------------------------------------------------------


TSP.cluster<- function(Viol.coordenates, mergeData,i){
  cluster<-subset(Viol.coordenates,clusters==i)[,1:3]
  cluster<-rbind(cluster,c(-2018264.841,3717116.008,i))
  cluster<-rbind(cluster,c(-2012361.533,3716615.895,i))
  dist.cluster<-dist(cluster, method="manhattan")
  tsp.cluster<-TSP(dist.cluster)
  depo<-which(labels(tsp.cluster)==nrow(cluster))
  ATSP.cluster<-as.ATSP(tsp.cluster)
  ATSP.cluster[,depo]<-0
  ini.tour.cluster<-solve_TSP(ATSP.cluster,method="repetitive_nn")
  path <- cut_tour(ini.tour.cluster,depo, exclude_cut = FALSE)
  cluster.path<-cluster[path,]
  cluster.path$id<-as.numeric(row.names(cluster.path))
  return(cluster.path=cluster.path)##ordered points in each cluster with the best path
}


PATH<-TSP.cluster(ViolatedCans[,c(1,2,8)],i=1)
for( j in 2:6){
  PATH<-rbind(PATH,TSP.cluster(ViolatedCans[,c(1,2,8)],i=j)) 
}

ggplot(PATH,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)))+geom_point()+
  geom_path()+theme_bw()

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
ind.dist$id<-NonVio.cans$ind
colnames(ind.dist)<-c("ViolatedCansID","MinimumDistance","id_nonviol")

for(i in 1:nrow(NonVio.cans)){
  id<-ind.dist$ViolatedCansID[i]
  NonVio.cans$clusters[i]<-subset(ViolatedCans,ind==id)$clusters
  ind.dist$clusters[i]<-subset(ViolatedCans,ind==id)$clusters
}


ggplot(PATH,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters),label=id))+geom_point()+
  geom_path()+theme_bw()+geom_text(hjust=0, vjust=0)+
  geom_point(data=NonVio.cans,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)),size=5,shape="*")
  

### -----------------------------------------------------------------------------
## Calculating the distance that would increase going through the new point
### -----------------------------------------------------------------------------

distances<-function(indPA,indNO,PATH,NonVio.cans){
  newD1<-abs(PATH$xcoordinate[indPA-1]-NonVio.cans$xcoordinate[indNO])+
    abs(PATH$ycoordinate[indPA-1]-NonVio.cans$ycoordinate[indNO])
  newD2<-abs(PATH$xcoordinate[indPA]-NonVio.cans$xcoordinate[indNO])+
    abs(PATH$ycoordinate[indPA]-NonVio.cans$ycoordinate[indNO])
  D3<-abs(PATH$xcoordinate[indPA-1]-PATH$xcoordinate[indPA])+
    abs(PATH$ycoordinate[indPA-1]-PATH$ycoordinate[indPA])
  newdist<-newD1+newD2-D3
  return(newdist)
}

for(j in 1:nrow(ind.dist)){
  ind<-ind.dist$ViolatedCansID[j]
  indPA<-which(PATH$id==ind)
  indNO<-which(NonVio.cans$ind==ind.dist$id_nonviol[j])
  dist1<-Inf
  dist2<-Inf
  
  if(PATH$clusters[indPA-1]==PATH$clusters[indPA]){
    dist1<-distances(indPA=indPA,indNO = indNO,PATH,NonVio.cans)
  }
  if(PATH$clusters[indPA]==PATH$clusters[indPA+1]){
    dist2<-distances(indPA=(indPA+1),indNO = indNO,PATH,NonVio.cans)
  }
  if(dist1<=dist2){
    ind.dist$ExtraDistance[j]<-round(dist1,3)
    ind.dist$AfterCan[j]<-PATH$id[indPA-1]
  }
  if(dist2<=dist1){
    ind.dist$ExtraDistance[j]<-round(dist2,3)
    ind.dist$AfterCan[j]<-PATH$id[indPA]
  }
}


### -----------------------------------------------------------------------------
## Computing the probabilities of tomorrow above the capacity
### -----------------------------------------------------------------------------


State<-Xt[-indCans,]

for(j in 1: nrow(State)){
  ID<-State$ID[j]
  IDC<-which(Cans$ind==ID)
  State$Prob[j]<-ppois(1/State$Demand[j],Cans$lambdas[IDC])
}

### -----------------------------------------------------------------------------
## Looking if the extra distances and time is worth 
### -----------------------------------------------------------------------------

cost<-10 # cost of each extra meter we do
penalty<-100 # cost if we exceed the max cap of the can

decision<-c()

for(j in 1:nrow(State)){
  decision[j]<-ifelse((State$Prob[j]*penalty>=ind.dist$ExtraDistance[j]),1,0)
}

NewCans<-which(decision==1)

NewCans<-NonVio.cans[NewCans,]

### -----------------------------------------------------------------------------
## Final plot of the cans we need to visit
### -----------------------------------------------------------------------------

ggplot(PATH,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters),label=id))+geom_point()+
  geom_path()+theme_bw()+geom_text(hjust=0, vjust=0)+
  geom_point(data=NewCans,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)),size=5,shape="*")

