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

coloursYN<-c("#d73027","#1a9850")

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

lambdas<-c(1,3,5,7,9,11)
lambdasList<-rep(lambdas,101)
lambdasList<-lambdasList[sample(1:nrow(Cans))] ## shuffle the assign lambdas in each can

Cans$lambdas<-lambdasList ## assing the lambdas to each can
Demands<-c()
for( i in 1:nrow(Cans)){
  exp<-rpois(n=12, lambda=Cans$lambdas[i])+1 # +1 because if not it could be fill
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
ggplot(data=VioPlot,aes(x=xcoordinate,y=ycoordinate,colour=condition))+
  theme(legend.position="none",panel.background = element_rect(fill = "white"))+
  geom_point()+scale_colour_manual(name="condition",values=coloursYN)

### -----------------------------------------------------------------------------
## Clusters to the violated cans with manhatann distances
### -----------------------------------------------------------------------------
Viol.distances<-dist(ViolatedCans[,1:2], method = "manhattan") # compute the manhattan distances

h.complete<-hclust(Viol.distances, method = "complete", members = NULL)
ViolatedCans$clusters<-cutree(h.complete, 6)

ggplot()+
  geom_point(data=ViolatedCans,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)),size=3)+
  theme(legend.position="none",panel.background = element_rect(fill = "white"))+
  geom_point(data=subset(VioPlot,condition==0),aes(x=xcoordinate,y=ycoordinate),size=6,shape="*",col="grey")

### -----------------------------------------------------------------------------
## TSP problem of each cluster Linkage
### -----------------------------------------------------------------------------


TSP.cluster<- function(Viol.coordenates, mergeData,i){
  cluster<-subset(Viol.coordenates,clusters==i)[,1:3]
  #cluster<-rbind(cluster,c(-2018264.841,3717116.008,i))
  cluster<-rbind(cluster,c(-2012361.533,3716615.895,i))
  dist.cluster<-dist(cluster, method="manhattan")
  tsp.cluster<-TSP(dist.cluster)
  depo<-which(labels(tsp.cluster)==nrow(cluster))
  ATSP.cluster<-as.ATSP(tsp.cluster)
  ATSP.cluster[,depo]<-0
  ini.tour.cluster<-solve_TSP(ATSP.cluster,method="cheapest_insertion")
  tour<-solve_TSP(ATSP.cluster,method="2-opt",control=list(tour=ini.tour.cluster))
  path <- cut_tour(tour,depo, exclude_cut = FALSE)
  leng<-attr(tour,"tour_length")
  cluster.path<-cluster[path,]
  cluster.path$id<-as.numeric(row.names(cluster.path))
  return(list(cluster.path=cluster.path,leng=leng))##ordered points in each cluster with the best path
}


PATH<-TSP.cluster(ViolatedCans[,c("xcoordinate","ycoordinate","clusters")],i=1)$cluster.path
PATH$id[1]<-1000+1
for( j in 2:6){
  P1<-TSP.cluster(ViolatedCans[,c("xcoordinate","ycoordinate","clusters")],i=j)$cluster.path
  P1$id[1]<-1000+j
  PATH<-rbind(PATH,P1) 
}

ggplot(PATH,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)))+geom_point()+
  geom_path()+theme_bw()

ggplot()+
  geom_point(data=PATH,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)),size=3)+
  geom_path(data=PATH,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)))+
  theme(legend.position="none",panel.background = element_rect(fill = "white"))+
  geom_point(data=subset(VioPlot,condition==0),aes(x=xcoordinate,y=ycoordinate),size=5,shape="*",col="grey")



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
}

NonVio.cans$MinimumDistance<-ind.dist$MinimumDistance
NonVio.cans$ViolatedID<-ind.dist$ViolatedCansID


ggplot(PATH,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)))+geom_point(size=3)+
  geom_path()+theme(legend.position="none",panel.background = element_rect(fill = "white"))+
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

for(j in 1:nrow(NonVio.cans)){
  ind<-NonVio.cans$ViolatedID[j]
  indPA<-which(PATH$id==ind)
  dist1<-Inf
  dist2<-Inf
  
  if(indPA!=1){
  if(PATH$clusters[indPA-1]==PATH$clusters[indPA]){
    dist1<-distances(indPA=indPA,indNO = j,PATH,NonVio.cans)
  }
  }
  if(indPA!=nrow(PATH)){
  if(PATH$clusters[indPA]==PATH$clusters[indPA+1]){
    dist2<-distances(indPA=(indPA+1),indNO = j,PATH,NonVio.cans)
  }
  }
  if(dist1<=dist2){
    NonVio.cans$ExtraDistance[j]<-round(dist1,3)
    NonVio.cans$AfterCan[j]<-PATH$id[indPA-1]
  }
  if(dist2<=dist1){
    NonVio.cans$ExtraDistance[j]<-round(dist2,3)
    NonVio.cans$AfterCan[j]<-PATH$id[indPA]
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

time_cost <- 1.5 # euros per hour
dist_cost<-2.5 # cost of each extra meter we do
penalty<-1 # cost if we exceed the max cap of the can

decision<-c()
cost<-c()
for(j in 1:nrow(State)){
  cost[j]<-(NonVio.cans$ExtraDistance[j]/1000)*dist_cost+
    (((NonVio.cans$service[j]/3600)+(NonVio.cans$ExtraDistance[j]/12000))*time_cost)
  decision[j]<-ifelse((State$Prob[j]*penalty>=cost[j]),1,0)
}

NewCans<-which(decision==1)
NeverCans<-NonVio.cans[-NewCans,]
NewCans<-NonVio.cans[NewCans,]


Cans$U<-rep(0,nrow(Cans))

for(j in 1:nrow(ViolatedCans)){
  id<-ViolatedCans$ind[j]
  it<-which(Cans$ind==id )
  Cans$U[it]<-1
}

for(j in 1:nrow(NewCans)){
  id<-NewCans$ind[j]
  it<-which(Cans$ind==id )
  Cans$U[it]<-1
}
### -----------------------------------------------------------------------------
## Final plot of the cans we need to visit
### -----------------------------------------------------------------------------

ggplot(PATH,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)))+geom_point(size=3)+
  geom_path()+theme(legend.position="none",panel.background = element_rect(fill = "white"))+
  geom_point(data=NewCans,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)),size=5,shape="*")

### -----------------------------------------------------------------------------
## Create the Xt of the first stage we are going to solve
### -----------------------------------------------------------------------------
st<-resultsd2$Cans$U
Xt2<-c()


for( i in 1:length(st)){
  if(st[i]==1){
    Xt2[i]<-0
  }
  if(st[i]==0){
    Xt2[i]<-Xt$Demand[i]
  }
}
Xt2<-as.numeric(Xt2)+Demands[,3]

Xt<-Xt2
Xt<-as.data.frame(cbind(Xt,Cans$ind))
colnames(Xt)<-c("Demand","ID")

### -----------------------------------------------------------------------------
## Final Cans Table
### -----------------------------------------------------------------------------
VisitCans<-NewCans[,c("xcoordinate","ycoordinate","service","ind","clusters")]

VisitCans<-rbind(VisitCans,ViolatedCans[,c("xcoordinate","ycoordinate","service","ind","clusters")])

FinalDistance<-c()

NEWPATH<-TSP.cluster(VisitCans[,c("xcoordinate","ycoordinate","clusters")],i=1)$cluster.path
NEWPATH$id[1]<-1000+1
FinalDistance[1]<-TSP.cluster(VisitCans[,c("xcoordinate","ycoordinate","clusters")],i=1)$len
for( j in 2:6){
  P1<-TSP.cluster(VisitCans[,c("xcoordinate","ycoordinate","clusters")],i=j)$cluster.path
  P1$id[1]<-1000+j
  NEWPATH<-rbind(NEWPATH,P1) 
  FinalDistance[j]<-TSP.cluster(VisitCans[,c("xcoordinate","ycoordinate","clusters")],i=j)$len
  
}

ggplot(NEWPATH,aes(x=xcoordinate,y=ycoordinate,col=as.factor(clusters)))+geom_point()+
  geom_path()+theme(legend.position="none",panel.background = element_rect(fill = "white"))+
  geom_point(data=NeverCans,aes(x=xcoordinate,y=ycoordinate),size=5,shape="*", col="grey")


### -----------------------------------------------------------------------------
## COST of period
### -----------------------------------------------------------------------------

time_cost <- 1.5 # euros per hour
dist_cost<-2.5 # cost of each extra meter we do
penalty<-1 # cost if we exceed the max cap of the can

COST<-c()
##DP
resultsd2<-ALL(1,Xt=Xt,Cans=Cans)

costDistance<-sum(resultsd1$FinalDistance)/1000*dist_cost
costTime<-sum(resultsd1$FinalDistance)/12000*time_cost
costService<-sum(resultsd1$VisitCans$service)/3600*time_cost
#costPenalty<-sum(Xt$Demand>=1)*penalty
COST[1]<-costDistance+costTime+costService

for(day in 3:10){
  st<-resultsd2$Cans$U
  Xt2<-c()
  
  
  for( i in 1:length(st)){
    if(st[i]==1){
      Xt2[i]<-0
    }
    if(st[i]==0){
      Xt2[i]<-Xt$Demand[i]
    }
  }
  Xt2<-as.numeric(Xt2)+Demands[,day]
  
  Xt<-Xt2
  Xt<-as.data.frame(cbind(Xt,Cans$ind))
  colnames(Xt)<-c("Demand","ID")
  
  resultsd2<-ALL(4,Xt=Xt,Cans=Cans)
  
  
  costDistance<-sum(resultsd2$FinalDistance)/1000*dist_cost
  costTime<-sum(resultsd2$FinalDistance)/12000*time_cost
  costService<-sum(resultsd2$VisitCans$service)/3600*time_cost
  #costPenalty<-sum(Xt$Demand>=1)*penalty
  COST[day-2]<-costDistance+costTime+costService
  
}

resultsd1<-ALL(1,Xt=Xt,Cans=Cans)
resultsd2<-ALL(4,Xt=Xt,Cans=Cans)









### -----------------------------------------------------------------------------
## COST of bad way
### -----------------------------------------------------------------------------


Cans.distances<-dist(Cans[,1:2], method = "manhattan") # compute the manhattan distances

h.Cans<-hclust(Cans.distances, method = "complete", members = NULL)
Cans$Fixedclusters<-cutree(h.Cans, 6)

ggplot(data=Cans,aes(x=xcoordinate,y=ycoordinate,col=as.factor(Fixedclusters)))+
  geom_point(size=3)+theme_bw()

TSP.Fixed.cluster<- function(Viol.coordenates, mergeData,i){
  cluster<-subset(Viol.coordenates,Fixedclusters==i)[,1:3]
  #cluster<-rbind(cluster,c(-2018264.841,3717116.008,i))
  cluster<-rbind(cluster,c(-2012361.533,3716615.895,i))
  dist.cluster<-dist(cluster, method="manhattan")
  tsp.cluster<-TSP(dist.cluster)
  depo<-which(labels(tsp.cluster)==nrow(cluster))
  ATSP.cluster<-as.ATSP(tsp.cluster)
  ATSP.cluster[,depo]<-0
  ini.tour.cluster<-solve_TSP(ATSP.cluster,method="cheapest_insertion")
  tour<-solve_TSP(ATSP.cluster,method="2-opt",control=list(tour=ini.tour.cluster))
  path <- cut_tour(tour,depo, exclude_cut = FALSE)
  leng<-attr(tour,"tour_length")
  cluster.path<-cluster[path,]
  cluster.path$id<-as.numeric(row.names(cluster.path))
  return(list(cluster.path=cluster.path,leng=leng))##ordered points in each cluster with the best path
}


FixedDistance<-c()


FixedPATH<-TSP.Fixed.cluster(Cans[,c("xcoordinate","ycoordinate","Fixedclusters")],i=1)$cluster.path
FixedPATH$id[1]<-1000+1
FixedDistance[1]<-TSP.Fixed.cluster(Cans[,c("xcoordinate","ycoordinate","Fixedclusters")],i=1)$len
for( j in 2:6){
  P1<-TSP.Fixed.cluster(Cans[,c("xcoordinate","ycoordinate","Fixedclusters")],i=j)$cluster.path
  P1$id[1]<-1000+j
  FixedPATH<-rbind(FixedPATH,P1) 
  FixedDistance[j]<-TSP.Fixed.cluster(Cans[,c("xcoordinate","ycoordinate","Fixedclusters")],i=j)$len
  
}

FcostDistance<-sum(FixedDistance)/1000*dist_cost
FcostTime<-sum(FixedDistance)/12000*time_cost
FcostService<-sum(Cans$service)/3600*time_cost
FcostPenalty<-sum(Xt$Demand>=1)*penalty
FCost<-FcostDistance+FcostTime+FcostService


