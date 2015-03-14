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


## GENERAL INFO:
# 
##-------------------------------------------------------------------------------------------------
##Marias directory
setwd("~/Documents/Box Sync/Current/Stochastics/DP")


## LIBRARIES

# Import packages and functions
##Packages that are used in this code

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggmap")) install.packages("ggmap")
if (!require("TSP")) install.packages("TSP")
if (!require("tspmeta")) install.packages("tspmeta")

##-------------------------------------------------------------------------------------------------
##INCLUDE DATA
rm(list=ls())

##libraries
#if (!require("foreach")) install.packages("foreach")
library(plyr) #merge tables
library(timeDate)
library(grid)
library(gridExtra)
##colours

#colours=c("#fdae61","#c51b7d","#bdbdbd","#636363")

options(digits=10) ## so the data is well imported
###READ DATA

market<-read.table("Data/cans.txt",colClasses=c("numeric","character","character","numeric","numeric"))
market$type<-rep("market",nrow(market))
#smallroads<-read.table("Data/smallroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
#smallroads$type<-rep("smallroads",nrow(smallroads))
#bigroads<-read.table("Data/bigroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
#bigroads$type<-rep("bigroads",nrow(bigroads))

#data<-rbind(market,smallroads,bigroads)
#data<-rbind(market,c(1,"468885.1451","4200354.6723",0,0,"depot"))
#data<-rbind(data,c(1,"464323.2597","4204134.6673",0,0,"disposal"))

##for now we are not going to include the disposal
data<-market
market$V3<-as.numeric(market$V3)

data$V2<-as.numeric(data$V2)
data$V3<-as.numeric(data$V3)
data$V4<-as.numeric(data$V4)
data$V5<-as.numeric(data$V5)
data$type<-as.factor(data$type)

colnames(data)<-c("ind","coorX","coorY","cap","time","type")

ggplot(data=data,aes(x=coorX,y=coorY,col=type))+geom_point()+theme_bw()

mergeData <- ddply(data, c("coorX","coorY","type"), summarise,
                  Cap = sum(cap),
                  Time=sum(time))
n<-nrow(mergeData)
mergeData$ind<-seq(1:n)

### -----------------------------------------------------------------------------
## Choose the first 40 cans locations
### -----------------------------------------------------------------------------
ind<-seq(1:n)
ind<-ind[sample(1:n)]

#select randomly the 40 first cans we are going to work with
Cans<-mergeData[ind[1:45],]
##plot of the selected cans
ggplot(data=Cans,aes(x=coorX,y=coorY,col=type))+geom_point()+theme_bw()

rm(data) ## clear the environment with data we are not going to use
rm(market)
rm(mergeData)
rm(ind)
### -----------------------------------------------------------------------------
## Now we have the selected cans, lets asign different lamdas to each can and
## compute the fillness in 10 days horizon
### -----------------------------------------------------------------------------

lambdas<-c(1,2,3,4,5)
lambdasList<-rep(lambdas,9)
lambdasList<-lambdasList[sample(1:45)] ## shuffle the assign lambdas in each can
Cans$lambdas<-lambdasList ## assing the lambdas to each can

### Create the matrix of demands following the expectation of each one
Demands<-c()
for( i in 1:nrow(Cans)){
  exp<-rpois(n=10, lambda=Cans$lambdas[i])+1 # +1 because if not it could be fill 
                                            # in 0 days and that was a problem
  expP<-round((1/exp),3)
  Demands<-rbind(Demands,expP)
}

rm(lambdas)
rm(lambdasList)
rm(exp)
rm(expP)
### -----------------------------------------------------------------------------
## Create the Xt of the first stage we are going to solve
### -----------------------------------------------------------------------------

Xt<-ifelse(Demands[,1]==1,0,Demands[,1])
Xt<-Xt+Demands[,2]

Xt ## demands of the first day of our 40 cans.

### -----------------------------------------------------------------------------
## Plotting the cans that exceed our 0.9 threeshold 
### -----------------------------------------------------------------------------

ViolatedCans<-ifelse(Xt>=0.9,1,0)
indCans<-which(ViolatedCans==1)

ViolatedCans<-Cans[indCans,] ## table with the data of the violated cans (we have to do the clusters with this)

ggplot(data=ViolatedCans,aes(x=coorX,y=coorY))+geom_point()+theme_bw()

### -----------------------------------------------------------------------------
## getting the manhatann distances of the violated Cans
### -----------------------------------------------------------------------------

Viol.coordenates<-ViolatedCans[,1:2] ## take only the coord.
Viol.distances<-dist(Viol.coordenates, method =  "manhattan") # compute the manhattan distances


### -----------------------------------------------------------------------------
## getting the vertical distance to the nearest violated can
### -----------------------------------------------------------------------------

NonVio.cans<-Cans[-indCans,]

## getting the nearest vertical can

vert.dist<-as.data.frame(matrix(0,nrow(NonVio.cans),nrow(ViolatedCans)))

for( i in 1:nrow(NonVio.cans)){
  vert.dist[i,]<-abs(ViolatedCans$coorY-NonVio.cans$coorY[i])
}

ind.dist<-as.data.frame(matrix(0,nrow(NonVio.cans),2))
for( i in 1:nrow(NonVio.cans)){
  ind<-which.min(vert.dist[i,])
  ind.dist[i,1]<-ViolatedCans$ind[ind]
  ind.dist[i,2]<-round(vert.dist[i,ind],4)
}

rm(Viol.coordenates)
rm(vert.dist)
rm(ind)
rm(indCans)
### -----------------------------------------------------------------------------
## Plotting the cans that exceed our 0.9 threeshold vs the not
### -----------------------------------------------------------------------------
yesno<-ifelse(Xt>=0.9,1,0)

VioPlot<-Cans
VioPlot$condition<-as.factor(yesno)
rm(yesno)


ggplot(data=VioPlot,aes(x=coorX,y=coorY,colour=condition,label=ind))+geom_point()+theme_bw()+
  geom_text(hjust=0, vjust=0)



