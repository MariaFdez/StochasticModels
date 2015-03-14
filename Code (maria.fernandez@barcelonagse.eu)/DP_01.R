##-------------------------------------------------------------------------------------------------
##   Getting Ready all the Demands
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to prepare the data for the DP.


## USEFULL LINKS:
## 


## GENERAL INFO:
# 
##-------------------------------------------------------------------------------------------------
##Marias directory
setwd("~/Documents/Box Sync/Current/Stochastics/StochasticModels")


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

colours=c("#fdae61","#c51b7d","#bdbdbd","#636363")

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
mergeData$ind<-seq(1:n)

### -----------------------------------------------------------------------------
## Choose the first 10 cans locations
### -----------------------------------------------------------------------------
n<-nrow(mergeData)
ind<-seq(1:n)
ind<-ind[sample(1:n)]

#select randomly the 10 first cans we are going to work with
Cans<-mergeData[ind[1:10],]
##plot of the selected cans
ggplot(data=Cans,aes(x=coorX,y=coorY,col=type))+geom_point()+theme_bw()

### -----------------------------------------------------------------------------
## Now we have the selected cans, lets asign different lamdas to each can and
## compute the fillness in 10 days horizon
### -----------------------------------------------------------------------------

lambdas<-c(1,2,3,4,5)
lambdasList<-rep(lambdas,2)
lambdasList<-lambdasList[sample(1:10)] ## shuffle the assign lambdas in each can
Cans$lambdas<-lambdasList ## assing the lambdas to each can

### Create the matrix of demands following the expectation of each one
Demands<-c()
for( i in 1:nrow(Cans)){
  exp<-rpois(n=10, lambda=Cans$lambdas[i])+1 # +1 because if not it could be fill 
                                            # in 0 days and that was a problem
  expP<-round((1/exp),3)
  Demands<-rbind(Demands,expP)
}

### -----------------------------------------------------------------------------
## See which day is above the threshold
### -----------------------------------------------------------------------------

t<-1 ## current stage
which(Demands[,t]>=0.9)


