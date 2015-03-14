##-------------------------------------------------------------------------------------------------
##   Travel Salesmans Problem
##-------------------------------------------------------------------------------------------------

##DESCRIPTION:
# this code is going to apply TSV to the market data of the cans


## USEFULL LINKS:
## http://cran.r-project.org/web/packages/TSP/TSP.pdf
## http://cran.r-project.org/web/packages/TSP/vignettes/TSP.pdf


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
#smallroads<-read.table("smallroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
#smallroads$type<-rep("smallroads",nrow(smallroads))
#bigroads<-read.table("bigroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
#bigroads$type<-rep("bigroads",nrow(bigroads))

#data<-rbind(market,smallroads,bigroads)
data<-rbind(market,c(1,"468885.1451","4200354.6723",0,0,"depot"))
data<-rbind(data,c(1,"464323.2597","4204134.6673",0,0,"disposal"))
   

data$V2<-as.numeric(data$V2)
data$V3<-as.numeric(data$V3)
data$type<-as.factor(data$type)

ggplot(data=data,aes(x=V2,y=V3,col=type))+geom_point()+theme_bw()


### -----------------------------------------------------------------------------
## Compute TSP
### -----------------------------------------------------------------------------

coordenates<-data[,2:3] ## take only the coord.
distances<-dist(coordenates, method =  "manhattan") # compute the manhattan distances

##TSP
atsp<-as.ATSP(distances)
initial <-85
atsp[, initial] <- 0

initial_tour <- solve_TSP(atsp, method="nn")
tour <- solve_TSP(atsp, method ="2-opt", control = list(tour = initial_tour))
path <- cut_tour(tour, initial, exclude_cut = FALSE)


#Path<-solve_TSP(x=TSP, method="nearest_insertion",start=85)

ind<-as.numeric(labels(path)) ## new order 

plot_path<-coordenates[ind,] ## reorder the cans by the path chossen 

ggplot(data=data,aes(x=V2,y=V3))+geom_path(col="blue")+theme_bw()+geom_point(col="red",size=2)

