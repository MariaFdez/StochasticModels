
##libraries
#if (!require("foreach")) install.packages("foreach")
library(plyr) #merge tables
library(ggplot2)
library(ggmap)
library(timeDate)
library(grid)
library(gridExtra)
##colours

colours=c("#fdae61","#c51b7d","#bdbdbd","#636363")

options(digits=10)
###READ DATA

market<-read.table("cans.txt",colClasses=c("numeric","character","character","numeric","numeric"))
market$type<-rep("market",nrow(market))
smallroads<-read.table("smallroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
smallroads$type<-rep("smallroads",nrow(smallroads))
bigroads<-read.table("bigroads.txt",colClasses=c("numeric","character","character","numeric","numeric"))
bigroads$type<-rep("bigroads",nrow(bigroads))

data<-rbind(market,smallroads,bigroads)
data<-rbind(data,c(1,"468885.1451","4200354.6723","N","N","depot"))
data<-rbind(data,c(1,"464323.2597","4204134.6673","N","N","disposal"))
   

data$V2<-as.numeric(data$V2)
data$V3<-as.numeric(data$V3)
data$type<-as.factor(data$type)

ggplot(data=data,aes(x=V2,y=V3,col=type))+geom_point()+theme_bw()
