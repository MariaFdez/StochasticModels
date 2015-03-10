#####################################################
library(dplyr) 
library(ggplot2)
library(ggmap)
library(timeDate)
library(grid)
library(gridExtra)
library(rgdal); library(sp);  library(raster)
#######################################################

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



data<-  ddply(data,c("xcoordinate","ycoordinate","type"),summarise,cap=sum(capacity),service=sum(time_service))

data<- rbind(data,c("468885.1451","4200354.6723","deposit_trucks",NA,NA))
data<-rbind(data,c("464323.2597","4204134.6673","disposal_rub",NA,NA))
data[,1]<-as.numeric(data[,1])
data[,2]<-as.numeric(data[,2])
data$type<-as.factor(data$type)

ggplot(data=data,aes(x=xcoordinate,y=ycoordinate,col=type))+geom_point()+theme_bw()


#write.table(newdata,"coordinates.csv")
