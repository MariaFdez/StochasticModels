ALL<-function(day,Xt,Cans){


ViolatedCans<-ifelse(Xt$Demand>=0.9,1,0)
indCans<-which(ViolatedCans==1)
ViolatedCans<-Cans[indCans,] ## table with the data of the violated cans (we have to do the clusters with this)


### -----------------------------------------------------------------------------
## Clusters to the violated cans with manhatann distances
### -----------------------------------------------------------------------------
Viol.distances<-dist(ViolatedCans[,1:2], method = "manhattan") # compute the manhattan distances

h.complete<-hclust(Viol.distances, method = "complete", members = NULL)
ViolatedCans$clusters<-cutree(h.complete, 6)


### -----------------------------------------------------------------------------
## TSP problem of each cluster Linkage
### -----------------------------------------------------------------------------


PATH<-TSP.cluster(ViolatedCans[,c("xcoordinate","ycoordinate","clusters")],i=1)$cluster.path
PATH$id[1]<-1000+1
for( j in 2:6){
  P1<-TSP.cluster(ViolatedCans[,c("xcoordinate","ycoordinate","clusters")],i=j)$cluster.path
  P1$id[1]<-1000+j
  PATH<-rbind(PATH,P1) 
}

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


### -----------------------------------------------------------------------------
## Calculating the distance that would increase going through the new point
### -----------------------------------------------------------------------------


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

return(list(FinalDistance=FinalDistance,
            VisitCans=VisitCans,Cans=Cans))

}

resultsd1<-ALL(1,Xt=Xt,Cans=Cans)


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
