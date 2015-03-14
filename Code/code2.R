earth.dist <- function (long1, lat1, long2, lat2) 
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

ndata<-data
coordinates(ndata) <- c("xcoordinate", "ycoordinate")
proj4string(ndata) <- CRS("+init=epsg:28992")
ndata<-spTransform(ndata, CRS("+proj=longlat +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +ellps=GRS80 "))
ndata<-as.data.frame(ndata)
earth.dist(ndata[1,1],ndata[1,2],ndata[2,1],ndata[2,2])*1000


### -----------------------------------------------------------------------------
## Compute TSP
### -----------------------------------------------------------------------------
coordenates<-data[,1:2] ## take only the coord.
distances<-dist(coordenates, method = "manhattan") # compute the manhattan distances

