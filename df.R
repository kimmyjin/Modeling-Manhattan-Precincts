#csamp <- function(n,rad=1,centre=c(0,0)){ 
#  x0 <- centre[1] ; y0 <- centre[2] 
#  u <- 2*pi*runif(n) 
#  cbind(x=rad*cos(u)+x0, y=rad*sin(u)+y0) 
#} 
#plot(csamp(100),asp=1) 

lat_neg1 = seq(from = 40.764267, to = 40.7680695, length.out = 100)
long_neg1 = -2.3275476660038894 * lat_neg1 + 20.907746512209357
neg1 = cbind(long_neg1, lat_neg1)

lat_neg2 = seq(from = 40.7968955, to = 40.8005735, length.out = 100)
long_neg2 = -2.4181620445884118*lat_neg2 + 24.704230235139775
neg2 = cbind(long_neg2, lat_neg2)

lat_pos1 = seq(from = 40.7680695, to = 40.8005735, length.out = 100)
long_pos1 = 0.7294640659609856*lat_pos1 - 103.72072023885003
pos1 = cbind(long_pos1, lat_pos1)

lat_pos2 = seq(from = 40.764267, to = 40.7968955, length.out = 100)
long_pos2 = 0.7280138529198642*lat_pos2 - 103.64997908012407
pos2 = cbind(long_pos2, lat_pos2)

Csamp <- function(n,rad=1,centre=c(0,0)){ 
  x0 <- centre[1] ; y0 <- centre[2] 
  u <- 2*pi*runif(n) 
  r <- sqrt(runif(n)) 
  cbind(x=rad*r*cos(u)+x0, y=rad*r*sin(u)+y0) 
} 
#plot(Csamp(1000),asp=1) 

samp1<-Csamp(500,0.0065,c(-73.9748436,40.76957945))
samp2<-Csamp(500,0.0062,c(-73.9701388,40.77604835))
samp3<-Csamp(500,0.0061,c(-73.9654345,40.78251725))
samp4<-Csamp(500,0.0061,c(-73.9607292,40.78898615))
samp5<-Csamp(500,0.0062,c(-73.9560239,40.79545503))

sample_together <-data.frame(rbind(samp1,samp2,samp3,samp4,samp5, neg1, neg2, pos1, pos2))
df <- data.frame(address = rep("Central Park",2000),precinct = 22,sample_together)

