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

lat_pos1 = seq(from = 40.7680695, to = 40.8005735, length.out = 200)
long_pos1 = 0.7294640659609856*lat_pos1 - 103.72072023885003
pos1 = cbind(long_pos1, lat_pos1)

lat_pos2 = seq(from = 40.764267, to = 40.7968955, length.out = 200)
long_pos2 = 0.7280138529198642*lat_pos2 - 103.64997908012407
pos2 = cbind(long_pos2, lat_pos2)

Csamp <- function(n,rad=1,centre=c(0,0)){ 
  x0 <- centre[1] ; y0 <- centre[2] 
  u <- 2*pi*runif(n) 
  r <- sqrt(runif(n)) 
  cbind(x=rad*r*cos(u)+x0, y=rad*r*sin(u)+y0) 
} 
#plot(Csamp(1000),asp=1) 

samp1<-Csamp(200,0.0063,c(-73.974309,40.770629))
samp2<-Csamp(200,0.0063,c(-73.968815,40.778104))
samp3<-Csamp(200,0.0062,c(-73.962721,40.785075))
samp4<-Csamp(200,0.0061,c(-73.959117,40.790858))
samp5<-Csamp(200,0.0062,c(-73.956284,40.795244))

sample_together <-data.frame(rbind(samp1,samp2,samp3,samp4,samp5, neg1, neg2, pos1, pos2))
df <- data.frame(address = rep("Central Park",1600),precinct = 22,sample_together)

