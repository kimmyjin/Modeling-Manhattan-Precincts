csamp <- function(n=1000,rad=0.00678,centre=c(-73.974309,40.770629)){ 
  x0 <- centre[1] ; y0 <- centre[2] 
  u <- 2*pi*runif(n) 
  cbind(x=rad*cos(u)+x0, y=rad*sin(u)+y0) 
} 

samp1<-csamp(1500,0.00678,c(-73.974309,40.770629))
samp2<-csamp(1500,0.00678,c(-73.968815,40.778104))
samp3<-csamp(1500,0.00678,c(-73.962721,40.785075))
samp4<-csamp(1500,0.00678,c(-73.959117,40.790858))
samp5<-csamp(1000,0.00678,c(-73.955855, 40.795244))

sample_together <-data.frame(rbind(samp1,samp2,samp3,samp4,samp5))
df <- data.frame(address = rep("Central Park",7000),precinct = as.integer(22),sample_together)
