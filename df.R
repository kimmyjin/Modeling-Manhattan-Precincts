#csamp <- function(n,rad=1,centre=c(0,0)){ 
#  x0 <- centre[1] ; y0 <- centre[2] 
#  u <- 2*pi*runif(n) 
#  cbind(x=rad*cos(u)+x0, y=rad*sin(u)+y0) 
#} 
#plot(csamp(100),asp=1) 
Csamp <- function(n,rad=1,centre=c(0,0)){ 
  x0 <- centre[1] ; y0 <- centre[2] 
  u <- 2*pi*runif(n) 
  r <- sqrt(runif(n)) 
  cbind(x=rad*r*cos(u)+x0, y=rad*r*sin(u)+y0) 
} 
#plot(Csamp(1000),asp=1) 

samp1<-Csamp(1000,0.0065,c(-73.974309,40.770629))
samp2<-Csamp(1000,0.0063,c(-73.968815,40.778104))
samp3<-Csamp(1000,0.0063,c(-73.962721,40.785075))
samp4<-Csamp(1000,0.0063,c(-73.959117,40.790858))
<<<<<<< HEAD
samp5<-Csamp(600,0.0063,c(-73.956284,40.795244))
=======
samp5<-Csamp(1000,0.0063,c(-73.955855,40.795244))
>>>>>>> cf6ff3d6ea02253c3d413170342293a8a585eabb


sample_together <-data.frame(rbind(samp1,samp2,samp3,samp4,samp5))
df <- data.frame(address = rep("Central Park",4600),precinct = 22,sample_together)
