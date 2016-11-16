library(dplyr)
c1 = c(-73.981419, 40.768517)
c2 = c(-73.958244, 40.800686)
c3 = c(-73.973093, 40.764551)
c4 = c(-73.949232, 40.796885)

x = seq(c1[1],c2[1], length.out = 100)
y = seq(c1[2],c2[2], length.out = 100)
cbound = cbind(x, y)

th = (c3-c1)/22
theta = matrix(NA, nrow =100, ncol =2)
theta[,1] = as.numeric(paste(rep(th[1],100)))
theta[,2] = as.numeric(paste(rep(th[2],100)))

original = cbound
for (i in 1:19){
  temp = original + theta*i
  cbound = rbind(cbound, temp)
}

cb <-data.frame(address = rep("Central Park",2000),precinct = 22,cbound)
