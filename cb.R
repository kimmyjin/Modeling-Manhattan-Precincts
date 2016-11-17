library(dplyr)
c1 = c(-73.981419, 40.768517)
c2 = c(-73.957995, 40.800545)
c3 = c(-73.973021, 40.764427)
c4 = c(-73.949281, 40.796896)

x = seq(c1[1],c2[1], length.out = 80)
y = seq(c1[2],c2[2], length.out = 80)
cbound = cbind(x, y)

th = (c3-c1)/22
theta = matrix(NA, nrow =80, ncol =2)
theta[,1] = as.numeric(paste(rep(th[1],80)))
theta[,2] = as.numeric(paste(rep(th[2],80)))

original = cbound
for (i in 1:19){
  temp = original + theta*i
  cbound = rbind(cbound, temp)
}

cb <-data.frame(address = rep("Central Park",1600),precinct = 22,cbound)
