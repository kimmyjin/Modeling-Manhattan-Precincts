library(dplyr)
c1 = c(-73.981419, 40.768517)
c2 = c(-73.957995, 40.800545)
c3 = c(-73.973021, 40.764427)
c4 = c(-73.949281, 40.796896)


x = seq(c1[1],c2[1], length.out = 50)
y = seq(c1[2],c2[2], length.out = 50)
cbound1 = cbind(x, y)
x = seq(c1[1],c2[1], length.out = 100)
y = seq(c1[2],c2[2], length.out = 100)
cbound2 = cbind(x, y)

th = (c3-c1)/20
theta1 = matrix(NA, nrow =50, ncol =2)
theta2 = matrix(NA, nrow =100, ncol =2)
theta1[,1] = as.numeric(paste(rep(th[1],50)))
theta1[,2] = as.numeric(paste(rep(th[2],50)))
theta2[,1] = as.numeric(paste(rep(th[1],100)))
theta2[,2] = as.numeric(paste(rep(th[2],100)))

cbound = cbound1 + theta1

original2 = cbound2
for (i in 2:17){
  temp = original2 + theta2*i
  cbound = rbind(cbound, temp)
}

x = seq(c3[1],c4[1], length.out = 200)
y = seq(c3[2],c4[2], length.out = 200)
cbound3 = cbind(x, y)
theta3 = matrix(NA, nrow = 200, ncol = 2)
theta3[,1] = as.numeric(paste(rep(th[1],200)))
theta3[,2] = as.numeric(paste(rep(th[2],200)))

nextbound = cbound3 + (-theta3)
cbound = rbind(cbound,nextbound, cbound3)


cb <-data.frame(address = rep("Central Park",2050),precinct = 22,cbound)