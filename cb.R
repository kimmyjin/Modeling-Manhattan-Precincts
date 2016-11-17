library(dplyr)
c1 = c(-73.981419, 40.768517)
c2 = c(-73.957995, 40.800545)
c3 = c(-73.973021, 40.764427)
c4 = c(-73.949281, 40.796896)

th = (c3-c1)/22
x = seq(c1[1],c2[1], length.out = 50)
y = seq(c1[2],c2[2], length.out = 50)
cbound1 = cbind(x, y)
x = seq(c1[1],c2[1], length.out = 100)
y = seq(c1[2],c2[2], length.out = 100)
cbound2 = cbind(x, y)
x = seq(c1[1],c2[1], length.out = 200)
y = seq(c1[2],c2[2], length.out = 200)
cbound3 = cbind(x, y)

th = (c3-c1)/22
theta1 = matrix(NA, nrow =50, ncol =2)
theta2 = matrix(NA, nrow =100, ncol =2)
theta3 = matrix(NA, nrow =200, ncol =2)
theta1[,1] = as.numeric(paste(rep(th[1],50)))
theta1[,2] = as.numeric(paste(rep(th[2],50)))
theta2[,1] = as.numeric(paste(rep(th[1],100)))
theta2[,2] = as.numeric(paste(rep(th[2],100)))
theta3[,1] = as.numeric(paste(rep(th[1],200)))
theta3[,2] = as.numeric(paste(rep(th[2],200)))

cbound = cbound1 + theta1
original2 = cbound2
original3 = cbound3
for (i in 2:19){
  if(i<=17){
  temp = original2 + theta2*i
  cbound = rbind(cbound, temp)
  }
  else{
  temp = original3 + theta3*i
  cbound = rbind(cbound, temp)
  }
}


cb <-data.frame(address = rep("Central Park",2050),precinct = 22,cbound)

