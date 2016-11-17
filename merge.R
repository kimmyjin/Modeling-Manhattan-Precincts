library(raster)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(magrittr)
library(tibble)

#load the parking violatoin data
load("/data/nyc_parking/NYParkingViolations.Rdata")

#read the ship-file 
pluto = st_read("/data/nyc_parking/pluto_manhattan/MNMapPLUTO.shp") %>%
  select(Address, geometry)    #select Address and geometry 

#geting the xy-coordinate of each polygon 
pluto_xy = cbind(
  select(pluto, Address),
  st_centroid(pluto) %>% 
    unlist() %>% 
    matrix(ncol=2,byrow=TRUE)
) %>% 
  setNames(c("address","x","y")) %>%
  tbl_df()


## Merge data
#creating a vector for the precincts in Manhattan 
valid_precincts = c(1, 5, 6, 7, 9, 10, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30, 32, 33, 34)

#getting the address and precinct of each parking violation in Manhattan
nyc_man = nyc %>%
  mutate(address = paste(House.Number, Street.Name)) %>%
  filter(Violation.Precinct %in% valid_precincts) %>%
  select(address, precinct = Violation.Precinct)

# Cleanup
#change address to lower case letters
nyc_man %<>% mutate(address = tolower(address)) 
#change address to lower case letters
pluto_xy %<>% mutate(address = tolower(address)) 



#reading the altnames file
altnames = read.csv(file="/data/nyc_parking/altnames.csv", na.strings=c("","NA"))
# finding 10 street types appearing with high frequency 
altnames.stype = altnames %>%  
  select(SType) %>% 
  table() %>%
  sort(., decreasing = TRUE) %>% 
  head(.,n = 10) %>% 
  names()
#creating a vector for these 10 street types with full name
full.name = c("avenue", "street", "place","way","blvd","road","bridge","drive","court", "parkway")
street.name = data.frame(tolower(altnames.stype), full.name, stringsAsFactors = FALSE)

# find the directions
altnames.dir = altnames %>% select(PDir) %>% unique() %>% na.omit()
full.dir = c("east", "west","south","north") 
dir.name = data.frame(tolower(altnames.dir[[1]]),full.dir,stringsAsFactors = FALSE) 


#find the street numbers
num.rep=c(1:10)
num.add <- function(i){                                                                                #convert cardinal number into ordinal
  last_digit <- as.numeric(substring(i, nchar(i)))
  ending <- sapply(last_digit + 1, switch, 'th', 'st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th') #suffix according to the last digit
  second_last_digit <- as.numeric(substring(i, nchar(i) - 1, nchar(i) - 1))                            
  ending[second_last_digit == 1L] <- 'th'                                                              #special case: tens digit=1
  out <- paste(i, ending, sep = '') 
  return(out)
}
num=sapply(num.rep,num.add)

# make the address format to be consistant through the nyc_man file
#replacing the abbreviated street name to full name
for (i in seq_len(dim(street.name)[1])){
  nyc_man$address = str_replace(nyc_man$address, paste0(street.name[i,1],"$"),street.name[i,2])
}

#replacing the first 10 street number
for (j in seq_along(num.rep)){
  nyc_man$address = str_replace(nyc_man$address, paste0(" ",num[j], " "),paste0(" ",num.rep[j], " "))
}

#replacing the abbreviated street direction to full direction
for (k in seq_len(dim(dir.name)[1])){
  nyc_man$address = str_replace(nyc_man$address, paste0(" ",dir.name[k,1], " "),paste0(" ",dir.name[k,2], " "))
  nyc_man$address = str_replace(nyc_man$address, paste0("^",dir.name[k,1], " "),paste0(dir.name[k,2], " "))
}

# make the address format to be consistant through the pluto_xy file
pluto_xy$address = str_replace(pluto_xy$address, "bl$", "blvd")
nyc_man$address = str_replace(nyc_man$address, "bway", "broadway")

#inner join the nyc_man and pluto_xy to keep the data appeared on both data set.
combined = inner_join(nyc_man, pluto_xy)


#creating points for central park
#obtain four boundary points of central park 
c1 = c(-73.981419, 40.768517)
c2 = c(-73.957995, 40.800545)
c3 = c(-73.973021, 40.764427)
c4 = c(-73.949281, 40.796896)

# generate 50 and 100 c1 and c2's x and y sequence respectively
x = seq(c1[1],c2[1], length.out = 100)
y = seq(c1[2],c2[2], length.out = 100)
# combine 50 sequence of x and y as cbound1 (the points on one side of the length)
cbound1 = cbind(x, y)
# combine 100 sequence of x and y as cbound2 (the points on one side of the length)
x = seq(c1[1],c2[1], length.out = 100)
y = seq(c1[2],c2[2], length.out = 100)
cbound2 = cbind(x, y)

# break the width into 20 pieces, th is the increasing value of x and y in each break
th = (c3-c1)/22
# creating the theta matrix respect to 50 and 100 rows
theta1 = matrix(NA, nrow =100, ncol =2)
theta2 = matrix(NA, nrow =100, ncol =2)
# paste th values(the increasing of x and y in each piece) in to the matrix of theta1 and theta2
theta1[,1] = as.numeric(paste(rep(th[1],100)))
theta1[,2] = as.numeric(paste(rep(th[2],100)))
theta2[,1] = as.numeric(paste(rep(th[1],100)))
theta2[,2] = as.numeric(paste(rep(th[2],100)))

#generate cbound as the first with one theta1 increasing(it is the first 50 points we generated in central park)
cbound = cbound1 + theta1

#continually generate 100 point each iteration with the increasing of theta2 (1600 points inside central park)
original2 = cbound2
for (i in 2:17){
  temp = original2 + theta2*i
  cbound = rbind(cbound, temp)
}

# combine 200 sequence of x and y as cbound3 (200 points on the other side of the length)
x = seq(c3[1],c4[1], length.out = 250)
y = seq(c3[2],c4[2], length.out = 250)
cbound3 = cbind(x, y)
# creating the theta matrix respect to 200 rows
theta3 = matrix(NA, nrow = 250, ncol = 2)
# paste th values(the increasing of x and y in each piece) in to the matrix of theta3
theta3[,1] = as.numeric(paste(rep(th[1],250)))
theta3[,2] = as.numeric(paste(rep(th[2],250)))

# generate the second last boundary points by decreasing theta3 as nextbound (200 pointa)
nextbound = cbound3 + (-theta3)

# combine all points we generated above as cbound
cbound = rbind(cbound,nextbound, cbound3)

# create a dataframe called cb which contains all the fake points we generated in central park 
cb <-data.frame(address = rep("Central Park",2200),precinct = 22,cbound)

#This function is used to get the mode of each address corresponding to precinct
Mode = function(x) {
  Unique = as.integer(unique(x))
  #tabulate takes the integer-valued vector bin and counts the number of times each integer occurs in it
  Tabulate = tabulate(match(x, Unique))
  #change numeric type to character type
  toString(Unique[Tabulate==max(Tabulate)])
}

#cleaning the repeated data by using the unique function
combined %<>% group_by(x,y) %>% 
  mutate(precinct=as.integer(Mode(precinct))) %>%
  na.omit() %>% 
  ungroup() %>%
  unique()

#adding the points of central park to the combined dataset
combined = rbind.data.frame(combined,cb)
#creating precinct.Rdata file
save(combined, file="precinct.Rdata")

