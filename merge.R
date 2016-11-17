library(raster)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(magrittr)
library(tibble)

#load the parking violatoin data
load("/data/nyc_parking/NYParkingViolations.Rdata")

pluto = st_read("/data/nyc_parking/pluto_manhattan/MNMapPLUTO.shp") %>%
  select(Address, geometry)


pluto_xy = cbind(
  select(pluto, Address),
  st_centroid(pluto) %>% 
    unlist() %>% 
    matrix(ncol=2,byrow=TRUE)
) %>% 
  setNames(c("address","x","y")) %>%
  tbl_df()


## Merge data
valid_precincts = c(1, 5, 6, 7, 9, 10, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30, 32, 33, 34)

nyc_man = nyc %>%
  mutate(address = paste(House.Number, Street.Name)) %>%
  filter(Violation.Precinct %in% valid_precincts) %>%
  select(address, precinct = Violation.Precinct)

# Cleanup
nyc_man %<>% mutate(address = tolower(address)) #change address to lower case letters
pluto_xy %<>% mutate(address = tolower(address)) #change address to lower case letters

# Replace strings
# find the most occurance 10 street type
alt.data = read.csv(file="/data/nyc_parking/altnames.csv", na.strings=c("","NA"))
alt.data.stype = alt.data %>% 
  select(SType) %>% 
  table() %>%
  sort(., decreasing = TRUE) %>% 
  head(.,n = 10) %>% 
  names()
alt.data.st.rep = c("avenue", "street", "place","way","blvd","road","bridge","drive","court", "parkway")
match.name = data.frame(tolower(alt.data.stype), alt.data.st.rep, stringsAsFactors = FALSE)

# find the directions
alt.data.dir = alt.data %>% select(PDir) %>% unique() %>% na.omit()
alt.data.dir.rep = c("east", "west","south","north") 
dir.name = data.frame(tolower(alt.data.dir[[1]]),alt.data.dir.rep,stringsAsFactors = FALSE) 


#find the street numbers
num.rep=c(1:10)
num.add <- function(i){
  last_digit <- as.numeric(substring(i, nchar(i)))
  ending <- sapply(last_digit + 1, switch, 'th', 'st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th')
  second_last_digit <- as.numeric(substring(i, nchar(i) - 1, nchar(i) - 1))
  ending[second_last_digit == 1L] <- 'th'
  out <- paste(i, ending, sep = '')
  return(out)
}
num=sapply(num.rep,num.add)

# make the address format to be consistant through the nyc_man file
for (i in seq_len(dim(match.name)[1])){
  nyc_man$address = str_replace(nyc_man$address, paste0(match.name[i,1],"$"),match.name[i,2])
}

for ( k in seq_along(num.rep)){
  nyc_man$address = str_replace(nyc_man$address, paste0(" ",num[k], " "),paste0(" ",num.rep[k], " "))
}

for ( j in seq_len(dim(dir.name)[1])){
  nyc_man$address = str_replace(nyc_man$address, paste0(" ",dir.name[j,1], " "),paste0(" ",dir.name[j,2], " "))
  nyc_man$address = str_replace(nyc_man$address, paste0("^",dir.name[j,1], " "),paste0(dir.name[j,2], " "))
}
#nyc_man$address = gsub(x = nyc_man$address , pattern = " (\\d{0,})\\w{2} ", replace = " \\1 " )
# make the address format to be consistant through the pluto_xy file
pluto_xy$address = str_replace(pluto_xy$address, "bl$", "blvd")
nyc_man$address = str_replace(nyc_man$address, "bway", "broadway")

combined = inner_join(nyc_man, pluto_xy)


#creating points for central park
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
Mode = function(x) {
  unique.x = as.integer(unique(x))
  tbl = tabulate(match(x, unique.x))
  toString(unique.x[tbl==max(tbl)])
}

combined %<>% group_by(x,y) %>% 
  mutate(precinct=as.integer(Mode(precinct))) %>%
  na.omit() %>% 
  ungroup() %>%
  unique()

#adding the points of central park to the combined dataset
combined = rbind.data.frame(combined,cb)
#creating precinct.Rdata file
save(combined, file="precinct.Rdata")


  

library(devtools)
install_github("edzer/sfr")
library(raster) # load before dplyr to avoid select bs

library(dplyr)
library(ggplot2)
library(sf)

# New packages
library(nnet)
library(xgboost)

# Load data
load(file="precinct.Rdata")
ggplot(combined, aes(x=x,y=y,color=factor(precinct))) + geom_point()



## Get Manhattan Info
nybb = st_read("/data/nyc_parking/nybb/", quiet=TRUE)
manh = nybb %>% filter(BoroName == "Manhattan")
#plot(manh,axes=TRUE)

library(raster)
ext = st_bbox(manh) %>% .[c("xmin","xmax","ymin","ymax")] %>% extent()
r = raster(ext, ncol=800, nrow=2400)
r = rasterize(as(manh,"Spatial"),r)
plot(r)


### Get prediction locations
pred_cells = which(!is.na(r[]))
pred_locs = xyFromCell(r, pred_cells) %>% as_data_frame()
plot(pred_locs, pch=16, cex=0.1)



## Model 4 - xgboost
library(xgboost)
precincts = factor(combined$precinct) %>% levels()
y = (factor(combined$precinct) %>% as.integer()) - 1L
x = combined %>% select(x,y) %>% as.matrix()
m = xgboost(data=x, label=y, nthead=4, nround=100, objective="multi:softmax", num_class=length(precincts))

pred_xg = predict(m, newdata=as.matrix(pred_locs))
pred_xg = precincts[pred_xg+1]
ggplot(cbind(pred_locs, pred=pred_xg), aes(x=x,y=y,color=factor(pred))) + geom_point()


## Rasters -> Polygons
r_xg = r
r_xg[pred_cells] = as.numeric(pred_xg)
plot(r_xg)


## Polygonize
library(rgdal)
source("polygonizer.R")
p = polygonizer(r_xg)
p = st_transform(p, 4326)
plot(p)
st_write(p,"precincts.json", "data", driver="GeoJSON", quiet=TRUE)



