library(raster)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(magrittr)
library(tibble)

load("/data/nyc_parking/NYParkingViolations.Rdata")
altnames <- read.csv("/data/nyc_parking/altnames.csv")

## Get pluto data

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

ggplot(pluto_xy, aes(x=x,y=y)) + 
  geom_point(alpha=0.1,size=0.1) +
  theme_bw()


## Merge data

valid_precincts = c(1, 5, 6, 7, 9, 10, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30, 32, 33, 34)

nyc_man = nyc %>%
  mutate(address = paste(House.Number, Street.Name)) %>%
  filter(Violation.Precinct %in% valid_precincts) %>%
  select(address, precinct = Violation.Precinct)

# Cleanup

nyc_man %<>% mutate(address = tolower(address))
pluto_xy %<>% mutate(address = tolower(address))

xy = pluto_xy %>% 
  filter(address != "")

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
num.rep = c(1:10)
num = c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")

# make the address format to be consistant through the nyc_man file
for (i in seq_len(dim(match.name)[1])){
  nyc_man$address = str_replace(nyc_man$address, paste0(match.name[i,1],"$"),match.name[i,2])
}
for ( k in seq_along(num.rep)){
  nyc_man$address = str_replace(nyc_man$address, paste0(" ",num[k], " "),paste0(" ",num.rep[k], " "))
}
for ( j in seq_len(dim(dir.name)[1])){
  nyc_man$address = str_replace(nyc_man$address, paste0(" ",dir.name[j,1], " "),paste0(" ",dir.name[j,2], " "))
  nyc_man$address = str_replace(nyc_man$address, paste0("^",dir.name[j,1], " "),paste0(" ",dir.name[j,2], " "))
}
# make the address format to be consistant through the pluto_xy file
pluto_xy$address = str_replace(pluto_xy$address, "bl$", "blvd")
nyc_man$address = str_replace(nyc_man$address, "bway", "broadway")


# Combine data
rep <- nyc_man %>% select(address) %>% table() %>% sort(., decreasing = TRUE) %>% head()
combined = inner_join(nyc_man, pluto_xy)

ggplot(combined, aes(x=x,y=y,color=factor(precinct))) + 
  geom_point(size=0.1) +
  theme_bw()

save(combined, file="precinct.Rdata")

#regst = "(\\sstreet)"
#
#xy$address=str_replace(xy$address, "street", "st")
#nyc_man$address = str_replace(nyc_man$address, "street", "st")
#xy$address=str_replace(xy$address, "avenue", "ave")
#nyc_man$address = str_replace(nyc_man$address, "avenue", "ave")
#xy$address=str_replace(xy$address, "street", "st")
#nyc_man$address = str_replace(nyc_man$address, "street", "st")
#xy$address=str_replace(xy$address, "boulevard", "blvd")
#nyc_man$address = str_replace(nyc_man$address, "boulevard", "blvd")
#xy$address=str_replace(xy$address, "drive", "dr")
#nyc_man$address = str_replace(nyc_man$address, "drive", "dr")
#
#
#nyc_man$address = str_replace(nyc_man$address, "first", "1")
#nyc_man$address = str_replace(nyc_man$address, "second", "2")
#nyc_man$address = str_replace(nyc_man$address, "third", "3")
#nyc_man$address = str_replace(nyc_man$address, "fourth", "4")
#nyc_man$address = str_replace(nyc_man$address, "fifth", "5")
#
##detect 
##head(nyc_man$address[str_detect(nyc_man$address, "(\\d+\\w*(st|nd|rd|th))")])
#
#combined = inner_join(nyc_man, xy)
##notcombined = anti_join(nyc_man, pluto_xy)
#
#ggplot(combined, aes(x=x,y=y,color=factor(precinct))) + 
#  geom_point(size=0.1) +
#  theme_bw()

save(combined, file="precinct.Rdata")
