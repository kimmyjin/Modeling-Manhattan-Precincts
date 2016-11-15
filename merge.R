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

#ggplot(pluto_xy, aes(x=x,y=y)) + 
#  geom_point(alpha=0.1,size=0.1) +
#  theme_bw()


## Merge data

valid_precincts = c(1, 5, 6, 7, 9, 10, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30, 32, 33, 34)

nyc_man = nyc %>%
  mutate(address = paste(House.Number, Street.Name)) %>%
  filter(Violation.Precinct %in% valid_precincts) %>%
  select(address, precinct = Violation.Precinct)

# Cleanup

nyc_man %<>% mutate(address = tolower(address))
pluto_xy %<>% mutate(address = tolower(address))



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
#num.rep=c(1:130)
#num.add <- function(i){
#  last_digit <- as.numeric(substring(i, nchar(i)))
#  ending <- sapply(last_digit + 1, switch, 'th', 'st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th')
#  second_last_digit <- as.numeric(substring(i, nchar(i) - 1, nchar(i) - 1))
#  ending[second_last_digit == 1L] <- 'th'
#  out <- paste(i, ending, sep = '')
#  return(out)
#}
#num=sapply(num.rep,num.add)

# make the address format to be consistant through the nyc_man file
for (i in seq_len(dim(match.name)[1])){
  nyc_man$address = str_replace(nyc_man$address, paste0(match.name[i,1],"$"),match.name[i,2])
}

#for ( k in seq_along(num.rep)){
#  nyc_man$address = str_replace(nyc_man$address, paste0(" ",num[k], " "),paste0(" ",num.rep[k], " "))
#}

for ( j in seq_len(dim(dir.name)[1])){
  nyc_man$address = str_replace(nyc_man$address, paste0(" ",dir.name[j,1], " "),paste0(" ",dir.name[j,2], " "))
  nyc_man$address = str_replace(nyc_man$address, paste0("^",dir.name[j,1], " "),paste0(dir.name[j,2], " "))
}
nyc_man$address = gsub(x = nyc_man$address , pattern = " (\\d{0,})\\w{2} ", replace = " \\1 " )
# make the address format to be consistant through the pluto_xy file
pluto_xy$address = str_replace(pluto_xy$address, "bl$", "blvd")
nyc_man$address = str_replace(nyc_man$address, "bway", "broadway")

combined = inner_join(nyc_man, pluto_xy)

Mode = function(x) {
  unique.x = as.integer(unique(x))
  tbl = tabulate(match(x, unique.x))
  toString(unique.x[tbl==max(tbl)])
}
combined %<>% group_by(x,y) %>% mutate(precinct=as.integer(Mode(precinct))) %>% na.omit() %>% unique()
combined = rbind.data.frame(combined,df)
combined %<>% ungroup()
save(combined, file="precinct.Rdata")



# Combine data
#rep <- nyc_man %>% select(address) %>% table() %>% sort(., decreasing = TRUE) %>% head()
#ggplot(combined, aes(x=x,y=y,color=factor(precinct))) +
#  geom_point(size=0.1) +
#  theme_bw()

