library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(magrittr)
library(stringr)
library(tibble)

load("/data/nyc_parking/NYParkingViolations.Rdata")

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

#regst = "(\\sstreet)"

xy$address=str_replace(xy$address, "street", "st")
nyc_man$address = str_replace(nyc_man$address, "street", "st")
xy$address=str_replace(xy$address, "avenue", "ave")
nyc_man$address = str_replace(nyc_man$address, "avenue", "ave")
xy$address=str_replace(xy$address, "street", "st")
nyc_man$address = str_replace(nyc_man$address, "street", "st")
xy$address=str_replace(xy$address, "boulevard", "blvd")
nyc_man$address = str_replace(nyc_man$address, "boulevard", "blvd")
xy$address=str_replace(xy$address, "drive", "dr")
nyc_man$address = str_replace(nyc_man$address, "drive", "dr")

nyc_man$address = str_replace(nyc_man$address, "first", "1")
nyc_man$address = str_replace(nyc_man$address, "second", "2")
nyc_man$address = str_replace(nyc_man$address, "third", "3")
nyc_man$address = str_replace(nyc_man$address, "fourth", "4")
nyc_man$address = str_replace(nyc_man$address, "fifth", "5")

#detect 
#head(nyc_man$address[str_detect(nyc_man$address, "(\\d+\\w*(st|nd|rd|th))")])

combined = inner_join(nyc_man, xy)
#notcombined = anti_join(nyc_man, pluto_xy)

ggplot(combined, aes(x=x,y=y,color=factor(precinct))) + 
  geom_point(size=0.1) +
  theme_bw()

save(combined, file="precinct.Rdata")
