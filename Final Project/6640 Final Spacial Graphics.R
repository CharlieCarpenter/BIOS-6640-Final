######################
## SPATIAL GRAPHICS ##
######################
library(tidyverse)
## For district spacial graphics
library(RColorBrewer)
library(sp)
library(maptools) 
library(lattice)
library(latticeExtra) # For layer()
library(rgdal)
library(gridExtra)

dat <- read.csv("~/Documents/BIOS 6640/Final_Data.csv")
poly <- maptools::readShapePoly("~/Documents/BIOS 6640/Moz_admin2.shp", IDvar = "DISTCODE")

######################################
## Practicing to make sure it works ##
######################################
all5 <- dat %>% 
  filter(year==2016, Epiweek==1)

## Changing row names to match
rownames(all5) <- all5$DISTCODE

## making poly data to map
poly2 <- poly[poly$DISTCODE %in% all5$DISTCODE,]
poly.d <- SpatialPolygonsDataFrame(poly2, all5)

## Pretty. Lets make a lot of them
spplot(poly.d, "Incidence", main = "Incidence", sub = "2016 week 2")

d <- dat %>% filter(year==2016, Epiweek==1)
rownames(d) <- d$DISTCODE
p <- poly[poly$DISTCODE %in% d$DISTCODE,]
pd <- SpatialPolygonsDataFrame(p,d)

spplot(pd, "Incidence", main="Incidence per 1000", sub= "2016 Week 1")
spplot(pd, "raintot2", main="2 Week Rain Lag", sub= "2016 Week 1")
spplot(pd, "raintot4", main="4 Week Rain Lag", sub= "2016 Week 1")
spplot(pd, "raintot8", main="8 Week Rain Lag", sub= "2016 Week 1")
