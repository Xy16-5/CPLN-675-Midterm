#----------------------
# Packages
library(tidyverse)
library(sf)
library(RSocrata)
library(viridis)
library(caret)
library(spatstat)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(plotROC)
library(pROC)
library(raster)
library(maptools)


#-------------------
# Fishnet Grids
## Calgary Fishnet
c_boundary <- 
  st_read("calgary_boundary/CALGIS_CITYBOUND_LIMIT.shp") %>%
  st_transform(crs=3780)

c_fishnet <- 
  st_make_grid(c_boundary, cellsize = 500) %>%
  st_sf() %>%
  st_transform(crs=3780)

c_fishnet <- 
  c_fishnet[c_boundary,] %>%
  mutate(uniqueID = rownames(.)) %>%
  dplyr::select(uniqueID)

ggplot(data=c_fishnet)+
  geom_sf()

## Denver Fishnet
d_boundary <- 
  st_read("denver_boundary/county_boundary.shp") %>%
  st_transform(crs=6428) %>%
  dplyr::filter(COUNTY=="Denver")

d_fishnet <- 
  st_make_grid(d_boundary, cellsize = 500) %>%
  st_sf()

d_fishnet <- 
  d_fishnet[d_boundary,] %>%
  mutate(uniqueID = rownames(.)) %>%
  dplyr::select(uniqueID)

ggplot(data=d_fishnet)+
  geom_sf()

#-----------------------
# Land USe
c_landcover<-st_read('Citywide Land Cover/Citywide Land Cover.shp')%>%
  st_transform(crs= 3780)
d_landcover<-st_read('Denverdc/Denverdc.shp')%>%
  st_transform(crs= 6428)

## Calgary
c_landcover_net<-
  c_landcover %>% 
  dplyr::select(resist)%>%
  aggregate(.,c_fishnet,mean) 

ggplot()+
  geom_sf(data=c_landcover_net,aes(fill=resist),color='transparent')+
  scale_fill_continuous(type = "viridis")+
  labs(title='Permeability of Calgary')

## Denver
d_landcover$resist<-ifelse(d_landcover$gridcode==1|d_landcover$gridcode == 2,1,0)
d_landcover$resist<-ifelse(d_landcover$gridcode==3|d_landcover$gridcode == 4,0.05,d_landcover$resist)
d_landcover$resist<-ifelse(d_landcover$gridcode==5|d_landcover$gridcode == 8,0.25,d_landcover$resist)
d_landcover$resist<-ifelse(d_landcover$gridcode==6|d_landcover$gridcode == 7,0.5,d_landcover$resist)
  

d_landcover_net<-dplyr::select(d_landcover,resist)
d_landcover_net<- aggregate(d_landcover_net,d_fishnet,mean)

#d_landcover_net%>%sf::st_write ('d_landcover_net2.shp')
#test<-st_read('d_landcover_net/d_landcover_net.shp')

ggplot()+
  geom_sf(data=d_landcover_net,aes(fill=resist),color='transparent')+
  scale_fill_continuous(type = "viridis")+
  labs(title='Permeability of Denver')


