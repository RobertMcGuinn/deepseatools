##### Header #####
## filename: 20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn. R
## author: Robert McGuinn
## date started: 20230404
## project log: https://docs.google.com/document/d/1dT_DYHmX2hfFfxQLCT8zUv0umIfd_MPTQX5iJ96whiE/edit?usp=sharing

##### packages #####
library(tidyverse)
library(openxlsx)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

##### load data #####
path <- "C:/rworking/deepseatools/indata/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/20230404-0_AleutianRecords-ForMap_THourigan.xlsx"
mapdata <- read.xlsx(path)

path2 <- "C:/rworking/deepseatools/indata/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/20230404-0_AleutianSurveysTaxonCategories_THourigan.xlsx"
com <- read.xlsx(path2)

##### check #####
dim(mapdata)
names(mapdata)
dim(com)
names(com)

##### static locator map creation #####
## filter out -999
sub2 <- mapdata %>%
  filter(Latitude != '-999' , Longitude != '-999')

## set projection of incoming data
projcrs <- 4326

## targetCRS
domainCRS <- 6393

## create sf object
geosub <- st_as_sf(x = sub2,
                   coords = c("Longitude", "Latitude"),
                   crs = projcrs)

geosub <- st_transform(geosub, crs = domainCRS)

## calc the bounding box
# bbox <- st_bbox(geosub)
# bbox <- as.matrix(bbox)

polybbox <- geosub  %>%
  st_bbox() %>%
  st_as_sfc()

polybbox_buffer <- st_buffer(polybbox, dist=200000) # map units are in meters
bbox <- st_bbox(polybbox_buffer)

## get sf basemap
world <- ne_countries(scale = "medium", returnclass = "sf") #, country = "United States of America"
world <- st_transform(world, crs = domainCRS)
world <- st_crop(world, bbox)

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = geosub,
          color = "red",
          size = 1,
          shape = 15)

##### write shapefile #####
st_write(geosub,
         "C:/Users/Robert.Mcguinn/Documents/ArcGIS/Projects/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/shapefiles/mapdata.shp",
         delete_dsn = T)

###### import protected area shapefile using sf #####
pa <- sf::st_read("C:/data/gis_data/protected_areas/shapefiles/20221104_protected_areas.shp")


