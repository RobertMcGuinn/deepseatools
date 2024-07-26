##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20240726
## purpose: see Redmine issue linked below (for Lisa Wickliffe)

##### linkage #####
filename <- '132980' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### load shapefiles #####
## Central Altlantic Call areas from Lisa Wickliffe
ca_ef <- sf::st_read("../indata/wickliffe/Central_ATL_E_and_F_call_areas_outlines.shp")
ca2 <- sf::st_read("../indata/wickliffe/Central_ATL_E_and_F_call_areas_outlines.shp")

##### creating a geographic bounding box for the polygons of interest
minLon <- -80
maxLon <- -72.5
minLat <- 32
maxLat <- 40

##### filtering data by bounding box #####
geofilt <-
  filt %>% filter(as.numeric(Latitude) > minLat,
                  as.numeric(Latitude) < maxLat,
                  as.numeric(Longitude) < maxLon,
                  as.numeric(Longitude) > minLon)

geofilt <- st_as_sf(geofilt, coords = c("Longitude", "Latitude"), crs = 4326)

##### export points for GIS check #####
st_write(geofilt,
         "C:/rworking/deepseatools/indata/geofilt.shp",
         append = F)

##### write out points for delivery #####
points_select <- filt %>% filter(CatalogNumber %in% geofilt$CatalogNumber)

##### export result to csv (export to CSV) #####
filename <- "20240726-0_subset_of_NDB_version_20240325-0_RPMcGuinn.csv"
write.csv(points_select,
          paste("../indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)














