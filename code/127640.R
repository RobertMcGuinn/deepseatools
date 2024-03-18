##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose: see redmine issue

##### linkage #####
## manual input here
filename <- '127640' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
## manual input here
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)

##### bring in NDB #####
## work
source('c:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### bring in shapefile of AOI #####
fph_aoi <- sf::st_read(
  "C:/rworking/deepseatools/indata/FL_Pan_AOI_500mbuff/FL_Pan_AOI_500mbuff.shp"
  )

##### set project crs #####
projcrs <- st_crs(fph_aoi)

##### create sf version of database #####
geo_filt <- st_as_sf(filt, coords = c("Longitude", "Latitude"), crs = 4326)

##### transform crs to project crs #####
geo_filt_transform <- st_transform(geo_filt, crs = projcrs)

##### intersect points with aoi #####
points <- st_intersection(geo_filt_transform, fph_aoi)

##### export points for GIS check #####
st_write(points,
         "C:/rworking/deepseatools/indata/points.shp",
         append = F)

##### write out points for delivery #####
points_select <- filt %>% filter(CatalogNumber %in% points$CatalogNumber)

##### points_select #####


##### export result to csv (export to CSV) #####
filename <- "20240318-0_subset_of_NDB_version_20240115-0_RPMcGuinn.csv"
write.csv(points_select,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)














