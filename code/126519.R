##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose: see redmine issue

##### linkage #####
## manual input here
filename <- '126519' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
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

##### load data #####

source('c:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### subset data by coordinates in Southeast Atlantic #####

## set bounding box
minlat <- 22
maxlat <- 37
minlon <- -84
maxlon <- -74

x <- subset(filt, as.numeric(Latitude) > minlat &
              as.numeric(Latitude) < maxlat &
              as.numeric(Longitude) > minlon &
              as.numeric(Longitude) < maxlon)

x$Latitude <- as.numeric(x$Latitude)
x$Longitude <- as.numeric(x$Longitude)

##### make gis file #####
points <- st_as_sf(x, coords = c("Longitude", "Latitude"), crs = 4326)

##### write shapefile for checking in ArcGIS Pro #####
st_write(points, "C:/rworking/deepseatools/indata/sub_geo.shp", delete_dsn = T)

##### export result to csv (export to CSV) #####
filename <- "20240213-0_subset_of_NDB_version_20240115-0_RPMcGuinn.csv"
write.csv(points,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin1",
          row.names = F,
          quote = T)


















