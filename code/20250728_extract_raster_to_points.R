##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20250728
## purpose: extract values from raster with points

##### packages #####
library(sf)
library(terra)
library(tidyverse)

##### load raster #####
## Load the raster using terra function 'rast'
r <- rast('indata/Apr_2014_AShantharam.tif')

# Set spatial extent
# Remember: terra uses xmin, xmax, ymin, ymax
ext(r) <- c(-180, 180, -90, 90)

# Set CRS (Equidistant Cylindrical = Plate Carrée)
# This is a lat/lon system with linear spacing in degrees
crs(r) <- "EPSG:4326"  # WGS 84 is typically used for Plate Carrée

##### load points to dataframe #####
points <- read.csv('indata/site_points_AShantharam.csv')

##### create a 'SpatVector' from points using the terra function 'vect' #####
pts <- vect(points,
            geom = c("Longitude", "Latitude"),
            crs = "EPSG:4326")

# Extract raster values at point locations
vals <- terra::extract(r, pts)



