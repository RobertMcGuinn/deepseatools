##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose:

##### linkage #####
filename <- '126248' ## manual: for this code file name, match to redmine
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
library(sf)

##### project rasters using the terra package #####
library(terra)
x1 <- rast("c:/rworking/deepseatools/indata/c41078a1.tif")
ext(x1)
original_crs <- crs(x1)

x2 <- project(x1, "EPSG:4326")
ext(x2)

x3 <- project(x2, original_crs)
ext(x3)

## Load raster data
x1 <- rast("c:/rworking/deepseatools/indata/cea.tif")

## project raster to EPSG:4326
x2 <- project(x1, "EPSG:4326")

# Define the extent for subsetting (adjust as needed)
subset_extent <- c(-117.6417, -117.3087, 33.66507, 33.94)  # xmin, xmax, ymin, ymax

# Crop the raster to the subset extent
x_subset <- crop(x2, subset_extent)

# Get extent of the raster
raster_extent <- ext(x_subset)

# Load Natural Earth data (world boundaries)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Convert raster to a data frame
raster_df <- terra::as.data.frame(x_subset, xy = TRUE)

# Create ggplot object
p <- ggplot() +
  # Add raster layer
  geom_raster(data = raster_df, aes(x = x, y = y, fill = cea)) +
  # Add world boundaries
  geom_sf(data = world, fill = NA, color = "black") +
  # Set color scale
  scale_fill_viridis_c() +
  # Add title and labels
  labs(title = "Raster Map with Natural Earth Boundaries",
       x = "Longitude", y = "Latitude",
       fill = "Value") +
  theme_minimal() +
  # Set plot limits to raster extent
  xlim(raster_extent[1], raster_extent[2]) +
  ylim(raster_extent[3], raster_extent[4])

p
