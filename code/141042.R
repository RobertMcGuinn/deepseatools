##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20241204
## purpose: Custom export of South Florida ESI data from NDB for Lauren Z. at RPI.

##### linkage #####
filename <- '141042' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
brofseURL(github_link)
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

##### create spatial points data frame from a subset of points #####
x_geo <- filt

## Convert to sf object
x_geo_sf <- st_as_sf(x_geo, coords = c("Longitude", "Latitude"), crs = 4326)

##### read in the AOI #####
shapefile_path <- 'C:/rworking/deepseatools/indata/datarequestforsouthfloridaesiatlas/AOI_BIO_500mbuf/AOI_BIO_500mbuf.shp'
aoi_500 <- st_read(shapefile_path)

##### intersect to extract the points in the aoi #####
## transform CRS for coral and sponge points projection to match polygons
x_geo_sf_transform <- st_transform(x_geo_sf, crs = st_crs(aoi_500))

## spatial join points with aoi
points <- st_join(x_geo_sf_transform, aoi_500, join = st_within)

## create separate sf data frame for points in
points_in_aoi <- points %>% filter(is.na(Shape_Area) == F)

##### get unique database version #####
v <- unique(filt$DatabaseVersion)

##### plot to see how they relate #####
ggplot() +
  geom_sf(data = points_in_aoi, fill = "green", alpha = 0.2) +
  geom_sf(data = aoi_500, fill = "blue", alpha = 0.3) +
  annotate("text", x = -Inf, y = Inf, label = paste("NDB Version:", paste(v, collapse = ", ")),
           hjust = 0, vjust = 1, size = 4, color = "black") +
  labs(
    title = "Spatial Points and AOI Visualization",
    x = "Longitude",  # New x-axis label
    y = "Latitude"    # New y-axis label
  ) +
  theme_minimal()


##### save the plot #####
ggsave('c:/rworking/deepseatools/indata/datarequestforsouthfloridaesiatlas/20241204_NDB_points_in_AOI_RPMcGuinn.png',
  width = 6,
  height = 5,
  units = c("in"),
  dpi = 300,
  create.dir = TRUE,
)


##### export result to csv (export to CSV) #####
points_in_aoi_df <- filt %>% filter(CatalogNumber %in% points_in_aoi$CatalogNumber)

filename <- "20241204_NDB_version_20241022-1_for_South_Florida_ESI_AOI_RPMcGuinn.csv"
write.csv(points_in_aoi_df,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)












