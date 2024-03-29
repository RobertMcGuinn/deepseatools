##### header #####
## author:Robert McGuinn | robert.mcguinn@noaa.gov | rpm@lumni.duke.edu
## start_date: 20230922
## forked_from: https://cran.r-project.org/web/packages/h3jsr/vignettes/intro-to-h3jsr.html
## purpose: experimentation with 'h3jsr'package

##### packages #####
local_options <- options()
library(sf)
library(tidyverse)
library(ggplot2)
library(h3jsr)
options(stringsAsFactors = FALSE)
library(Hmisc)


##### lat/long point to sfc_point #####
bth <- sf::st_sfc(sf::st_point(c(153.023503, -27.468920)), crs = 4326)

##### check #####
bth
class(bth)

##### find hid index at a particular resolution
hid <-point_to_cell(bth, res = 15) #res must range from 0-15
hid
class(hid)

##### bring in polygon shapefile to sf dataframe #####
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

##### check #####
class(nc)
dim(nc)
st_crs(nc)
str(nc)
structure(nc)
dim(nc)

##### find the centroid of each polygon and create sf point object #####
nc_pts <- st_centroid(nc)

#####
st_crs(nc_pts)

##### set the coordinate reference system #####
nc_pts <- st_transform(nc_pts, crs = 4326)

###### whittle down the columns #####
nc_pts <- dplyr::select(nc_pts, CNTY_ID, NAME)

##### check #####
nc
dim(nc_pts)
summary(nc_pts)
str(nc_pts)
str(nc)

##### get h3 index at all resolutions for every point #####
nc_all_res <- point_to_cell(nc_pts,
                            res = seq(0, 15),
                            simple = FALSE)

##### check result #####
head(nc_all_res[, c(1:10)])
dim(nc_all_res)

##### unlist hexes to character string for single row and selected columns#####
x <- nc_all_res %>%
  filter(NAME == "Buncombe") %>%
  select(c('h3_resolution_4','h3_resolution_5', 'h3_resolution_15'))

hexes <- unlist(x, use.names = FALSE)

##### check ######
class(hexes)

##### create polygons from hexes ######
hexes <- cell_to_polygon(hexes, simple = FALSE)

##### check #####
class(hexes)
st_crs(hexes)

##### plot the map #####
nc %>%
  filter(NAME == "Buncombe") %>%
  ggplot() +
  geom_sf(fill = NA, colour = 'black') +
  geom_sf(data = hexes, aes(fill = h3_address), alpha = 0.5) +
  scale_fill_viridis_d() +
  ggtitle('H3 hexagons NC Counties', subtitle = 'Resolution XX') +
  theme_minimal() +
  coord_sf()

##### get area of hexagons #####

##### Convert the CRS to a suitable projection for your area of interest #####
hexes_transform <- st_transform(hexes, crs = 32119)  # Example CRS (UTM Zone 33N)

##### Calculate the area in square kilometers #####
area_km2 <- st_area(hexes_transform) / 1e6  # Convert square meters to square kilometers

##### transform from 'units' object to character string #####
area_km2_char <- as.character(area_km2)

##### add area back to original hexes file #####
hexes_area <- hexes %>%
  mutate(area_km2 = area_km2_char)

##### check #####
hexes_area
hexes_area$area_km2
class(hexes_area$area_km2)









