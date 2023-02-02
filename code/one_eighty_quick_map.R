##### packages #####
library(sf)
library(ggplot2)
##### static locator map creation #####
## filter out -999
sub2 <- sub %>%
  filter(Latitude != '-999' , Longitude != '-999')

## set projection of incoming data
projcrs <- 4326

## you can change this to suit the location
domainCRS <- '+proj=aea +lat_1=30 +lat_2=70 +lat_0=52 +lon_0=-170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs '

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

polybbox_buffer <- st_buffer(polybbox, dist=1) # map units are in meters 2000000
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

