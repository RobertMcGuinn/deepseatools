##### header #####
## author:Robert McGuinn | robert.mcguinn@noaa.gov | rpm@lumni.duke.edu
## start_date: 20231009
## forked_from: https://cran.r-project.org/web/packages/h3jsr/vignettes/intro-to-h3jsr.html
## purpose: experimentation with 'h3jsr'package and the NDB

##### packages #####
local_options <- options()
library(sf)
library(tidyverse)
library(ggplot2)
library(h3jsr)
options(stringsAsFactors = FALSE)

##### load ndb #####
source('c:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### create sf object from NDB #####
filtgeo  <- st_as_sf(filt, coords = c("Longitude", "Latitude"), crs = 4326)

##### get hexagons for chosen resolutions #####
filt_h3 <- point_to_cell(filtgeo,
                            res = seq(5,6),
                            simple = FALSE)

##### check #####
filt_h3 %>%
  group_by(AphiaID, h3_resolution_5) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  View


##### looking at depth distribution of coral and sponge occurrences summarized within hexes #####
filt_h3 %>% group_by(h3_resolution_6) %>%
  summarize(mindepth_res5 = min(MinimumDepthInMeters),
            maxdepth_res5 = max(MaximumDepthInMeters)) %>%
  View()

##### look up hexagons and points that match a set of conditions #####
locality <- 'Davidson Seamount'

cats <- filt_h3 %>%
  filter(grepl(locality, Locality)) %>%
  select(c('h3_resolution_5','h3_resolution_6'))
hexlist <- unlist(cats, use.names = FALSE)
hexlist <- unique(hexlist)

## make a selection of points that match
points <- filtgeo %>%
  filter(grepl(locality, Locality))

x <- filt_h3 %>%
  filter(h3_resolution_5 %in% hexlist |
           h3_resolution_6 %in% hexlist) %>%
  pull(CatalogNumber) %>% unique()

points2 <- filtgeo %>% filter(CatalogNumber %in% x)

##### check ######
class(hexlist)
length(hexlist)
length(unique(hexlist))

##### create polygons from hexes ######
hexes <- cell_to_polygon(unique(hexlist), simple = FALSE)

##### check #####
class(hexes)
st_crs(hexes)

##### export shapefiles for examination #####
library(sf)
st_write(hexes, "c:/rworking/deepseatools/indata/hexes.shp", append = F)
st_write(points, "c:/rworking/deepseatools/indata/points.shp", append = F)
st_write(points2, "c:/rworking/deepseatools/indata/points2.shp", append = F)

##### plot the map #####
hex_bbox <- st_bbox(hexes)

points %>%
  ggplot() +
  geom_sf(fill = NA, colour = 'black') +
  geom_sf(data = hexes, aes(fill = NA), alpha = 0.5) +
  scale_fill_viridis_d() +
  ggtitle('Hex Relationships', subtitle = 'Neighboring Resolutions') +
  theme_minimal() +
  coord_sf(xlim = c(hex_bbox$xmin, hex_bbox$xmax), ylim = c(hex_bbox$ymin, hex_bbox$ymax))



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









