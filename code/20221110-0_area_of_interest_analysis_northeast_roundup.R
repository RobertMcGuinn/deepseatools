##### Header #####
## author: Robert.McGuinn
## file start date: 20221104
## purpose: managed area analysis

##### packages ######
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(marmap)
library(raster)
library(googledrive)

##### authorizations #####
drive_auth(email = "robert.mcguinn@noaa.gov")
gs4_auth(email = "robert.mcguinn@noaa.gov")

##### manual: load latest version of NDB #####
setwd("C:/rworking/deepseatools/indata")
indata <- read.csv("DSCRTP_NatDB_20220801-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)
rm(indata)

##### manual: load latest version of Sarah Bingo file #####
## https://vlab.noaa.gov/redmine/issues/109659
setwd("C:/rworking/deepseatools/indata")
sbingo <- read.csv("20221104-1_NOAA_EX1304_Northeast_US_SBingo_2013.csv", header = T)

##### rbind the sbingo file with the filt #####
filt <- rbind(filt, sbingo)

##### check ######
# length(filt$CatalogNumber) +
# length(sbingo$CatalogNumber)
# length(filtbingo$CatalogNumber)

##### import shapefile of aoi using sf #####
## 2 separate shapefiles

midatl <- sf::st_read(
  "C:/data/gis_data/fish_council_region_2017_update_heather/MidAtlanticFMCregion2017update.shp")
midatl <- st_transform(midatl,32618)

neweng <- sf::st_read(
  "C:/data/gis_data/fish_council_region_2017_update_heather/NewEnglandFMCregion2017update.shp")
neweng <- st_transform(neweng,32618)

##### buffer operation #####
midatl_buff_5000 <- st_buffer(midatl,dist = 5000)
neweng_buff_5000 <- st_buffer(neweng,dist = 5000)

##### join buffered polygons #####
pa <- dplyr::bind_rows(list(midatl_buff_5000, neweng_buff_5000))
pa <- st_union(pa)

##### check #####
# names(pa)
# table(pa$REGION, useNA = 'always')
# ggplot(pa) + geom_sf()
# names(pa)

##### assign pa to aoi #####
aoi <- pa

##### check #####
aoi$Sitename
View(aoi)
plot(aoi)

##### transform coral and sponge points to sf object #####
points <- st_as_sf(filt, coords = c("Longitude", "Latitude"), crs = 4326)

##### check #####
##### set CRS of points to be same as AOI #####
points_1 <- st_transform(points, crs = st_crs(aoi))

##### intersect the points with the aoi #####
aoi_points <- st_intersection(points_1, aoi)
##### check #####
st_crs(aoi_points)

##### make a simple map #####
## get bathy layer
# bathy = getNOAA.bathy(lon1 = -75,
#                       lon2 = -65,
#                       lat1 = 38,
#                       lat2 = 44,
#                       resolution = 1)

## create a world map and transform coordinates to be same as aoi
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = st_crs(aoi))

## create a bounding coordinates set for aoi for mapping purposes
aoi_coord <- st_coordinates(aoi)
aoi_coord <- data.frame(aoi_coord)

## manual: set zoom level for map (z must be entered in the map units used in the CRS)
z <- 0 # look at the map units first

## calculate bounding box with buffers
minlon <- min(aoi_coord$X)- z
maxlon <- max(aoi_coord$X) + z
minlat <- min(aoi_coord$Y) - z
maxlat <- max(aoi_coord$Y) + z

## plot the map using ggplot2
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = aoi) +
  geom_sf(data = aoi_points) +
  coord_sf(xlim = c(minlon,maxlon),
         ylim = c(minlat,maxlat),
         expand = FALSE)

## save the plot
ggsave("c:/rworking/deepseatools/images/20221128-quick_map_NE_summary.png",
       width = 20,
       height = 20,
       units = "cm")

##### create a summary of the dataset and export to csv #####
sum_tbl <-
  aoi_points %>%
  group_by(DatasetID, SurveyID, ObservationYear) %>%
  dplyr::summarize(
    EventIDs = paste(unique(EventID), collapse=" | "),
    VehicleNames = paste(unique(VehicleName), collapse=" | "),
    SamplingEquipment = paste(unique(SamplingEquipment), collapse=" | "),
     PIs = paste(unique(PI), collapse=" | "),
    DataContacts = paste(unique(DataContact), collapse=" | "),
    Records = n()) %>% arrange(desc(ObservationYear))

sum_tbl_df <- sum_tbl %>% st_drop_geometry()

##### write to CSV #####
write.csv(sum_tbl_df,
          "c:/rworking/deepseatools/reports/20221110-0_greater_atlantic_summary.csv")

##### upload csv summary to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1-mTXFck0tA_O-QzXvas0NGtPi9wZ0VaV"
setwd("C:/rworking/deepseatools/reports")
filename = "20221110-0_greater_atlantic_summary.csv"
drive_upload(filename,
             path = as_id(folderurl),
             name = filename,
             overwrite = T)

##### upload PNG images to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1UkBLOLqGpSUT0gWhsRjW0ZwU7imQy9q-"
setwd("C:/rworking/deepseatools/images")
filename = "20221104-quick_map_NE_seamounts_canyons_monument.png"
drive_upload(filename,
             path = as_id(folderurl),
             name = filename,
             overwrite = T)

##### get unique list of species #####
species_list <- aoi_points %>%
  group_by(ScientificName, AphiaID, Phylum, Class, Order, Family, Genus) %>%
  summarize(n=n())

species_list_df <- as.data.frame(species_list)
species_list_df <- species_list_df[,1:8]




