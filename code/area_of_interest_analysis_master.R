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

##### import current database #####
##### manual input: load latest version of NDB #####
setwd("C:/rworking/deepseatools/indata")
indata <- read.csv("DSCRTP_NatDB_20220801-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

rm(indata)
##### import shapefile using sf #####
pa <- sf::st_read("C:/data/gis_data/protected_areas/shapefiles/20221104_protected_areas.shp")

##### check #####
names(pa)

##### extract the aoi polygon of interest using a filter #####
aoi <- pa %>% filter(grepl("NE Canyons and Seamounts", Sitename))

##### check #####
aoi$Sitename
View(aoi)
plot(aoi)

##### transform coral and sponge points to sf object #####
points <- st_as_sf(filt, coords = c("Longitude", "Latitude"), crs = 4326)

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
z <- 200000 # look at the map units first

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
ggsave("c:/rworking/deepseatools/images/20221104-quick_map_NE_seamounts_canyons_monument.png",
       width = 20,
       height = 20,
       units = "cm")

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

write.csv(species_list_df, "c:/rworking/deepseatools/reports/20221104-0_unique_coral_and_sponge_species_in_Northeast_Canyons_and_Seamounts_Nat_Monument.csv")

##### upload csv species list to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1nwzGrKuTWx5bc8FyYXiDWqE1HHwI7pye"
setwd("C:/rworking/deepseatools/reports")
filename = "20221104-0_unique_coral_and_sponge_species_in_Northeast_Canyons_and_Seamounts_Nat_Monument.csv"
drive_upload(filename,
             path = as_id(folderurl),
             name = filename,
             overwrite = T)

##### query and load images to local folder (go get a coffee if you have tons of images) #####
aoi_points_images <- aoi_points %>%
  filter(is.na(ImageURL) == F,
         ImageURL != "NA")


path <- 'C:/rworking/deepseatools/images/aoi_imageset_GARFO'
dir.create(path)
setwd(path)
for(i in 1:length(aoi_points_images$CatalogNumber)){
  download.file(as.character(aoi_points_images$ImageURL[i]),
                destfile = paste("DSCRTP",
                                 aoi_points_images$CatalogNumber[i],
                                 aoi_points_images$ScientificName[i],
                                 aoi_points_images$DepthInMeters[i],
                                 basename(as.character(aoi_points_images$ImageURL[i])),
                                 sep = '_'),
                mode = "wb")
}

##### loading files from a local folder to Google Drive #####
## MANUAL CHANGE "folderurl" to the desired drive folder ID
folderurl <- "https://drive.google.com/drive/folders/1AvTgB31Y5_8w250zyTn_j6GTcTMpjqqq"

## get the list of files from the local folder
files <- list.files(path = path, full.names=TRUE, recursive=FALSE)

## loop upload images to Google Drive
for(i in files){
  drive_upload(i,
               path = as_id(folderurl),
               overwrite = T)
}





