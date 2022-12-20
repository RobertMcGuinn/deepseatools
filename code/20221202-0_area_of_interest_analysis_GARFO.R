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
library(googlesheets4)
library(spocc)

##### authorizations #####
drive_auth(email = "robert.mcguinn@noaa.gov")
gs4_auth(email = "robert.mcguinn@noaa.gov")
##### manual input: load latest version of NDB #####
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

##### import protected area shapefile using sf #####
pa <- sf::st_read("C:/data/gis_data/protected_areas/shapefiles/20221104_protected_areas.shp")

##### check #####
## names(pa)

##### extract the aoi polygon of interest using a filter #####
aoi <- pa %>% filter(grepl("NE Canyons and Seamounts", Sitename))

##### write the aoi to a shapefile for the record #####
st_write(aoi,
         "C:/rworking/deepseatools/indata/aoi/nescnm.shp",
         append = F)

##### check #####
# aoi$Sitename
# View(aoi)
# plot(aoi)

##### transform coral and sponge points to sf object #####
points <- st_as_sf(filt, coords = c("Longitude", "Latitude"), crs = 4326)

##### set CRS of points to be same as AOI #####
points_1 <- st_transform(points, crs = st_crs(aoi))

##### intersect the points with the aoi #####
aoi_points <- st_intersection(points_1, aoi)

##### write input data inside the monument #####
## tranform back to EPSG:4326
aoi_points_4326 <-
  st_transform(aoi_points,
               crs = st_crs(points))

## get latitude and longitude back
aoi_points_4326_no_geom <- aoi_points_4326 %>%
  mutate(longitude = unlist(map(aoi_points_4326$geometry,1)),
         latitude = unlist(map(aoi_points_4326$geometry,2)))

## drop geometry from sf file first
aoi_points_4326_df <-
  aoi_points_4326_no_geom %>%
  st_drop_geometry()

## write it
write_csv(aoi_points_4326_df,
          "c:/rworking/deepseatools/reports/20221207-0_DSCRTP_Occurrences_Northeast_Canyons_and_Seamounts_Nat_Monument.csv")

##### write input data from DSCRTP to Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1er5SqUGKd6zn49Q9lbvI_1vME8259Yc5"
setwd("C:/rworking/deepseatools/reports")
filename = "20221207-0_DSCRTP_Occurrences_Northeast_Canyons_and_Seamounts_Nat_Monument.csv"
drive_upload(filename,
             path = as_id(folderurl),
             name = filename,
             overwrite = T)

##### check #####
# st_crs(aoi_points)

##### make a simple map #####
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
ggsave("c:/rworking/deepseatools/images/20221202-0_quick_map_NE_seamounts_canyons_monument.png",
       width = 20,
       height = 20,
       units = "cm")

##### upload PNG images to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1UkBLOLqGpSUT0gWhsRjW0ZwU7imQy9q-"
setwd("C:/rworking/deepseatools/images")
filename = "20221202-0_quick_map_NE_seamounts_canyons_monument.png"
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

##### write unique species list from NDB #####
write.csv(species_list_df, "c:/rworking/deepseatools/reports/20221202-0_unique_coral_and_sponge_species_in_Northeast_Canyons_and_Seamounts_Nat_Monument.csv")

##### check #####
# names(species_list_df)
# names(aoi_points)
# table(aoi_points$SurveyID)
# aoi_points_sub <- aoi_points %>%
#   filter(grepl("Bingo", DataContact)) %>%
#   group_by(DatasetID, DataProvider, SurveyID) %>%
#   summarize(n=n())
# View(aoi_points_sub)
#
# ndb_sub_bingo <- filt %>%
#   filter(grepl("1304", SurveyID)) %>%
#   group_by(DatasetID, DataProvider, SurveyID) %>%
#   summarize(n=n())
# View(ndb_sub_bingo)

##### check #####
# aoi_points %>%
#   filter(grepl("EX", SurveyID)) %>%
#   pull(SurveyID) %>%
#   unique() %>% View()

##### upload csv species list to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1nwzGrKuTWx5bc8FyYXiDWqE1HHwI7pye"
setwd("C:/rworking/deepseatools/reports")
filename = "20221202-0_unique_coral_and_sponge_species_in_Northeast_Canyons_and_Seamounts_Nat_Monument.csv"
drive_upload(filename,
             path = as_id(folderurl),
             name = filename,
             overwrite = T)

##### manual: query and load images to local folder (go get a coffee if you have tons of images) #####
aoi_points_images <- aoi_points %>%
  filter(is.na(ImageURL) == F,
         ImageURL != "NA",
         grepl("deepseacoral",
               ImageURL))

## just get a few records for testiing
aoi_points_images <- aoi_points_images %>% slice(2, 3, 8)

## just get the first link to the image
aoi_points_images$ImageURL <- gsub( " .*$", "", aoi_points_images$ImageURL)

## write the file for export (optional)
# path <- 'C:/rworking/deepseatools/indata'
# write.csv(aoi_points_images,
#           paste(path,
#                 "/20221205-0_EX1304_sub_RPMcGuinn.csv",
#                 sep = ''))


path <- 'C:/rworking/deepseatools/images/test'
dir.create(path)
setwd(path)
for(i in 1:length(aoi_points_images$CatalogNumber)){
  download.file(as.character(aoi_points_images$ImageURL[i]),
                destfile = paste("DSCRTP",
                                 aoi_points_images$CatalogNumber[i],
                                 aoi_points_images$ScientificName[i],
                                 aoi_points_images$DepthInMeters[i],
                                 aoi_points_images$SurveyID[i],
                                 # basename(as.character(aoi_points_images$ImageFilePath[i])),
                                 sep = '_'),
                mode = "wb",
                method = "auto")
}=

download.file(as.character(aoi_points_images$images[i]),
              destfile = "yo.jpg",
              mode = "wb")

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






##### getting obis data #####
## using bounding box variables defined above
aoi_transform <- st_transform(aoi, crs = st_crs(points))
bbox <- st_bbox(aoi_transform)
bbox_matrix <- as.matrix(bbox)
bbox = c(bbox_matrix[1,1],
         bbox_matrix[2,1],
         bbox_matrix[3,1],
         bbox_matrix[4,1])

out <- occ(geometry = bbox,
           from = 'obis',
           limit = 10000)

obis_species_occ <- as.data.frame(out$obis$data)

##### turning the obis dataset into sf object  #####
obis_points <- st_as_sf(obis_species_occ,
                        coords = c("longitude", "latitude"),
                        crs = 4326)

##### transformation of CRS ####
obis_points_transform <- st_transform(obis_points, crs = st_crs(aoi))

##### intersect the obis points with the aoi #####
obis_points_in_monument <- st_intersection(obis_points_transform, aoi)

##### write input data from OBIS inside the monument #####
## tranform back to EPSG:4326
obis_points_in_monument_4326 <-
  st_transform(obis_points_in_monument,
               crs = st_crs(points))

## get latitude and longitude back
obis_points_in_monument_4326_no_geom <- obis_points_in_monument_4326 %>%
  mutate(longitude = unlist(map(obis_points_in_monument_4326$geometry,1)),
         latitude = unlist(map(obis_points_in_monument_4326$geometry,2)))

## drop geometry from sf file first.
obis_points_in_monument_4326_df <- obis_points_in_monument_4326_no_geom %>% st_drop_geometry()

## write it
write_csv(obis_points_in_monument_4326_df,
          "c:/rworking/deepseatools/reports/20221207-0_OBIS_Occurrences_Northeast_Canyons_and_Seamounts_Nat_Monument.csv")

##### write input data from OBIS to Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1er5SqUGKd6zn49Q9lbvI_1vME8259Yc5"
setwd("C:/rworking/deepseatools/reports")
filename = "20221207-0_OBIS_Occurrences_Northeast_Canyons_and_Seamounts_Nat_Monument.csv"
drive_upload(filename,
             path = as_id(folderurl),
             name = filename,
             overwrite = T)



##### make a simple map #####
## create a world map and transform coordinates to be same as aoi
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = st_crs(aoi))

## create a bounding coordinates set for aoi for mapping purposes
aoi_coord <- st_coordinates(aoi)
aoi_coord <- data.frame(aoi_coord)

## manual: set zoom level for map (z must be entered in the map units used in the CRS)
z <- 200000 # look at the map units first

## calculate bounding box with buffers
minlon <- min(aoi_coord$X) - z
maxlon <- max(aoi_coord$X) + z
minlat <- min(aoi_coord$Y) - z
maxlat <- max(aoi_coord$Y) + z

## plot the map using ggplot2
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = aoi) +
  geom_sf(data = obis_points_in_monument) +
  coord_sf(xlim = c(minlon,maxlon),
           ylim = c(minlat,maxlat),
           expand = FALSE)

## save the plot
ggsave("c:/rworking/deepseatools/images/20221202-0_quick_map_NE_seamounts_canyons_monument.png",
       width = 20,
       height = 20,
       units = "cm")

##### make species list for OBIS #####
obis_species_list <- obis_points_in_monument %>%  filter(phylum != "Cnidaria", phylum != "Porifera") %>%
  group_by(kingdom, phylum, class, order, family, genus, species, scientificNameID, aphiaID) %>%
  summarise(n=n())

View(obis_species_list)

##### write unique species list from OBIS #####
write.csv(obis_species_list,
          "c:/rworking/deepseatools/reports/20221206-0_OBIS_Northeast_Canyons_and_Seamounts_Nat_Monument.csv")

##### upload csv species list to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1nwzGrKuTWx5bc8FyYXiDWqE1HHwI7pye"
setwd("C:/rworking/deepseatools/reports")
filename = "20221206-0_OBIS_Northeast_Canyons_and_Seamounts_Nat_Monument.csv"
drive_upload(filename,
             path = as_id(folderurl),
             name = filename,
             overwrite = T)
