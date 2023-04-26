##### Header #####
## projectname: 20230329_BSEE_Alaska_deep_ESI_RPMcGuinn
## author: Robert McGuinn
## date started:
## forkedfrom: none
## drive:https://drive.google.com/drive/folders/1XRw6f-6tb9AguVyHzh1B2UeW8yFbuJiJ?usp=share_link
## purpose: aoi analysis for RPI ESI work for BSEE

##### packages #####
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

##### authorizations #####
# Set authentication token to be stored in a folder called \.secrets``
options(gargle_oauth_cache = ".secrets")

# Authenticate manually
gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:
list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### load national database (local) #####
setwd("C:/rworking/deepseatools/indata")
filename <- "DSCRTP_NatDB_20221213-0.csv"
indata<-read_csv(filename,
                 locale = locale(encoding = 'latin9'),
                 na = c("-999", "NA"))

filt <- indata %>%
  filter(Flag == 0)

##### clear all objects besides national database #####
rm(list = setdiff(ls(), "filt"))

##### load aois from shapefile #####
path <- 'C:/Users/Robert.Mcguinn/Documents/ArcGIS/Projects/20230329_BSEE_Alaska_deep_ESI_RPMcGuinn/aoi/'

beaufort <- sf::st_read(paste(path,"BSEE_AOI_BEAUFORT_SEA/BSEE_AOI_BEAUFORT_SEA.shp", sep = ''))
chukchi <- sf::st_read(paste(path, "BSEE_AOI_CHUKCHI_SEA/BSEE_AOI_CHUKCHI_SEA.shp", sep = ''))
cook <- sf::st_read(paste(path,"BSEE_AOI_COOK_INLET_KODIAK_IS/BSEE_AOI_COOK_INLET_KODIAK_IS.shp", sep = ''))

##### transform coral and sponge points to sf object #####
points <- st_as_sf(filt, coords = c("Longitude", "Latitude"), crs = 4326)

##### **beaufort #####
##### buffer operation #####
beaufort_5000 <- st_buffer(beaufort, dist = 5000)

##### check #####
plot(beaufort_5000)

##### set to generic aoi #####
aoi <- beaufort_5000

##### check #####
##### set CRS of points to be same as AOI #####
points_1 <- st_transform(points, crs = st_crs(aoi))

##### intersect the points with the aoi #####
aoi_points <- st_intersection(points_1, aoi)

##### check #####
st_crs(aoi_points)
class(aoi_points)
aoi_points$CatalogNumber

##### pull intersecting points directly from the database #####
cats <- aoi_points$CatalogNumber
beaufort_corals_sponges <- filt %>%  filter(CatalogNumber %in% cats)

##### write_to_csv #####
filename <- '20230421_NDB_for_corals_sponges_version_20221213-0_subset_BSEE_AOI_BEAUFORT_SEA.csv'
write_csv(beaufort_corals_sponges,
          paste("c:/rworking/deepseatools/indata/",
                filename))










##### **chuckchi #####
##### buffer operation #####
chukchi_5000 <- st_buffer(chukchi, dist = 5000)
##### check #####
plot(chukchi)

##### set to generic aoi #####
aoi <- chukchi_5000

##### check #####
##### set CRS of points to be same as AOI #####
points_1 <- st_transform(points, crs = st_crs(aoi))

##### intersect the points with the aoi #####
aoi_points <- st_intersection(points_1, aoi)

##### check #####
st_crs(aoi_points)
class(aoi_points)
aoi_points$CatalogNumber

##### pull intersecting points directly from the database #####
cats <- aoi_points$CatalogNumber
chukchi_corals_sponges <- filt %>%  filter(CatalogNumber %in% cats)

##### write_to_csv #####
filename <- '20230421_NDB_for_corals_sponges_version_20221213-0_subset_BSEE_AOI_CHUKCHI_SEA.csv'
write_csv(chukchi_corals_sponges,
          paste("c:/rworking/deepseatools/indata/",
                filename))











##### **cook #####
##### buffer operation #####
cook_5000 <- st_buffer(cook, dist = 5000)
##### check #####
plot(cook)

##### set to generic aoi #####
aoi <- cook_5000

##### check #####
##### set CRS of points to be same as AOI #####
points_1 <- st_transform(points, crs = st_crs(aoi))

##### intersect the points with the aoi #####
aoi_points <- st_intersection(points_1, aoi)

##### check #####
st_crs(aoi_points)
class(aoi_points)
aoi_points$CatalogNumber

##### pull intersecting points directly from the database #####
cats <- aoi_points$CatalogNumber
cook_corals_sponges <- filt %>%  filter(CatalogNumber %in% cats)

##### write_to_csv #####
filename <- '20230421_NDB_for_corals_sponges_version_20221213-0_subset_BSEE_AOI_COOK_INLET_KODIAK_IS.csv'
write_csv(cook_corals_sponges,
          paste("c:/rworking/deepseatools/indata/",
                filename))









##### *****getting OBIS data for Beaufort sea *****#####
library(spocc)
beaufortobis <- st_transform(beaufort, crs = st_crs(points))
bbox <- st_bbox(beaufortobis)
bbox_matrix <- as.matrix(bbox)
bbox = c(bbox_matrix[1,1],
         bbox_matrix[2,1],
         bbox_matrix[3,1],
         bbox_matrix[4,1])

out <- occ(geometry = bbox,
           from = 'obis',
           limit = 10000)
obis <- as.data.frame(out$obis$data)
obis2 <- obis %>% select(id, scientificNameID, recordNumber, latitude, longitude, phylum, family)

##### turning the obis dataset into sf object  #####
obis_points <- st_as_sf(obis2,
                        coords = c("longitude", "latitude"),
                        crs = 4326)

##### transformation of CRS ####
obis_points_transform <- st_transform(obis_points, crs = st_crs(beaufort))

##### intersect the obis points with the aoi #####
obis_points_intersect <- st_intersection(obis_points_transform, beaufort)

##### use this intersection data to select from the original obis data #####
intersects <- obis_points_intersect$id
obis_beaufort_sea <- obis %>% filter(id %in% intersects)

##### write csv #####
write_csv(obis_beaufort_sea,
          "c:/rworking/deepseatools/indata/20230426-0_obis_in_beaufort_sea_RPMcGuinn.csv")

##### write shapefile #####
setwd('c:/rworking/deepseatools/indata')
st_write(obis_points_intersect,
         "obis.shp",
         delete_dsn = T)









##### *****getting OBIS data for Chukchi sea *****#####
library(spocc)
chukchiobis <- st_transform(chukchi, crs = st_crs(points))
bbox <- st_bbox(chukchiobis)
bbox_matrix <- as.matrix(bbox)
bbox = c(bbox_matrix[1,1],
         bbox_matrix[2,1],
         bbox_matrix[3,1],
         bbox_matrix[4,1])

out <- occ(geometry = bbox,
           from = 'obis',
           limit = 10000)
obis <- as.data.frame(out$obis$data)
obis2 <- obis %>% select(id, scientificNameID, recordNumber, latitude, longitude, phylum, family)

##### turning the obis dataset into sf object  #####
obis_points <- st_as_sf(obis2,
                        coords = c("longitude", "latitude"),
                        crs = 4326)

##### transformation of CRS ####
obis_points_transform <- st_transform(obis_points, crs = st_crs(chukchi))

##### intersect the obis points with the aoi #####
obis_points_intersect <- st_intersection(obis_points_transform, chukchi)

##### use this intersection data to select from the original obis data #####
intersects <- obis_points_intersect$id
obis_chukchi_sea <- obis %>% filter(id %in% intersects)

##### write csv #####
write_csv(obis_chukchi_sea,
          "c:/rworking/deepseatools/indata/20230426-0_obis_in_chukchi_RPMcGuinn.csv")

##### write shapefile #####
setwd('c:/rworking/deepseatools/indata')
st_write(obis_points_intersect,
         "obis_chukchi.shp",
         delete_dsn = T)


##### *****getting OBIS data for cook_kodiak *****#####
library(spocc)
cook_kodiakobis <- st_transform(cook, crs = st_crs(points))
bbox <- st_bbox(cook_kodiakobis)
bbox_matrix <- as.matrix(bbox)
bbox = c(bbox_matrix[1,1],
         bbox_matrix[2,1],
         bbox_matrix[3,1],
         bbox_matrix[4,1])

out <- occ(geometry = bbox,
           from = 'obis',
           limit = 10000)
obis <- as.data.frame(out$obis$data)
obis2 <- obis %>% select(id, scientificNameID, recordNumber, latitude, longitude, phylum, family)

##### turning the obis dataset into sf object  #####
obis_points <- st_as_sf(obis2,
                        coords = c("longitude", "latitude"),
                        crs = 4326)

##### transformation of CRS ####
obis_points_transform <- st_transform(obis_points, crs = st_crs(cook))

##### intersect the obis points with the aoi #####
obis_points_intersect <- st_intersection(obis_points_transform, cook)

##### use this intersection data to select from the original obis data #####
intersects <- obis_points_intersect$id
obis_cook_kodiak_sea <- obis %>% filter(id %in% intersects)

##### write csv #####
write_csv(obis_cook_kodiak_sea,
          "c:/rworking/deepseatools/indata/20230426-0_obis_in_cook_kodiak_RPMcGuinn.csv")

##### write shapefile #####
setwd('c:/rworking/deepseatools/indata')
st_write(obis_points_intersect,
         "obis_cook_kodiak.shp",
         delete_dsn = T)

##### look at the DSCRTP data #####
obis_cook_kodiak_sea %>% filter(dataset_id == 'f5a4799e-dc24-4807-89d9-01da47d52e3b') %>% pull(dataset_id)








