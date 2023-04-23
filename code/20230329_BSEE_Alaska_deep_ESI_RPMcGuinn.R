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
                 col_types = cols(.default = "c"),
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








