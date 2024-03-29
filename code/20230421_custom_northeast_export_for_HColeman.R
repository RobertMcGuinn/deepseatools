##### Header #####
## author: Robert McGuinn
## date started: 20230421
## forkedfrom: none
## drive: https://drive.google.com/drive/folders/1BSXMVD5khNni1lvwKaT8posa71HwpUJ7?usp=share_link
## purpose: Northeast data extract of existing and currently in queue datasets.

##### packages #####
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(sf)

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

##### load national database #####
setwd("C:/rworking/deepseatools/indata")
filename <- "DSCRTP_NatDB_20221213-0.csv"
indata<-read_csv(filename,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = 'latin9'),
                 na = c("-999", "NA"))

filt <- indata %>%
  filter(Flag == 0)

rm(indata)
rm(filename)

##### load data in queue #####
## next file
path <- "C:/rworking/deepseatools/indata/"
filename <- "20230420-0_NOAA_EX1304_Northeast_US_SBingo_2013"
string <- paste(path,filename,".csv",sep = '')
sub <- read_csv(string)
flagged <- sub %>%  filter(Flag == "1")

## next file
path <- "C:/rworking/deepseatools/indata/"
filename <- "20230420-0_EX-14-04-L2-L3"
string <- paste(path,filename,".csv",sep = '')
sub2 <- read_csv(string)
flagged <- sub %>%  filter(Flag == "1")

##### rbind data together #####
filt2 <- rbind(filt,sub)
filt2 <- rbind(filt2,sub2)

##### clean lat/long #####
filt2 <- filt2 %>%
  filter(Latitude != '-999' , Longitude != '-999')

##### creating a geographic bounding box (Northeasten US Atlantic Ocean) #####
minLon <- -77
maxLon <- -54
minLat <- 33
maxLat <- 48

##### filtering data by bounding box #####
geofilt <-
  filt2 %>% filter(as.numeric(Latitude) > minLat,
                  as.numeric(Latitude) < maxLat,
                  as.numeric(Longitude) < maxLon,
                  as.numeric(Longitude) > minLon)

##### creating sf object #####
projcrs <- 4326
geosub <- st_as_sf(x = geofilt,
                   coords = c("Longitude", "Latitude"),
                   crs = projcrs)

##### write shapefile #####
st_write(geosub,
         "C:/Users/Robert.Mcguinn/Documents/ArcGIS/Projects/20230421_custom_northeast_export_for_HColeman_RPMcGuinn/shapefiles/mapdata.shp",
         delete_dsn = T)

