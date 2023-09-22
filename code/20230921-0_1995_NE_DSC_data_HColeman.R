##### header #####
## author: Robert McGuinn
## start_date: 20230921
## purpose: inspect corrections
## redmine: https://vlab.noaa.gov/redmine/issues/120781

##### packages #####
library(tidyverse)
library(sf)
library(openxlsx)

##### read NE corrections dataset #####
sub <- read.xlsx('c:/rworking/deepseatools/indata/20230921-0_1995 NE DSC data_HColeman.xlsx')

##### creating sf object for sub #####
projcrs <- 4326
sub_geo <- st_as_sf(x = sub,
                    coords = c("Lon_DD", "Lat_DD"),
                    crs = projcrs)


##### subset the NDB #####
filt_sub <- filt %>% filter(SurveyID == 'GOM_coral_NURC') %>%
  select(ObservationYear, ObservationDate, Vessel, SurveyID, PI, VehicleName, EventID, Latitude, Longitude)
## dim(filt_sub)
## View(filt_sub)

##### creating sf object for ndb #####
projcrs <- 4326
filt_geo <- st_as_sf(x = filt_sub,
                    coords = c("Longitude", "Latitude"),
                    crs = projcrs)


##### write shapefiles #####
path <- "C:/rworking/deepseatools/indata"
setwd(path)

st_write(sub_geo,
         "sub_geo.shp",
         delete_dsn = T)

st_write(filt_geo,
         "filt_geo.shp",
         delete_dsn = T)



##### check #####
2167
3889
3892
3893
2161

yo <- sub %>% filter(grepl('3893', System.Dive.No.))
View(yo)
unique(sub$System.Dive.No.)
