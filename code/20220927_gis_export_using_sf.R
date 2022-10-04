##### Header #####
## Author: Robert McGuinn, rpm@alumni.duke.edu, robert.mcguinn@noaa.gov
## Date started: 20220927
## Purpose: bring in NDB lat/long points and export them to shapefile

##### packages #####
library(tidyverse)
library(sf)

##### authorizations #####
drive_auth(email = "robert.mcguinn@noaa.gov")
gs4_auth(email = "robert.mcguinn@noaa.gov")

##### read current database from disk #####
ndb_all <- read.csv("C:/rworking/deepseatools/indata/DSCRTP_NatDB_20220801-0.csv", header = T)

##### filter out flagged data #####
ndb_filt <- ndb_all %>%
  filter(Flag == "0",
         Latitude != -999 |
           Longitude != -999,
  )

##### cleanup #####
# rm(ndb_all)

##### check #####
# yo <- ndb_filt %>%
#   group_by(Flag) %>%
#   summarize(n=n()) %>% View()

##### make copy to turn into spatial points data frame #####
ndb_filt_geo <- ndb_filt

##### filter data by bounding box #####
## specify a geographic bounding box
minLon <- -71
maxLon <- -65
minLat <- 40
maxLat <- 46

## using the box to filter the data
ndb_filt_geo <- ndb_filt_geo %>% filter(as.numeric(Latitude) > minLat,
                                              as.numeric(Latitude) < maxLat,
                                              as.numeric(Longitude) < maxLon,
                                              as.numeric(Longitude) > minLon)
##### check #####
ndb_filt_geo %>%
  group_by(VernacularNameCategory, ScientificName) %>%
  summarize(n=n())

table(ndb_filt_geo$VernacularNameCategory, useNA = 'always')

##### recode of DSC 'VerncacularNameCategory' to 'StructuralQuery' #####
# recode_list <- list('alcyonacean (unspecified)' = 'structural_corals',
#                     'black coral' = 'structural_corals',
#                     'calcareous sponge' = 'structural_other',
#                     'demosponge' = 'structural_other',
#                     'glass sponge' = 'structural_other',
#                     'gorgonian coral' = 'structural_corals',
#                     'sea pen' = 'mask',
#                     'soft coral' = 'nonstructural',
#                     'sponge (unspecified)' = 'structural_other',
#                     'stoloniferan coral' = 'nonstructural',
#                     'stony coral (branching)' = 'structural_corals',
#                     'stony coral (cup coral)' = 'nonstructural',
#                     'stony coral (unspecified)' = 'nonstructural',
#                     'homoscleromorph sponge'= 'structural_other',
#                     'lace coral' = 'mask'
#                     )
#
# ndb_filt_geo$StructuralQuery <- recode(ndb_filt_geo$VernacularNameCategory, !!!recode_list)

##### check ######
# table(ndb_filt_geo$StructuralQuery, useNA = 'always')

##### filter ######
# ndb_filt_geo <- ndb_filt_geo %>% filter(StructuralQuery != 'mask')

##### create simple features object data frame #####
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
ndb_filt_geo_sf <- st_as_sf(x = ndb_filt_geo,
                     coords = c("Longitude", "Latitude"),
                     crs = projcrs)


##### thin down the variables #####
ndb_filt_geo_export <- ndb_filt_geo_sf %>%
  dplyr::select(CatalogNumber,
                Phylum,
                ScientificName,
                SampleID,
                SurveyID,
                EventID,
                Flag,
                ImageURL
          )

##### export data it will guess the driver automatically based on the .shp extension #####
st_write(ndb_filt_geo_export,
         "C:/data/gis_data/20220926-0_gulf_of_maine_RFI_DPacker/mapping_RPMcGuinn/export.shp",
         delete_dsn = T)



