##### header #####
## author: Robert McGuinn | robert.mcguinn@noaa.gov | rpm@alumni.duke.edu
## file_start: 20230927
## purpose: calculations of Atlantic EEZ area and no-trawl areas within that boundary for THourigan and HColeman

##### packages ######
library(sf)
library(tidyverse)

##### linkage #####
## manual input here
filename <- '20240202-0_fishcouncilregion_and_PA_areas_for_THourigan_RPMcGuinn_rm126092.R' ## for this  code, include .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues'
## manual input here
issuenumber <- '/126092'
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)


##### load shapefile of protected areas #####
pa <- sf::st_read("c:/rworking/deepseatools/indata/20221104_protected_areas_update_HColeman/20221104_protected_areas.shp")

##### load shapefile of all fish council region ######
fcr <- sf::st_read("C:/rworking/deepseatools/indata/fishery_management_council_regions_20210609/20210609_fishery_management_council_regions.shp")

##### load shapefile of invidual FCRS
ca <- sf::st_read("C:/rworking/deepseatools/indata/FishCouncilRegions_Separate_rmcguinn_20150313/Caribbean.shp")
go <- sf::st_read("C:/rworking/deepseatools/indata/FishCouncilRegions_Separate_rmcguinn_20150313/GOMEX.shp")
ma <- sf::st_read("C:/rworking/deepseatools/indata/FishCouncilRegions_Separate_rmcguinn_20150313/MidAtlantic.shp")
ne <- sf::st_read("C:/rworking/deepseatools/indata/FishCouncilRegions_Separate_rmcguinn_20150313/NewEngland.shp")
se <- sf::st_read("C:/rworking/deepseatools/indata/FishCouncilRegions_Separate_rmcguinn_20150313/SouthAtlantic.shp")

##### Transform the sf object to a CRS that uses meters as the unit of measurement#####
ca_t <- st_transform(ca, crs = st_crs("+proj=utm +zone=33 +datum=WGS84"))
go_t <- st_transform(go, crs = st_crs("+proj=utm +zone=33 +datum=WGS84"))
ma_t <- st_transform(ma, crs = st_crs("+proj=utm +zone=33 +datum=WGS84"))
ne_t <- st_transform(ne, crs = st_crs("+proj=utm +zone=33 +datum=WGS84"))
se_t <- st_transform(se, crs = st_crs("+proj=utm +zone=33 +datum=WGS84"))
fcr_t <- st_transform(fcr, crs = st_crs("+proj=utm +zone=33 +datum=WGS84"))
pa_t <- st_transform(pa, crs = st_crs("+proj=utm +zone=33 +datum=WGS84"))

##### Calculate the area in square kilometers #####
ca_area <- st_area(ca_t) / 1e6
go_area <- st_area(go_t) / 1e6
ma_area <- st_area(ma_t) / 1e6
ne_area <- st_area(ne_t) / 1e6
se_area <- st_area(se_t) / 1e6
fcr_area <- st_area(fcr_t) / 1e6
pa_area <- st_area(pa_t) / 1e6

all <- ca_area + go_area + ma_area + ne_area + se_area

##### check #####
pa_area
ca_area
go_area
ma_area
ne_area
se_area
all

##### create a union of all of the selected EEZ#####
atl_eez <- st_union(ca_t,
                      go_t,
                      ma_t,
                      ne_t,
                      se_t)
##### check #####
plot(atl_eez)

###### clip protected areas with selected eez #####
pa_in_EEZ <- st_intersection(pa_t, atl_eez)

###### select only ones with bottom trawl listed in GearProhib #####
pa_in_EEZ_no_trawl <- pa_in_EEZ %>%
  filter(GearProhib != 'Pots, traps, bottom longlines, gillnets or trammel nets')
pa_in_EEZ_no_trawl <- pa_in_EEZ_no_trawl %>%
  filter(GearProhib != 'Pots, traps, bottom longlines, gillnets or trammel nets, and anchoring by fishing vessels')


###### calculate area #####
pa_in_EEZ_no_trawl_area <- st_area(pa_in_EEZ_no_trawl) / 1e6
total_area_in_no_trawl <- sum(pa_in_EEZ_no_trawl_area)

##### check #####
View(pa_in_EEZ_no_trawl_area)
table(pa_in_EEZ$GearProhib, useNA = 'always')
plot(pa_in_EEZ)

##### export shapefile #####
st_write(points, "C:/Users/Robert.Mcguinn/Documents/ArcGIS/Projects/20230511-0_ISDSC8_Scotland_RPMcGuinn/shapefiles/sub_geo.shp", delete_dsn = T)



