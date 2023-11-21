##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20231121
## purpose: see redmine issue

##### linkage #####
## manual input here
filename <- '20231121-0_Laurence_Helene_De_Clippele_RPMcGuinn.R' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
## manual input here
issuenumber <- '122876'
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)

##### import dataset #####
setwd('c:/rworking/deepseatools/indata')
filename <- 'JC073_ROV_species_enviromental_variables_40m.tab'
sub <- read.delim(filename,
           sep='\t',
           header = T,
           skip = 50)

##### transform dataset #####
sub2 <- sub %>%
  pivot_longer(cols = Antipatharia.sp.......Antipatharia.sp..1.:Bathynectes.sp.....,
               names_to = "ScientificName",
               values_to = "IndividualCount")

##### check #####
View(sub2)

##### fix data #####
## for a certain subset, the lat and long has been switched
## identify records where latitude and longitude are flipped
flipped_records <- sub2$Latitude < 55

# Swap latitude and longitude for flipped records
sub2[flipped_records, c("Latitude", "Longitude")] <- sub2[flipped_records, c("Longitude", "Latitude")]

##### create sf object #####
points <- st_as_sf(sub2, coords = c("Longitude", "Latitude"), crs = 4326)

##### export shapefile #####
st_write(points, "C:/rworking/deepseatools/indata/geo3.shp")

##### check #####








