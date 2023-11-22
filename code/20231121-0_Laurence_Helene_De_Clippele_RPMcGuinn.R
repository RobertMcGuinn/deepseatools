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
# View(sub2)

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
# filt %>% filter(grepl('James Cook', Vessel)) %>%
#   pull(Vessel) %>% table()
#
# filt %>% filter(grepl('James Cook', Vessel)) %>%
#   pull(SurveyID) %>% table()
#
# filt %>% filter(grepl('Roberts', PI)) %>%
#   pull(PI) %>% table()
#
# filt %>% filter(RecordType == 'literature') %>%
#   pull(DataProvider) %>% unique()

##### add information from DSCRTP schema #####
sub2$Vessel <- "James Cook R/V"
sub2$SurveyID <- "JC073"
sub2$VehicleName <- "Holland 1"
sub2$SamplingEquipment <- "ROV"
sub2$Citation <- "https://www.frontiersin.org/articles/10.3389/fmars.2019.00184/full | https://www.bodc.ac.uk/resources/inventories/cruise_inventory/reports/jc073.pdf"
sub2$WebSite <- "https://www.frontiersin.org/articles/10.3389/fmars.2019.00184/full | https://www.bodc.ac.uk/resources/inventories/cruise_inventory/reports/jc073.pdf | https://www.bodc.ac.uk/resources/inventories/cruise_inventory/report/11421/ | | http://rvinfobase.eurocean.org/spec/vessel.jsp?id=4140"
sub2$RecordType <- "video observation"
sub2$NavType <- "USBL"
sub2$LocationAccuracy <- "20m"
sub2$ObservationDate <- "2012-05-18" ## May 18–June 15, 2012
sub2$Locality <- "Logachev Mound Province"
sub2$Habitat <- sub2$Stones......Dropstone.
sub2$DepthInMeters <- sub2$Depth.water..m.
sub2$PI <- "De Clippele, Laurence Helene"
sub2$PIAffiliation <- "University of Edinburgh, Edinburgh, United Kingdom"
sub2$DataProvider <- "De Clippele et al. 2019"
sub2$DataContact <- "De Clippele, Laurence Helene"
sub2$Repository <- "National Oceanography Centre, British Oceanographic Data Centre"
sub2 <- sub2 %>% mutate(CategoricalAbundance = ifelse(IndividualCount == 0, 'absent', 'present'))



