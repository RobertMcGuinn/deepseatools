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
library(googlesheets4)

##### authorizations #####
gs4_auth(email = "robert.mcguinn@noaa.gov")

##### import dataset #####
setwd('c:/rworking/deepseatools/indata')
filename <- 'JC073_ROV_species_enviromental_variables_40m.tab'
sub <- read.delim(filename,
           sep='\t',
           header = T,
           skip = 50)

##### import schema from google drive #####
s <- read_sheet('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')

##### transform data set #####
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
st_write(points,
         "C:/rworking/deepseatools/indata/geo3.shp",
         append = F)

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
#
# filt %>% filter(RecordType == 'literature') %>%
#   pull(DatasetID) %>% unique()

##### add information from DSCRTP schema #####
sub2$Vessel <- "James Cook R/V"
sub2$SurveyID <- "JC073"
sub2$VehicleName <- "Holland 1"
sub2$SamplingEquipment <- "ROV"
sub2$Citation <- "https://www.frontiersin.org/articles/10.3389/fmars.2019.00184/full | https://www.bodc.ac.uk/resources/inventories/cruise_inventory/reports/jc073.pdf"
sub2$WebSite <- "https://www.frontiersin.org/articles/10.3389/fmars.2019.00184/full | https://www.bodc.ac.uk/resources/inventories/cruise_inventory/reports/jc073.pdf | https://www.bodc.ac.uk/resources/inventories/cruise_inventory/report/11421/ | | http://rvinfobase.eurocean.org/spec/vessel.jsp?id=4140 | https://doi.pangaea.de/10.1594/PANGAEA.909118"
sub2$RecordType <- "video observation"
sub2$NavType <- "USBL"
sub2$LocationAccuracy <- "20m"
sub2$ObservationDate <- "2012-05-18" ## May 18–June 15, 2012
sub2$Locality <- "Logachev Mound Province"
sub2$Habitat <- sub2$Stones......Dropstone.
sub2$DepthInMeters <- sub2$Depth.water..m.
sub2$MinimumDepthInMeters <- sub2$Depth.water..m.
sub2$MaximumDepthInMeters <- sub2$Depth.water..m.
sub2$PI <- "De Clippele, Laurence Helene"
sub2$PIAffiliation <- "University of Edinburgh, Edinburgh, United Kingdom"
sub2$DataProvider <- "De Clippele et al. 2019"
sub2$DatasetID <- "De_Clippele_etal_2019"
sub2$DataContact <- "De Clippele, Laurence Helene"
sub2$Repository <- "National Oceanography Centre, British Oceanographic Data Centre"
sub2 <- sub2 %>% mutate(CategoricalAbundance = ifelse(IndividualCount == 0, 'absent', 'present'))
sub2$VerbatimScientificName <- sub2$ScientificName
sub2$ScientificName <- 'NA'
sub2$SampleID <- 'NA'
sub2$DepthMethod <- 'reported'
sub2$IdentifiedBy <- 'De Clippele, Laurence Helene'
sub2$EventID <- 'NA'
sub2$Modified <- '2023-11-22'
sub2$Reporter <- 'McGuinn, Robert P.'
sub2$ReporterEmail <- 'robert.mcguinn@noaa.gov'
sub2$AccessionID <- 'National_Oceanography_Centre_James_Cook_JC073_2012'

##### add AphiaID #####
sub2 <- sub2 %>%
  mutate(
    AphiaID = case_when(
      ScientificName == "Antipatharia.sp.......Antipatharia.sp..1." ~ "22549",
      ScientificName == "Antipatharia.sp.......Antipatharia.sp..2." ~ "22549",
      ScientificName == "Paramuricea.sp....." ~ "125311",
      ScientificName == "Trissopathes.sp.......Trissopathes.sp..1." ~ "267926",
      ScientificName == "Acanella.sp....." ~ "125303",
      ScientificName == "Leiopathes.sp.......small." ~ "103305",
      ScientificName == "Leiopathes.sp.......medium." ~ "103305",
      ScientificName == "Leiopathes.sp.......large." ~ "103305",
      ScientificName == "Bathypathes.sp....." ~ "103304",
      ScientificName == "Stichopathes......Stichopathes.cf..Gravieri." ~ "103308",
      ScientificName == "Parantipathes.sp.......Parantipathes.sp..1." ~ "103306",
      ScientificName == "Parantipathes.sp.......Parantipathes.sp..2." ~ "103306",
      ScientificName == "Shrimps...." ~ "106674",
      ScientificName == "Hydrozoa...." ~ "1337",
      ScientificName == "Gastroptyctus.sp....." ~ "106832",
      ScientificName == "Shark.egg...." ~ "1517375",
      ScientificName == "Asteroidea.sp....." ~ "123080",
      ScientificName == "P..cuvieri...." ~ "107264",
      ScientificName == "Ophiuroidea...." ~ "123084",
      ScientificName == "Sponge.r......Massive.red.sponge." ~ "558",
      ScientificName == "Crinoidea......Crinoidea.sp..1." ~ "123081",
      ScientificName == "Crinoidea......Crinoidea.sp..2." ~ "123081",
      ScientificName == "Crinoidea......Crinoidea.sp..3." ~ "123081",
      ScientificName == "C..cidaris...." ~ "124257",
      ScientificName == "Munida.sp....." ~ "106835",
      ScientificName == "C..affinis...." ~ "107369",
      ScientificName == "Bathynectes.sp....." ~ "106920",
      TRUE ~ ""  # default case, if none of the above conditions are met
    )
  )

##### check #####
# table(sub2$AphiaID, useNA = 'always')
# setdiff(names(sub2), s$FieldName)
# x <- s %>% filter(PointNew == "R") %>% pull(FieldName)
# setdiff(x, names(sub2))

##### get rid of un-wanted variables #####
sub2 <- sub2 %>%
  select(-c(
    "BPI...i6xo9..",
    "BPI...i3xo6..",
    "Rugosity...i9xo9..",
    "Rugosity...i3xo3..",
    "Aspect..arbitrary.units...Northness.",
    "Aspect..arbitrary.units...Eastness.",
    "Aspect..arbitrary.units.",
    "Backsc",
    "Depth.water..m.",
    "Stones......Dropstone."
  ))

##### check ####
# setdiff(s$FieldName, names(sub2))
# setdiff(names(sub2),s$FieldName)

##### write CSV #####
write.csv(sub2,
          "c:/rworking/deepseatools/indata/20231122-0_National_Oceanography_Centre_James_Cook_JC073_2012.csv")

