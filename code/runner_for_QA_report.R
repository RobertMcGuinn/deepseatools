##### Header #####
## Author: Robert McGuinn
## Started: 20200921

##### packages #####
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)
library(googlesheets4)

##### authorization

##### authorizations #####
drive_auth(email = "robert.mcguinn@noaa.gov")
gs4_auth(email = "robert.mcguinn@noaa.gov")

##### render the QA dashboard #####
## MANUAL CHANGE: add the 'AccessionID' of the data set you want to report on as 'x'

filename <- "20220607-3_ROPOS_2019_Dive_2114"

## render
rmarkdown::render("C:/rworking/deepseatools/code/20220629_rmd_accession_qa_dashboard.rmd",
       output_file =  paste(filename,".docx", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

## manual change: make sure your target RMD in the render function step is correct.
##### MANUAL inspection of QA report in Word, #####
## manual: then Develop Redmine Checklist
## manual: then SAVE to PDF.
##### upload PDF report to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1FMh4GKhVV6q5r5Ri1K-72HE38JT_1Wl1"

setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)

##### checking #####
unique(sub$DatasetID)
unique(sub$SurveyID)
unique(sub$Citation)
unique(sub$Repository)

x <- filt %>%
  filter(DatasetID == 'MBARI') %>%
  group_by(DataContact, DataProvider, ImageURL) %>%
  summarize(n=n())
View(x)

x <- filt %>%
  filter(FishCouncilRegion == "Caribbean" |
           FishCouncilRegion == "South Atlantic" |
           FishCouncilRegion == "Caribbean") %>%
  group_by(FishCouncilRegion, Vessel, EntryDate, ObservationYear) %>%
  summarize(n=n())
View(x)

x <- filt %>%
  filter(grepl("Monterey", DataProvider)) %>%
  group_by(ImageURL) %>%
  summarize(n=n())
View(x)

setwd("C:/rworking/deepseatools/indata")
sub <- read.csv("dsc_natdb.csv", header = TRUE)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

x <- filt %>%
  filter(FishCouncilRegion == "Caribbean" |
           FishCouncilRegion == "South Atlantic" |
           FishCouncilRegion == "Gulf of Mexico",
         as.Date(EntryDate) > "2019-10-01") %>%
  group_by(FishCouncilRegion, Vessel, VehicleName, EntryDate, ObservationYear) %>%
  summarize(n=n())
View(x)
length(x$CatalogNumber)

x <- filt %>%
  filter(grepl("NA", DatasetID)) %>%
  group_by(Vessel, DatasetID, DataProvider) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter(CategoricalAbundance == 'minimum count') %>%
  group_by(ScientificName, FlagReason, IndividualCount) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter() %>%
  group_by() %>%
  summarize(n=n())
View(x)

filt %>%
  filter(grepl("Deep Sea Coral", Repository)) %>%
  group_by(Repository, DatasetID) %>%
  summarize(n=n()) %>% View()

filt %>%
  filter(grepl("YOGI", VehicleName)) %>%
  group_by(DataProvider, VehicleName) %>%
  summarize(n=n()) %>% View()

x <- sub %>%
  filter(DepthInMeters < 50) %>%
  group_by(ScientificName, FlagReason, DepthInMeters, DepthMethod, ShallowFlag) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  group_by(CategoricalAbundance, IndividualCount) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter(VernacularNameCategory == "nipple foliose sponge (yellow)") %>%
  group_by(Flag, FlagReason, ScientificName) %>%
  summarize(n=n())
View(x)

s %>% filter(FieldName == "Modified") %>% pull(ValidValues)
s %>% filter(FieldName == "IdentificationVerificationStatus") %>% pull(ValidValues)
s %>% filter(FieldName == "IdentificationVerificationStatus") %>% pull(FieldDescription)
s %>% filter(FieldName == "TaxonRank") %>% pull(ValidValues)

##### checking #####
sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(RecordType, SamplingEquipment, Repository, ) %>%
  summarize(n=n()) %>%
  View()

sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(IndividualCount, CategoricalAbundance) %>%
  summarize(n=n()) %>%
  View()

filt %>%
  filter(grepl("Natural History", DataProvider)) %>%
  group_by(DataProvider) %>%
  summarize(n=n()) %>%
  View()

sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(ObservationDate) %>%
  summarize(n=n()) %>%
  View()

sub %>%
  filter(grepl("Desmophyllum", ScientificName)) %>%
  group_by(ScientificName) %>%
  summarize(n=n()) %>%
  View()

filt %>%
  filter(grepl("140", SurveyID)) %>%
  group_by(Vessel, SurveyID, AccessionID, DatasetID, DataProvider, SamplingEquipment ) %>%
  summarize(n=n()) %>%
  View()


x <- s %>%
  filter(FieldName == 'LocationAccuracy') %>%
  pull(ValidValues) %>%
View()

x <- s %>%
  filter(FieldName == 'OtherData') %>%
  pull(FieldDescription) %>%
  View()


##### export points and dive centers to GIS #####
## load packages
library(tidyverse)
library(rgdal)
library(arcgisbinding)
arc.check_product()

## create x from sub
x <- sub

## filter data
# get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 | Longitude != -999)
# make copy to turn into spatial points data frame.
x_geo <- x

## create spatial points data frame
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

## create feature-class
fgdb_path <- 'C:/rworking/sf/sf.gdb'
arc.write(file.path(fgdb_path, 'x_geo'), data=x_geo, overwrite = TRUE)

### Export dive points

## create summary by EventID
x <- sub %>% filter(Flag == 0) %>%
  group_by(EventID) %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude)
  )

## get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 |
                    Longitude != -999)

## make copy to turn into spatial points data frame.
x_geo <- x

## create spatial points data frame
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

## create feature-class

fgdb_path <- 'C:/rworking/sf/sf.gdb'
arc.write(file.path(fgdb_path, 'x_geo_dives'), data=x_geo, overwrite = TRUE)


