##### Header #####
## Author: Robert McGuinn
## Started: 20200921

##### packages #####
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)

##### render the QA dashboard #####
## MANUAL CHANGE: add the 'AccessionID' of the data set you want to report on as 'x'
## manual change: make sure your target RMD in the render function step is correct.
filename <- "20211019-3_NOAA_AFSC_Aleutian_DropCam_Rooper_Stellar_Sea_Lion_2016_2017"

## render
rmarkdown::render("C:/rworking/deepseatools/code/20220309_rmd_accession_qa_dashboard.rmd",
       output_file =  paste(filename,".docx", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

##### MANUAL inspection of QA report in Word, #####
## manual: then SAVE to PDF.
## manual: then Develop Redmine Checklist

##### Upload PDF report to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1WcL_fMRU7SXpsc_60OWd5XzRQBvyCJOz"
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

x <- sub %>%
  group_by(Purpose, EventID, Locality) %>%
  summarize(n=n())
View(x)


x <- sub %>%
  filter(FlagReason == 'Insufficient taxonomic resolution') %>%
  group_by(ScientificName, FlagReason) %>%
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
  filter(grepl("", DataProvider)) %>%
  group_by(DataProvider, DatasetID) %>%
  summarize(n=n()) %>% View()


x <- sub %>%
  group_by(ScientificName, FlagReason) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  group_by(CategoricalAbundance, IndividualCount) %>%
  summarize(n=n())
View(x)

s %>% filter(FieldName == "IdentificationVerificationStatus") %>% pull(ValidValues)
s %>% filter(FieldName == "IdentificationVerificationStatus") %>% pull(FieldDescription)
s %>% filter(FieldName == "TaxonRank") %>% pull(ValidValues)


##### checking #####
sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(IndividualCount, CategoricalAbundance) %>%
  summarize(n=n()) %>%
  View()

sub %>%
  filter(grepl("Sentry", SamplingEquipment)) %>%
  group_by(SamplingEquipment, Vessel) %>%
  summarize(n=n()) %>%
  View()

sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(ObservationDate) %>%
  summarize(n=n()) %>%
  View()

filt %>%
  filter(grepl("140", SurveyID)) %>%
  group_by(Vessel, SurveyID, AccessionID, DatasetID, DataProvider, SamplingEquipment ) %>%
  summarize(n=n()) %>%
  View()


x <- s %>%
  filter(FieldName == 'IdentificationVerificationStatus') %>%
  pull(FieldDescription) %>%
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


