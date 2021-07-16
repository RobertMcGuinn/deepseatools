##### Header #####
## Author: Robert McGuinn
## Started: 20200921

##### packages #####
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)

##### render the QA dashboard #####
# MANUAL CHANGE add the 'AccessionID' of the data set you want to report on as 'x'
filename <- "20200921-2_NOAA_NEFSC_PC1605_Nizinski_2016_2016"

rmarkdown::render("C:/rworking/deepseatools/code/20210303_rmd_accession_qa_dashboard.rmd",
       output_file =  paste(filename,".docx", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

##### MANUAL inspection of QA report in Word, #####
## then SAVE to PDF. Develop Redmine Checklist

##### checking #####
x <- sub %>%
  filter(FlagReason == 'Insufficient taxonomic resolution') %>%
  group_by(ScientificName, FlagReason) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter() %>%
  group_by() %>%
  summarize(n=n())
View(x)

filt %>%
  filter(grepl("Pisces", Vessel)) %>%
  group_by(DatasetID, AccessionID, SurveyID, EventID, Vessel, ObservationYear) %>%
  summarize(n=n()) %>% View()

x <- sub %>%
  group_by(ScientificName, FlagReason) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  group_by(CategoricalAbundance, IndividualCount) %>%
  summarize(n=n())
View(x)

##### Upload PDF report to specific folder on Google Drive #####

## MANUAL CHANGE folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1YHxSB3R3ERUt434GlR8hbQoR4SxeSWWc"

setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)

## checking
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


