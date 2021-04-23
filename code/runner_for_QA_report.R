##### Header #####
##### Author: Robert McGuinn #####
##### Started: 20200921

##### packages #####
library(curl)
library(rmarkdown)
library(googledrive)

##### render the QA dashboard #####
# MANUAL CHANGE add the 'AccessionID' of the data set you want to report on as 'x'
filename <- "20210405-0_NOAA_NEFSC_HB1404_Nizinski_2014_2014"

rmarkdown::render("C:/rworking/deepseatools/code/20210303_rmd_accession_qa_dashboard.rmd",
       output_file =  paste(filename,".docx", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

###### MANUAL inspection of QA report in Word, then save to PDF. Develop Redmine Checklist #####

##### checking #####
filt %>% filter(grepl("Okeanos", Vessel)) %>% pull(Citation) %>% table()
# filt %>% filter(grepl("Greater Farallones", DataProvider)) %>% pull(DataProvider) %>% table()
# filt %>% filter(grepl("National Centers for Coastal Ocean Science", DataProvider)) %>% pull(DataProvider) %>% table()
sub %>% filter(is.na(VernacularNameCategory) == T) %>% pull(ScientificName) %>% table()
sub %>% filter(SampleID == "EX1903L2_D01_01B_A11") %>% pull(TrackingID) %>% table()
sub %>% filter(VernacularNameCategory == "sea fan") %>% pull(ScientificName) %>% table()
s %>% filter(FieldName == 'VernacularNameCategory') %>% pull(ValidValues)
sub %>% filter(FlagReason == "Insufficient taxonomic resolution") %>% pull(VernacularNameCategory) %>% table()

filt %>%
  filter(grepl("Bigelow", Vessel)) %>%
  pull(Vessel) %>%
  table()

sub %>%
  pull(SurveyID) %>%
  table()

filt %>%
  filter(grepl("NOAA", DatasetID)) %>%
  pull(DatasetID) %>%
  table()

sub %>%
  filter(VernacularNameCategory == 'check with dataprovider') %>%
  pull(ScientificName) %>%
  table()

x <- sub %>%
  filter(DepthInMeters == 0) %>%
  pull(CatalogNumber) %>%
  length()

sub %>%
  filter(grepl("taxonomic", FlagReason)) %>%
  group_by(Flag, FlagReason, ScientificName) %>%
  summarize(n=n())

sub %>%
  filter(grepl("taxonomic", FlagReason)) %>%
  pull(CatalogNumber) %>%
  length()

filt %>%
  filter(ScientificName == "Bathyalcyon robustum") %>%
  group_by(ScientificName) %>%
  summarize(n=n())

##### Upload PDF report to specific folder on Google Drive #####

## MANUAL CHANGE folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1xWnfaSoEdrEIcC4-Is5oLooDmslzzy8I"

setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)


##### ##export to GIS## #####
##### load packages #####
library(tidyverse)
library(rgdal)
library(arcgisbinding)
arc.check_product()

##### create x from sub #####
x <- sub

##### filter data #####
# get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 | Longitude != -999)
# make copy to turn into spatial points data frame.
x_geo <- x

##### create spatial points data frame #####
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### create feature-class #####
fgdb_path <- 'C:/rworking/sf/sf.gdb'
arc.write(file.path(fgdb_path, 'x_geo'), data=x_geo, overwrite = TRUE)

