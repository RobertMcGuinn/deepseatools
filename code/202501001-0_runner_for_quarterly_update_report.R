##### Header #####
## Author: Robert McGuinn
## Started: 20250723

##### packages #####
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)

##### render the QA dashboard #####
## set filename that you want to call the report
filename <- "20251001-0_quarterly_update_report"

## render
## set target to render
rmarkdown::render("C:/rworking/deepseatools/code/20251001-0_rmd_quarterly_report_for_database_update.rmd",
       output_file =  paste(filename,".docx", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

##### MANUAL inspection of QA report in Word, #####
## manual: then SAVE to PDF.
## manual: then Develop Redmine Checklist

##### Upload PDF report to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1KPK1YI-n7EHNuOIKfZJM_EsaDCAUQOl8"

setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)

##### checking #####
yo <- filt %>% filter(AphiaID == -999)
length(yo$CatalogNumber)
unique(yo$ScientificName)
unique(yo$DatasetID)
unique(yo$SurveyID)
unique(sub$Citation)
unique(sub$Repository)
filt %>% filter(grepl("Brooke", PI)) %>% pull(ObservationYear) %>% unique()
filt %>% filter(AphiaID == -999) %>% pull(ScientificName) %>% table(useNA = 'always')
filt %>% filter(AphiaID == '-999') %>% pull(Class) %>% table(useNA = 'always')
filt %>% filter(ScientificName == 'Haliclona (Reniera)') %>% pull(AphiaID)
filt %>% filter(ScientificName == 'Flabellum (Flabellum) oclairi') %>% pull(AphiaID)

##### export points GIS #####
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

##### export dive points to GIS #####
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


