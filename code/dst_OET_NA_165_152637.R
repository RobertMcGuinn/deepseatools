##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20251208
## purpose: data submission, see Redmine
##### linkage #####
filename <- 'dst_data_OET_NA_165_152637' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- issuenumber <- sub(".*_(.*)$", "\\1", filename)
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(leaflet)
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)
library(robis)
library(googledrive)
library(readr)

##### authenticate #####
# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### ***** ORIGINAL 20251120 ***** #####
## https://drive.google.com/file/d/15GtU8Zsrdrp6OkIPCQFrx9W3cZraUkxW/view?usp=drive_link
## 'NA165_DARC_Annorations_Full_20251120'
##### load data set from TSV stored on Google Drive #####
googleid <- "15GtU8Zsrdrp6OkIPCQFrx9W3cZraUkxW"

## download to a temporary file
tmp <- tempfile(fileext = ".tsv")
drive_download(as_id(googleid), path = tmp, overwrite = TRUE)

## read the TSV (set all col types to character)
sub <- read_tsv(tmp, col_types = cols(.default = "c"))

##### check #####
names(df)
spec(df)
dim(df)

##### map-it using leaflet #####
## optional create new 'sub' ##
sub2 <- sub
## sub2$Longitude <- as.numeric(sub$Longitude)-6
sub2 <- sub2 %>% filter(Latitude != -999 |
                          Longitude != -999)

## set Lat and Lon to numeric
sub2$Latitude <- as.numeric(sub2$Latitude)
sub2$Longitude <- as.numeric(sub2$Longitude)

# filter(CatalogNumber == "1178074")
m <- leaflet()
m <- addProviderTiles(m, "Esri.NatGeoWorldMap")
m <- addCircleMarkers(m, data=sub2,
                      radius=2,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=.5,
                      popup = paste(
                        "<b><em>","Flag:","</b></em>", sub2$Flag, "<br>",
                        "<b><em>","Catalog Number:","</b></em>", sub2$CatalogNumber, "<br>",
                        "<b><em>","Record Type:","</b></em>", sub2$RecordType, "<br>",
                        "<b><em>","DatasetID:","</b></em>", sub2$DatasetID, "<br>",
                        "<b><em>","AccessionID:","</b></em>", sub2$AccessionID, "<br>",
                        "<b><em>","DataProvider:","</b></em>", sub2$DataProvider, "<br>",
                        "<b><em>","ObservationYear:","</b></em>", sub2$ObservationYear, "<br>",
                        "<b><em>","Vessel:","</b></em>", sub2$Vessel, "<br>",
                        "<b><em>","Locality:","</b></em>", sub2$Locality, "<br>",
                        "<b><em>","Scientific Name:","</b></em>", sub2$ScientificName, "<br>",
                        "<b><em>","Depth (meters):","</b></em>", sub2$DepthInMeters, "<br>",
                        "<b><em>","Survey ID:","</b></em>", sub2$SurveyID, "<br>",
                        "<b><em>","Event ID:","</b></em>", sub2$EventID, "<br>",
                        "<b><em>","ImageFilePath:","</b></em>", sub2$ImageFilePath, "<br>",
                        "<b><em>","ImageURL:","</b></em>", sub2$ImageURL, "<br>",
                        "<b><em>","Latitude:","</b></em>", sub2$Latitude, "<br>",
                        "<b><em>","Longitude:","</b></em>", sub2$Longitude))

m



