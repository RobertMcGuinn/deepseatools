##### Heading #####
# author: Robert P. McGuinn
# date started: 20181107
# purpose: collection of scripts useful to DSCRTP database quality assurance

##### Installation/Loading of Packages #####
# install.packages('xlsx')
#install.packages('openxlsx')
library(openxlsx)
library(sp)
library(tidyverse)
library(rerddap)
#install.packages('leaflet')
library(leaflet)
# install.packages('extrafont')
library(extrafont)
# install.packages('RColorBrewer')
library(RColorBrewer)
# install.packages('googlesheets')
library(googlesheets)
# install.packages('googledrive')
library(googledrive)
library(rmarkdown)
library(knitr)
#install.packages("maps")
library(maps)
#install.packages("rgdal")
library(rgdal)
#install('raster')
library(raster)
#install.packages("spocc")
library(spocc)
#install.packages('arcgisbinding')
library(arcgisbinding)
arc.check_product()
#install.packages('refinr')
library(refinr)
# install.packages('marmap')
library(marmap) #yo
#install.packages('prettydoc')
library(prettydoc)
#install.packages('robis')
library(robis)
#install.packages('devtools')
library(devtools)
library(httr)
library(jsonlite)

##### _____ Bringing in database #####

# setwd("C:/rworking/digs/indata")
# indata1<-read.csv("DSCRTP_NatDB_20190620-0.csv", header = T) #DSCRTP_NatDB_20181005-0.csv # DSCRTP_NatDB_20181211-2.csv
# filt1 <- indata1 %>%
#   filter(Flag == "0")

setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20191217-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

# save.image("C:/rworking/deepseatools/.RData")

# # save the R workspace
# save.image("C:/rworking/deepseatools/.RData")
# #from table delimited text file (from David Sallis on 20190307)
# indata <- read.table("DSCRTP_NatDB_20190306-0.txt", header = T, sep="\t", fill = TRUE)

##### _____ Create standardized tables of key variables at latest DB version #####
# IdentificationQualifier

version <- '20190920-0'

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(IdentificationQualifier) %>%
  summarise(
    Change_IdentificationQualifier = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    # DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    # ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )

setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("IdentificationQualifier_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("IdentificationQualifier_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

# SurveyID

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(SurveyID) %>%
  summarise(
    Change_SurveyID = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    # DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    #ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )

setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("SurveyID_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("SurveyID_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

# DataProvider

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(DataProvider) %>%
  summarise(
    Change_DataProvider = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    # DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    #ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )


setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("DataProvider_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("DataProvider_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

# DatasetID

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(DatasetID) %>%
  summarise(
    Change_DatasetID = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    # DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    #ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )

setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("DatasetID_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("DatasetID_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

# Repository

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(Repository) %>%
  summarise(
    Change_Repository = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    #Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    #ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )


setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("Repository_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("Repository_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

# Vessel

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(Vessel) %>%
  summarise(
    Change_Vessel = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    #Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    #ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )

setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("Vessel_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("Vessel_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

# VehicleName

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(VehicleName) %>%
  summarise(
    Change_VehicleName = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    # VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    # ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )


setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("VehicleName_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("VehicleName_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

# PI

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(PI) %>%
  summarise(
    Change_PI = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    # ImageURL = paste(unique(ImageURL), collapse= " | "),
    # PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )

setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("PI_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("PI_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

# PIAffiliation

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(PIAffiliation) %>%
  summarise(
    Change_PIAffiliation = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    # ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    # PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )


setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("PIAffiliation_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("PIAffiliation_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

# IdentifiedBy

x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(IdentifiedBy) %>%
  summarise(
    Change_IdentifiedBy = '',
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    #ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )


setwd("C:/rworking/digs/indata")

x %>%
  write.csv(paste("IdentifiedBy_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

upload <- gs_upload(paste("IdentifiedBy_NatDB_", version,".csv", sep = ''))
# gs_browse(upload, ws = 1)

##### _____ download google sheet of DatasetID key (output: key) #####
# key <- gs_title('DatasetID_Key_DSCRTP')
# # gs_browse(key)
# key <- gs_read(key)
# key <- data.frame(key)
# https://drive.google.com/open?id=1IOpb2slRwfOTx9JitYwA8xjUkWLwtgHI
key.new <- gs_key('1IOpb2slRwfOTx9JitYwA8xjUkWLwtgHI')
#gs_browse(key.new)
key.new <- gs_read(key.new)
key.new <- data.frame(key.new)

##### _____ download DatasetID key that is stored on google drive as an xlsx file #####

# create a list of files (or single file) that meets title query
x <- drive_find(q = "name contains '20190828-0_DatasetID_Key_DSCRTP'")

# getting the id as a character string
y <- x$id

# this downloads the file to the specified path
dl <- drive_download(as_id(y), path = "C:/rworking/deepseatools/indata/20190828-0_DatasetID_Key_DSCRTP.xlsx", overwrite = TRUE)

# read the file into R as a data frame
key <- read.xlsx(dl$local_path)

##### _____ Bringing in standardized tables back from Google Drive #####
# Register and download Google Sheet version of DatasetID table
DatasetID_key <- gs_title('20190107_DatasetID_Key_DSCRTP_NatDB_20181211-2')
DatasetID_key <- gs_read(DatasetID_key)
names(DatasetID_tab)

DatasetID_tab <- gs_title('20190109_0_unique_DatasetID_DSCRTP_NatDB_20181211-2_RPMcGuinn')
#gs_browse(DatasetID_tab, ws = 1)
DatasetID_tab <- gs_read(DatasetID_tab)
names(DatasetID_tab)

Repository_tab <- gs_title('20190109_0_unique_Repository_DSCRTP_NatDB_20181211-2_RPMcGuinn')
#browse(Repository_tab)
Repository_tab <- gs_read(Repository_tab)
names(Repository_tab)

VehicleName_tab <- gs_title('20190108_0_unique_VehicleName_DSCRTP_NatDB_20181211-2_RPMcGuinn')
VehicleName_tab <- gs_read(VehicleName_tab)
names(VehicleName_tab)

Vessel_tab <- gs_title('20190108_0_unique_Vessel_DSCRTP_NatDB_20181211-2_RPMcGuinn')
Vessel_tab <- gs_read(Vessel_tab)
names(Vessel_tab)

DataProvider_tab <- gs_title('20190108_0_DataProvider_DSCRTP_NatDB_20181211-2_RPMcGuinn')
gs_browse(DataProvider_tab, ws = 1)
DataProvider_tab <- gs_read(DataProvider_tab)
names(DataProvider_tab)

##### _____ get modified and original dataset from CSV #####
setwd("C:/rworking/deepseatools/indata")
x <- read.csv("20191216-0_UnpurgedRecords_THourigan.csv", header = T)
#y <- read.csv("20190819-4_NOAA_NEFSC_Connecticut_ISIS2_TowCam_Packer_Part_3_2013_2013.csv", header = T)

# 20190815-0_NOAA_NEFSC_Connecticut_ISIS2_TowCam_Packer_Part_2_2013_2013
# 20190819-4_NOAA_NEFSC_Connecticut_ISIS2_TowCam_Packer_Part_3_2013_2013

# setwd("C:/rworking/digs/indata")
# alt <- read.csv("20190301-0_Hourigan_Q2_NewDatasets_THourigan.csv", header = T)

##### _____ get modified and original dataset from tab delim. #####
setwd("C:/rworking/deepseatools/indata")
sub <- read.table("", header = T, sep="\t", fill = TRUE)

##### _____ get modified and original datasets via Excel #####
setwd("C:/rworking/deepseatools/indata")
sub <- read.xlsx('20191108-1_NOAA_OER_EX1806_NCarolina_Morrison_Sautter_2018_2018-TH.xlsx', sheet = 1)

#sub2 <- read.csv()

# # read in original dataset for comparison
# setwd("C:/rworking/digs/indata")
# orig <- read.csv("20181107_0_DSCRTP_National_Database_Schema_RPMcGuinn.csv", header = T)

##### _____ get any necessary crosswalk tables #####

setwd("C:/rworking/digs/indata")
crosswalk <- read.csv("NF1708_Append.csv", header = T)

x <- sub[2670:2672,]
yo <- bind_rows(x,crosswalk)
yo<-yo[names(x)]

setwd("C:/rworking/digs/indata")
write.csv(yo, 'yo.csv')

##### ***** create some subset of the national database ***** #####
sub <- filt %>%
  filter(
    ScientificName == 'Thesea nivea'
    #ScientificName == ''
    #is.na(ImageURL) == FALSE
 )# %>%
# dplyr::select(Longitude, Latitude, Class, Order, Family, Genus, Species, ScientificName, ImageURL, DepthInMeters) %>%
# group_by(ScientificName) %>%
# summarize(n = n()) %>%
# arrange(n)
# sub

##### look the 3D bounding box. convex hull #####
spdf<-sub
coordinates(spdf) <- c("Longitude", "Latitude", "DepthInMeters")
proj4string(spdf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
x<-bbox(spdf)
x

##### exporting extent box as shapefile #####
##### _____ Creating aoi polygon from bounding coordinates #####
# x<-bbox(spdf)
# x
minLon <- x[1,1]
maxLon <- x[1,2]
minLat <- x[2,1]
maxLat <- x[2,2]

e <- as(raster::extent(minLon, maxLon, minLat, maxLat), "SpatialPolygons")
proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
data = data.frame(f=99.9)
aoibox = SpatialPolygonsDataFrame(e,data)
plot(aoibox)
#
# setwd("C:/data/BaseLayers/GoldenCrabAOIp2")
# writeOGR(aoibox, dsn="aoibox",
#          layer= "aoibox",
#          driver = "ESRI Shapefile",
#          overwrite_layer = T)

##### write 2d envelope to shapefile #####

setwd("C:/data/aprx/explore/shapefiles")
writeOGR(aoibox, dsn="sweet",
         layer= "sweet",
         driver = "ESRI Shapefile",
         #dataset_options=c("NameField=ScientificName"),
         overwrite_layer = T)


##### read in some shapefiles #####
setwd("C:/data/BaseLayers/NaturalEarth")
countries <- readOGR(".", "ne_50m_admin_0_countries")

##### export to GIS using AcrGIS bridge (not working currently) #####

##### writing to ArcGIS geodatabase and shapefile #####

sub_geo <- sub
coordinates(sub_geo) <- c("Longitude", "Latitude")
proj4string(sub_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

fgdb_path <- 'C:/data/aprx/yo/new.gdb'
arc.write(file.path(fgdb_path, 'this3'), data=sub_geo)

# -OR- write it to a shapefile (this unfortunately results in truncated variable names)
arc.write('C:/data/aprx/geomorph/geomorphtest2.shp', data=sub_geo)

# arc.write(file.path(fgdb_path, 'sewardj2'), data=sub_, coords=c('Longitude', 'Latitude'),
#           shape_info=list(type='Point',hasZ=FALSE, WKID=4326), overwrite = TRUE)


##### export to GIS using shapefile using rgdal #####
spdf <- sub
coordinates(spdf) <- c("Longitude", "Latitude", "DepthInMeters")
proj4string(spdf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

setwd("C:/data/aprx/explore/shapefiles")
writeOGR(spdf, dsn="reed",
         layer= "reed",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### mapping AOI and data points with marmap getNOAA.bathy #####
#library(ggplot2)
# create a spatial points data frame
spdf<-sub
coordinates(spdf) <- c("Longitude", "Latitude", "DepthInMeters")
proj4string(spdf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
x<-bbox(spdf)
# install.packages('marmap')
# library(marmap)
zoom <- 2 # as number gets bigger you achieve a wider extent to your download
cont <- getNOAA.bathy(lon1 = x[1,1]-zoom, lon2 = x[1,2]+zoom,
                      lat1 = x[2,1]-zoom, lat2 = x[2,2]+zoom, resolution = 1,
                      keep = FALSE, antimeridian = FALSE)


aoi <- data.frame(x=c(x[1,1],x[1,1], x[1,2], x[1,2]),
                  y=c(x[2,1],x[2,2], x[2,2],x[2,1]))

# topographical color scale, see ?scale_fill_etopo
g <- autoplot(cont, geom=c("raster", "contour")) +
  scale_fill_etopo(name="Depth\n(meters)") +
  labs(x = 'Longitude') +
  labs(y = 'Latitude')

g + geom_point(aes(x=Longitude, y=Latitude), colour='red', data=sub, alpha=0.5) +
  geom_polygon(aes(x=x, y=y), colour="black", fill=NA, data = aoi)


##### download Google Sheet version of schema for use in R #####
# Register and download Google Sheet
s <- gs_title('2019_DSCRTP_National_Database_Schema')
#s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
s <- gs_read(s)
names(s)

##### write schema #####
setwd("C:/rworking/digs/indata")
write.csv(s, "2019_DSCRTP_Schema.csv")
##### _____ -OR- upload and transform schema #####
##### import schema via CSV#####
setwd("C:/rworking/digs/indata")
s <- read.csv("20181107_0_DSCRTP_National_Database_Schema_RPMcGuinn.csv", header = T)

# check
# View(s)
# names(s)
# str(s)
# unique(s$DataType)
# unique(s$OldGroup)

# get rid of extra fields
s<-s[,-(33:35)]

#trim leading and trailing whitespace
Trim <- function(x){
  gsub("^\\s+|\\s+$", "", x)
}
s$FieldName <- Trim(s$FieldName)
s$DSCRTPCategory <- Trim(s$DSCRTPCategory)
s$DSCRTPGroup <- Trim(s$DSCRTPGroup)

##### check #####
names(s)

##### _____ required and desired fields #####
x <- s %>%
  filter(
    grepl("R", PointProgram) |
      grepl("P", PointProgram)
  ) %>%
  group_by(
    DSCRTPCategory
  ) %>%
  summarise(
    fields = toString(unique(FieldName)),
    n=n()
  )
x

##### checking #####
table(s$DSCRTPCategory, useNA = 'always')

##### _____ valid values #####
x <- s %>%
  filter(
    grepl("VernacularNameCategory", FieldName)
  ) %>%
  group_by(
    FieldName
  ) %>%
  summarise(
    description = toString(unique(FieldDescription)),
    valid_values = toString(unique(ValidValues)),
    internal = toString(unique(InternalUseOnly)),
    n=n()
  )

View(x)

##### public vs. private fields #####

x <- s %>%
  filter(
    grepl('0', InternalUseOnly)
  ) %>%
  group_by(
    DSCRTPGroup
  ) %>%
  summarise(
    fields = toString(unique(FieldName)),
    n=n()
  )

View(x)

##### _____ diversity summary #####
sum_tbl <-
  indata %>%
  group_by(gisMEOW, DepthCat) %>%
  summarize(
    diversity = length(unique(na.omit(Genus))),
    n = n()
  )
sum_tbl

View(sum_tbl)

setwd("C:/rworking/digs/outdata")
write.csv(sum_tbl,"yo.csv", row.names = F, quote = T)
##### _____ internal vs external field #####
x <- s %>%
  filter(InternalUseOnly == 1) %>%
  group_by(FieldName) %>%
  summarise(n=n())
fix(x)

##### look at set differences between schema and current accession #####
setdiff(names(indata), s$FieldName)
setdiff(s$FieldName, names(sub))

##### filters #####
x <- filt %>%
  # filter(
  #   grepl('Brooke, Sandra, sbrooke@fsu.edu', DataProvider)
  # ) %>%
 group_by(DataProvider) %>%
  summarise(
    Record_Type = toString(unique(RecordType)),
    Citation = toString(unique(Citation)),
    DatasetID = toString(unique(DatasetID)),
    PI = toString(unique(PI)),
    ObservationYear = toString(unique(ObservationYear)),
    n=n()
    )
x
View(x)


# x <- indata %>%
#   filter(
#     DatasetID == 'NOAA_SWFSC_AST',
#     grepl('Lophelia', ScientificName)
#   ) %>%
#   group_by(Flag, FlagReason, DataProvider, PI,DataContact, PIAffiliation, Reporter) %>%
#   summarise(n=n())
# x
#
#
# x <- indata %>%
#   filter(
#     grepl('Wickes', Reporter),
#     Flag == '1'
#   ) %>%
#   group_by(DataProvider, PI,DataContact, PIAffiliation, Reporter) %>%
#   summarise(n=n())
# x

##### map it leaflet #####
x <- sub
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("ScientificName:", x$ScientificName, "<br>",
                                    "DatasetID:", x$DatasetID, "<br>",
                                    "SurveyID:", x$SurveyID, "<br>",
                                    "TrackingID:", x$TrackingID, "<br>",
                                    "Station:", x$Station, "<br>",
                                    "Observation Year:", x$ObservationYear))
m
##### LocationAccuracy #####
table(sub$LocationAccuracy, useNA = 'always')

##### gisLocality #####
x<-table(filt$gisNGIALocality, useNA = 'always')
View(x)

x <- filt %>%
  # filter(
  #   grepl('Kelley', PI)
  #) %>%
  group_by(gisNGIALocality) %>%
  summarise(
    mindist = min(gisNGIADist),
    mediandist = median(gisNGIADist),
    maxdist = max(gisNGIADist),
    n=n()
    )
x

##### DarwinCoreTerm #####
x <- s %>%
  # filter(
  #   grepl('Kelley', PI)
  #) %>%
  group_by(DarwinCoreTerm) %>%
  summarise(
    fields = toString(unique(FieldName)),
    n=n()
  )
x
View(x)

##### IndividualCount and CategoricalAbundance #####
x <- filt %>%
  filter(
    IndividualCount != '-999'
    #CategoricalAbundance != 'present',
    #CategoricalAbundance == '20-Oct'
  ) %>%
  group_by(IndividualCount) %>%
  summarise(
    CategoricalAbundance = toString(unique(CategoricalAbundance)),
    n=n()
  )
View(x)
summary(x$IndividualCount)

hist(x$IndividualCount, breaks=100, main='Breaks=100')


##### looking at IndividualCount and CategoricalAbundance in schema #####
x <- s %>%
  filter(
    grepl("IdentificationVerificationStatus", FieldName)
  ) %>%
  group_by(
    FieldName
  ) %>%
  summarise(
    FieldDescription = toString(unique(FieldDescription)),
    ValidValues = toString(unique(ValidValues)),
    n=n()
  )

View(x)

##### table of IndividualCount values #####

x <- filt %>%
  filter(
    IndividualCount > 5000
  ) %>%
  group_by(IndividualCount) %>%
  summarise(
    toString(ScientificName),
    toString(CatalogNumber),
    toString(SurveyID),
   # toString(Citation),
    toString(Vessel),
    toString(VehicleName),
    toString(RecordType),
    toString(SamplingEquipment),
    toString(DataProvider),
    toString(SampleAreaInSquareMeters),
    toString(Density),
    n=n()
  )
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(s, "20181114_0_IndividualCount_greater_than_5000_RPMcGuinn.csv")


##### IndividualCount #####
x <- filt %>%
  filter(
    IndividualCount == '0'
  ) %>%
  group_by(IndividualCount) %>%
  summarise(
    CategoricalAbundance = toString(unique(CategoricalAbundance)),
    CatalogNumber = toString(unique(CatalogNumber)),
    toString(ScientificName),
    toString(SurveyID),
    toString(Vessel),
    toString(VehicleName),
    toString(RecordType),
    toString(SamplingEquipment),
    toString(DataProvider),
    n=n()
  )
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(s, "20181114_0_IndividualCount_greater_than_5000_RPMcGuinn.csv")

##### minimum and maximum size values #####

x <- filt %>%
  filter(
    MinimumSize != '-999',
    MaximumSize != '-999'
  ) %>%
  group_by(MinimumSize, MaximumSize) %>%
  summarize(
    n=n()
  )
View(x)

x %>% ggplot(aes(MaximumSize, fill = Phylum)) +
  geom_histogram(alpha = 0.5,
                 aes(y = ..count..), #or you can use 'density'
                 position = 'identity', binwidth = 1)



##### looking at Citation and SurveyID #####
x <- filt %>%
  filter(
    grepl('Giamonna', Citation)
  ) %>%
  group_by(DatasetID, Citation, DataProvider) %>%
  summarise(
    toString(unique(SurveyID)),
    toString(unique(Vessel)),
    toString(unique(VehicleName)),
    n=n()
  )
View(x)


##### looking at EventID #####
x <- filt %>%
  filter(
    grepl('Innovator', VehicleName)
  ) %>%
  group_by(DatasetID, Citation) %>%
  summarise(
    toString(unique(EventID)),
    toString(unique(SurveyID)),
    toString(unique(Vessel)),
    toString(unique(VehicleName)),
    n=n()
  )
View(x)


##### looking at Condition #####
x <- sub %>%
  filter(
    #grepl('Innovator', VehicleName)
    Flag == '0'
  ) %>%
  group_by(Condition) %>%
  summarise(
    # toString(unique(EventID)),
    # toString(unique(SurveyID)),
    # toString(unique(Vessel)),
    # toString(unique(VehicleName)),
    n=n()
  )
View(x)

# setwd("C:/rworking/digs/outdata")
# write.csv(x,"x.csv", row.names = F, quote = T)

##### looking at taxonomy of specific ScientificName or by CatalogNumber #####

x <- sub %>%
  filter(
    #CatalogNumber == '914010',
    ScientificName == 'Nephththeidae'
  ) %>%
  group_by(VernacularName) %>%
  summarise(
    toString(unique(VernacularName)),
    toString(unique(VernacularNameCategory)),
    toString(unique(AphiaID)),
    toString(unique(ScientificName)),
    toString(unique(Phylum)),
    toString(unique(Class)),
    toString(unique(Order)),
    toString(unique(Family)),
    toString(unique(Genus)),
    n=n()
  )
View(x)

# setwd("C:/rworking/digs/outdata")
# write.csv(x,"x.csv", row.names = F, quote = T)


##### looking at Oxygen #####
x <- filt %>%
  filter(
    #IndividualCount != '-999'
    #CategoricalAbundance != 'present',
    #CategoricalAbundance == '20-Oct',
    Oxygen < 10,
    Oxygen != '-999'
  )
#%>%
  # group_by(IndividualCount) %>%
  # summarise(
  #   CategoricalAbundance = toString(unique(CategoricalAbundance)),
  #   n=n()
  # )
# View(x)
# summary(x$IndividualCount)

hist(x$Oxygen, breaks=100, main='Breaks=100')

##### looking at Temperature #####

x <- sub %>%
  filter(
    #IndividualCount != '-999'
    #CategoricalAbundance != 'present',
    #CategoricalAbundance == '20-Oct',
    #Temperature < 10,
    Temperature != '-999'
  )

#%>%
# group_by(IndividualCount) %>%
# summarise(
#   CategoricalAbundance = toString(unique(CategoricalAbundance)),
#   n=n()
# )
# View(x)
# summary(x$IndividualCount)

hist(x$Temperature, breaks=100, main='Breaks=100')

##### looking at Salinity #####

x <- filt %>%
  filter(
    #IndividualCount != '-999'
    #CategoricalAbundance != 'present',
    #CategoricalAbundance == '20-Oct',
    #Temperature < 10,
    Salinity != '-999',
    Salinity != '0'
  )

#%>%
# group_by(IndividualCount) %>%
# summarise(
#   CategoricalAbundance = toString(unique(CategoricalAbundance)),
#   n=n()
# )
# View(x)
# summary(x$IndividualCount)

hist(x$Salinity, breaks=100, main='Breaks=100')


##### looking at PI and PIAffiliation #####
x <- filt %>%
  filter(
    #IndividualCount != '-999'
    #CategoricalAbundance != 'present',
    #CategoricalAbundance == '20-Oct',
    #Temperature < 10,
    # Salinity != '-999',
    # Salinity != '0',
    grepl('Nizinski', PI) |
      grepl('Nizinski', DataContact) |
      grepl('Nizinski', Reporter)
  ) %>%
group_by(PI) %>%
summarise(
  PIAffiliation = toString(unique(PIAffiliation)),
  n=n()
)
View(x)




##### _____ map it data using leaflet #####
#install.packages("leaflet")

#x <- sub


library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", x$CatalogNumber, "<br>",
                        "ScientificName:", x$ScientificName, "<br>",
                        "RecordType:", x$RecordType, "<br>",
                        "Vessel:", x$Vessel, "<br>",
                        "DatasetID:", x$DatasetID, "<br>",
                        "SurveyID:", x$SurveyID, "<br>",
                        "SampleID:", x$SampleID, "<br>",
                        "TrackingID:", x$TrackingID, "<br>",
                        "Station:", x$Station, "<br>",
                        "Locality:", x$Locality, "<br>",
                        "Observation Year:", x$ObservationYear))
m


##### look at duplicated SampleID #####

x <- sub %>%
  group_by(SampleID) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

##### filter data by a list of something (extract)#####

y <- unique(crosswalk$SampleID)
x <- sub %>% filter(SampleID %in% y) %>% arrange(SampleID)

##### checking #####
length(x$CatalogNumber)
length(sub$CatalogNumber)
length(z$CatalogNumber)

length(crosswalk$Locality)

crosswalk$SampleID
x$SampleID

##### transform #####
x$Longitude <- crosswalk$LongitudeInDD
x$Latitude <- crosswalk$LatitudeInDD

##### load #####
z <- sub %>% filter(!SampleID %in% y) %>% arrange(SampleID)
sub <- rbind(z, x)

##### check #####
#length(sub$CatalogNumber)

##### write ####
setwd("C:/rworking/digs/outdata")
write.csv(sub,"20181130-0_PSU_ECOGIG_MultiVessel_ROV_CordesFisher_2011_2017.csv", row.names = F, quote = T)


##### ____ taxonomy stuff #####
##### ____ load the most current taxonomy from Google Sheets #####
# https://drive.google.com/open?id=0B9c2c_XdhpFBT29NQmxIeUQ4Tlk

n <- '20190828-0'

taxfl <- gs_title(paste(n, '_taxonomy_to_flag',sep = ''))
gs_browse(taxfl)
taxfl <- gs_read(taxfl)

taxch <- gs_title(paste(n, '_taxonomy_to_change', sep = ''))
gs_browse(taxch)
taxch <- gs_read(taxch)

tax <- gs_title(paste(n, '_taxonomy', sep = ''))
gs_browse(tax)
tax <- gs_read(tax)

##### load most current taxonomic tables from CSV #####
setwd("C:/rworking/digs/indata")
tax <- read.csv("20181130-0_taxonomy.csv", header = T)
taxch <- read.csv("20181130-0_taxonomy_to_change.csv", header = T)
taxfl <- read.csv("20181130-0_taxonomy_to_flag.csv", header = T)

##### load taxa of interest via CSV #####
#csv of ScientificNames of interest
setwd("C:/rworking/digs/indata")
nomatch <- read.csv('nomatch.csv', header = T)
nomatch

# #check
# table(nomatch$ScientificName)

#create a list from the filtered data
list <- factor(nomatch$ScientificName)

##### -OR- load a single species or a few to a list #####

list <- 'Fungiidae'
list <- 'Pleurocorallium'
list <- 'Paracorallium'
list <- 'Hemicorallium|Pleurocorallium|Paracorallium'
list <- 'Paramuricea'
list <- 'Victorgorgiidae'
list <- 'Nephtheidae'
list <- 'Paramuriceidae'
list <- 'Parazoanthidae'
list <- 'Bullagummizoanthus emilyacadiaarum'
list <- 'Octocorallia'
list <- 'Hydrozoa'
list <- 'Botrucnidifer sp.'
list <- 'Primnoidae'
list <- 'Antipathes caribbeanus'


##### compare list of taxa of interest to current taxonomy tables (must be exact to use it this way) #####
# is it on a list?
setdiff(list, sub$ScientificName)
setdiff(list, filt$ScientificName)
setdiff(list, tax$ScientificName)
setdiff(list, taxch$VerbatimScientificName)
setdiff(list, taxfl$ScientificName)

##### extracting taxomic string and sending to google drive  #####
x <- tax %>% filter(ScientificName == 'Paramuricea placomus')

x %>%
  write.csv("Paramuricea-placomus.csv", row.names = FALSE)

xsheet <- gs_upload("Paramuricea-placomus.csv")
#xsheet
gs_browse(xsheet, ws = 1)



##### building new additions to taxfl table #####
listdf <- data.frame(list)
names(listdf) <- c('ScientificName')
listdf$Flag <- 1
listdf$FlagReason <- 'Outside of taxonomic scope'
new_taxfl <- rbind(taxfl,listdf)
new_taxfl <- new_taxfl %>% arrange(ScientificName)

setwd("C:/rworking/digs/indata")
new_taxfl %>%
  write.csv('20190228-0_taxonomy_to_flag.csv', row.names = FALSE)
xsheet <- gs_upload('20190228-0_taxonomy_to_flag.csv')
gs_browse(xsheet, ws = 1)

##### extract proper taxonomic string to add records from incoming to taxonomic sheet ######

# names(sub)

nomatch.list <- nomatch$ScientificName

taxstring <- sub %>% filter(ScientificName %in% nomatch.list) %>%
  dplyr::select(VernacularNameCategory, VernacularName, ScientificName, TaxonRank,
                AphiaID, Phylum, Class, Subclass, Order, Suborder, Family, Subfamily, Genus,
                Subgenus, Species, Subspecies, ScientificNameAuthorship,
                Synonyms) %>%
  group_by(VernacularNameCategory, VernacularName, ScientificName, TaxonRank,
           AphiaID, Phylum, Class, Subclass, Order, Suborder, Family, Subfamily, Genus,
           Subgenus, Species, Subspecies, ScientificNameAuthorship,
           Synonyms) %>% summarise(n=n())

setwd("C:/rworking/digs/indata")
write.csv(taxstring,"taxstring.csv", row.names = F, quote = T)


##### looking at individual taxa within a dataset or the taxonomy table #####
x <- tax %>%
  filter(
    grepl(list, ScientificName)
  ) %>%
  group_by(
    ScientificName, Class, Order, Family, Genus, VernacularNameCategory
  ) %>%
  summarise(
    # region = toString(unique(FishCouncilRegion)),
    # mean_depth_meters = mean(as.numeric(DepthInMeters)),
    n=n()
  )

View(x)



##### looking at ScientificName authorship #####
x <- tax %>% filter(TaxonRank != 'species', TaxonRank != 'subspecies' , TaxonRank != 'variety')

table(x$ScientificNameAuthorship, useNA = 'always')
table(x$TaxonRank, useNA = 'always')

##### Depth Stuff #####
x <- sub %>% filter(
  DepthInMeters != '-999',
  Flag == '1'
) %>%
  group_by(ScientificName, Flag, FlagReason) %>%
  summarise(
    min.Depth = min(as.numeric(DepthInMeters)),
    max.Depth = max(as.numeric(DepthInMeters)),
    mean.Depth = mean(as.numeric(DepthInMeters)),
    mean.DepthGIS = mean(as.numeric(gisEtopoDepth)),
    n = n()
  )
x


# summary(x$DepthInMeters)
# summary(x$gisCRMDepth)
# summary(x$gisEtopoDepth)


##### Citation Maker #####
sum_tbl <- sub %>%
  #  filter(
  #    DatasetID == 'BOEM_RB-09-05'
  #  ) %>%
  group_by(DatasetID, AccessionID, Citation) %>%
  summarize(
    minObservationYear = min(as.numeric(ObservationYear)),
    maxObservationYear = max(as.numeric(ObservationYear)),
    SurveyID = toString(unique(SurveyID)),
    RecordType = toString(unique(RecordType)),
    PI = toString(unique(PI)),
    WebSite = toString(unique(WebSite)),
    DataProvider = toString(unique(DataProvider)),
    Repository = toString(unique(Repository)),
    DataContact = toString(unique(DataContact)),
    Reporter = toString(unique(Reporter)),
    Vessel = toString(unique(Vessel)),
    VehicleName = toString(unique(VehicleName)),
    n = n()
  )

sum_tbl$CitationMaker <- paste(sum_tbl$DataProvider,'. ',
                               sum_tbl$minObservationYear,' to ',
                               sum_tbl$maxObservationYear,'. ',
                               'Coral or sponge occurrence observations submitted to the NOAA National Database for Deep Sea Corals (www.deepseacoraldata.noaa.gov)', '. ',
                               'DSCRTP Dataset ID: ', sum_tbl$DatasetID, '. ',
                               'DSCRTP Accession ID: ',sum_tbl$AccessionID, '. ',
                               'Record type: ', sum_tbl$RecordType, '. ',
                               'Vessel(s): ', sum_tbl$Vessel,'. ',
                               'Sampling vehicle: ', sum_tbl$VehicleName,'. ',
                               'Survey ID: ', sum_tbl$SurveyID,'. ',
                               'Principle investigator: ', sum_tbl$PI,'. ',
                               'Data contact: ', sum_tbl$DataContact,'. ',
                               'Reporter: ', sum_tbl$Reporter,'. ',
                               'Repository: ', sum_tbl$Repository,'. ',
                               'Web site [last accessed on YYYY-MM-DD]: ', sum_tbl$WebSite,'.',
                               sep = '')

cite <- sum_tbl %>%
  ungroup() %>%
  select(DataProvider, DatasetID, AccessionID,Citation, CitationMaker, n)

setwd("C:/rworking/digs/outdata")
write.csv(cite,"20180702_0_CitationMaker_DSCRTP_NatDB_20180405-0.csv", row.names = F, quote = T)


##### _____ exporting datasets directly to Excel #####
##### filter data #####
x <- filt %>%
  filter(
    DatasetID == 'MCZ-IZ' |
      DatasetID == 'MCZ_IZ'
  ) #%>%
# group_by(DatasetID) %>%
#   summarize(n())

#write.xlsx(x, "yo.xlsx")

##### export each dataset ID to individual excel files #####

setwd("C:/rworking/deepseatools/indata")
for (name in head(levels(factor(filt$DatasetID)))){
  #Subset the data by DatasetID
  tmp <- subset(indata, DatasetID==name)
  #Create a new filename for each DataseitID - the folder 'xls' should already exist in your output folder
  fn <- paste('xls/',unique(filt$DatabaseVersion),gsub(' ','',name),'.xlsx',sep='')
  #Save the CSV file containing separate expenses data for each MP
  write.xlsx(tmp,fn, row.names = FALSE)
}

##### _____ read a bunch of xls files from a folder and the bind them all together into a dataframe #####

# set working directory for where to find the xlsx files
# setwd("C:/rworking/digs/indata/20190211_0_review")
# file.list <- list.files(pattern='*.xlsx')

setwd('C:/rworking/digs/indata/20190320-1_0_altered_spreadsheets_from_McGuinn_Hourigan_Review')
file.list2 <- list.files(pattern='*.xlsx')

##### create a list of data frames (all must have same column names, no extras, for this to work) #####
library(readxl)
df.list <- lapply(file.list2, read_excel)

##### bind all of these files together#####
library(data.table)
df <- rbindlist(df.list, idcol = "id")
df <- df[,2:128]

##### checking: extract the records with duplicated CatalogNumber and create a list #####
x <- df2 %>%  filter(duplicated(CatalogNumber) == TRUE)
list <- x$CatalogNumber
list

##### checking: select the duplcated records from the original #####
# y <- sub %>% filter(CatalogNumber %in% list)

##### checking #####
# z <- y %>% filter(DatasetID != 'NOAA_EX-15-04-L2') %>%
#   dplyr::select(DatasetID, CatalogNumber,
#                ScientificName, Genus,
#                Latitude, Longitude)
# View(z)
#
# xx <- y %>%  filter(ScientificName != 'Corallium sulcatum')
# table(factor(xx$DatasetID))

##### extracts record that meets following condition #####
df2 <- df %>% filter(ScientificName != 'Corallium sulcatum' | CatalogNumber != '494843')

##### write list of files #####
setwd('C:/rworking/digs/indata/')
write.csv(file.list2, 'list.csv')

##### output step #####
setwd("C:/rworking/digs/indata")
write.csv(df2, '20190327-0_Q2-2019_corrections_DatasetID_dashboard_review.csv', row.names = FALSE)

# # If you also want to include the files in subdirectories, use:
#
#   file.list <- list.files(pattern='*.xlsx', recursive = TRUE)

##### _____ recode values based on conditions (non_dplyr) #####

x$ScientificName <- as.character(x$ScientificName)
x$ScientificName[x$VernacularNameCategory == 'gorgonian coral' &
                   x$DatasetID == 'Oceana_SW_16-08' ] <- 'New Name'

x$DatasetID <- as.character(x$DatasetID)
x$DatasetID[x$DatasetID == 'MCZ-IZ'] <- 'MCZ_IZ'

##### _____ checking DatasetID matching #####

setdiff(filt$DatasetID, datasetID$DatasetID)
setdiff(datasetID$DatasetID, filt$DatasetID)

table(datasetID$DatasetID)

x <- filt %>%
  filter(
    grepl('Giammona', DatasetID)
  ) %>%
  group_by(DatasetID, Citation, DataProvider) %>%
  summarise(
    toString(unique(SurveyID)),
    toString(unique(Vessel)),
    toString(unique(VehicleName)),
    toString(unique(RecordType)),
    toString(unique(Repository)),
    n=n()
  )
View(x)

##### creating standardized variable lists and upload to Google Drive #####
x <- filt %>%
  arrange(ObservationYear) %>%
  # filter(
  #   grepl('North Pacific', Vessel)
  #   #is.na(RecordType) == T
  #   #RecordType == 'literature'
  # ) %>%
  group_by(DataProvider) %>%
  summarise(
    RecordType_list = toString(unique(RecordType)),
    N_Records=n(),
    #DatasetID_list = toString(unique(DatasetID)),
    ObservationYear_list = toString(unique(ObservationYear)),
    Vessel_list = toString(unique(Vessel)),
    VehicleName_list = toString(unique(VehicleName)),
    WebSite_list = toString(unique(WebSite)),
    #ImageURL = toString(unique(ImageURL)),
    PI_list = toString(unique(PI)),
    Citation_list = toString(unique(Citation)),
    DataContact_list = toString(unique(DataContact)),
    Reporter_list = toString(unique(Reporter))
  )

# x <- filt %>%
#   arrange(ObservationYear) %>%
#   filter(
#     grepl('Wagner, Daniel', DataProvider)
#   ) %>%
#   group_by(DataProvider, Repository, DatasetID, RecordType, Citation, DataContact, Reporter) %>%
#   summarise(
#     SurveyID = toString(unique(SurveyID)),
#     n=n()
#   )

setwd("C:/rworking/digs/indata")
write.csv(x, 'x.csv')

x %>%
  write.csv("x.csv", row.names = FALSE)

xsheet <- gs_upload("x.csv")
#xsheet
gs_browse(xsheet, ws = 1)

# DSCRTP_NatDB_20181211-2

##### _____ filter then map it using leaflet #####
#install.packages("leaflet")

x <- filt %>%
  arrange(ObservationYear) %>%
  filter(
    # grepl('Wagner, Daniel', DataProvider),
    # is.na(RecordType) ==  T
    # DatasetID == 'BOEM_RB-09-07',
    # RecordType 'literature'
    Reporter == 'McGuinn, Robert P.',
    SurveyID == 'RB-09-07'
  )


library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", x$CatalogNumber, "<br>",
                        "ScientificName:", x$ScientificName, "<br>",
                        "RecordType:", x$RecordType, "<br>",
                        "Vessel:", x$Vessel, "<br>",
                        "DatasetID:", x$DatasetID, "<br>",
                        "SurveyID:", x$SurveyID, "<br>",
                        "SampleID:", x$SampleID, "<br>",
                        "TrackingID:", x$TrackingID, "<br>",
                        "Station:", x$Station, "<br>",
                        "Locality:", x$Locality, "<br>",
                        "Observation Year:", x$ObservationYear,"<br>",
                        "DataProvider:", x$DataProvider,"<br>",
                        "Repository:", x$Repository,"<br>",
                        "PI:", x$PI, "<br>",
                        "PIAffiliation:", x$PIAffiliation, "<br>",
                        "ImageURL:", x$ImageURL
                        ))
m

##### atomized query summary with Google Drive output #####
x <- filt %>%
  arrange(ObservationYear) %>%
  filter(
    #grepl('Hard', Habitat)
    DatasetID == 'WHOI_AT-18'
  ) %>%
  group_by(DatasetID, Flag, DataProvider, Repository, AccessionID, PI, PIAffiliation, DataContact, Reporter, Citation) %>%
  summarise(
    RecordType_list = toString(unique(RecordType)),
    n=n(),
    DatasetID_list = toString(unique(DatasetID)),
    #Habitat_list = toString(unique(Habitat)),
    ObservationYear_list = toString(unique(ObservationYear)),
    ObservationDate_list = toString(unique(ObservationDate)),
    Vessel_list = toString(unique(Vessel)),
    VehicleName_list = toString(unique(VehicleName)),
    SamplingEquipment_list = toString(unique(SamplingEquipment)),
    SurveyID_list = toString(unique(SurveyID)),
    EventID_list = toString(unique(EventID)),
    SampleID_list = toString(unique(SampleID)),
    TrackingID_list = toString(unique(TrackingID)),
    SurveyComments_list = toString(unique(SurveyComments)),
    ReporterComments_list = toString(unique(ReporterComments)),
    Locality_list = toString(unique(Locality)),
    WebSite_list = toString(unique(WebSite))
    # #ImageURL = toString(unique(ImageURL)),
    # #PI_list = toString(unique(PI)),
    # #Citation_list = toString(unique(Citation)),
    # Species_list = toString(unique(ScientificName)),
    # ImageFilePath_list = toString(unique(ImageFilePath)),
    # ImageURL_list = toString(unique(ImageURL))
    # #DataContact_list = toString(unique(DataContact)),
    #Reporter_list = toString(unique(Reporter))
  )

#View(x)
setwd("C:/rworking/digs/indata")

x %>%
  write.csv("x.csv", row.names = FALSE)

xsheet <- gs_upload("x.csv")
#xsheet
gs_browse(xsheet, ws = 1)

# setwd("C:/rworking/digs/outdata")
# write.xlsx(x,'20190208_0_NOAA_VO-05-08_from_DSCRTP_NatDB_20190117-0.xlsx',row.names = FALSE)

##### query with Google Drive output #####
x <- indata  %>%
arrange(ObservationYear)  %>%
  filter(
    #grepl('Wagner, Daniel', DataProvider),
    #is.na(RecordType) ==  T
    #RecordType 'literature'
    #Flag == '0',
    DatasetID == 'RSMAS'
  ) %>%
group_by(DataProvider, Repository) %>%
  summarise(
    RecordType = toString(unique(RecordType)),
    n=n(),
    DatasetID = toString(unique(DatasetID)),
    ObservationYear = toString(unique(ObservationYear)),
    Vessel = toString(unique(Vessel)),
    VehicleName = toString(unique(VehicleName)),
    ImageURL = toString(unique(ImageURL)),
    PI = toString(unique(PI)),
    Citation = toString(unique(Citation)),
    DataContact = toString(unique(DataContact)),
    Reporter = toString(unique(Reporter))
  )

setwd("C:/rworking/digs/indata")

x %>%
  write.csv("query.csv", row.names = FALSE)

xsheet <- gs_upload("query.csv")
gs_browse(xsheet, ws = 1)

##### robis #####

# get OBIS data by resourceid
data <- occurrence(scientificname = 'Ptilosarcus gurneyi')

# get OBIS data by ScientificName
data <- occurrence("Lophelia")

# get data at a specific year
q2002 <- data %>%
  filter(
  yearcollected == '2002',
  catalogNumber == '457'
  )

# write to google drive

setwd("C:/rworking/digs/indata")
q2002 %>%
  write.csv("q2002.csv", row.names = FALSE)
xsheet <- gs_upload("q2002.csv")
gs_browse(xsheet, ws = 1)

# for this example, convert back from data frame tbl (dplyr) to standard data frame
data <- as.data.frame(data)

head(data)
head(data, n = 100)
dim(data)
nrow(data)
ncol(data)
names(data)
str(data)
summary(data)
View(data)

# now convert to data frame tbl (dplyr)
data <- tbl_df(data)

data
head(data)
print(data, n = 100)

#filtering
require(robis)
require(dplyr)

data <- occurrence("Lophelia")
data %>% filter(scientificName == "Lophelia pertusa" & yearcollected > 2005)
View(data)

#Reordering
names(data)
data %>% arrange(datasetID, desc(eventDate))
table(data$datasetID)
unique(data$occurrenceRemarks)
View(data)

#Selecting and renaming columns
data %>% select(scientificName, eventDate, lon=decimalLongitude, lat=decimalLatitude)

#select() can be used with distinct() to find unique combinations of values:
data %>% select(scientificName, locality) %>% distinct()
View(data)

#Adding columns
data %>% mutate(zone = .bincode(minimumDepthInMeters, breaks=c(0, 10, 100))) %>% select(minimumDepthInMeters, zone) %>% filter(!is.na(zone)) %>% print(n = 100)

#Aggregation
data %>% summarise(lat_mean = mean(decimalLatitude), lat_sd = sd(decimalLatitude))
data %>% group_by(scientificName) %>% summarise(records=n(), datasets=n_distinct(datasetName))

#Restructuring
#This example converts a dataset from OBIS to a matrix format, which is more suitable for community analysis:

require(robis)
require(reshape2)

data <- occurrence(resourceid = 586)
wdata <- dcast(data, locality ~ scientificName, value.var = "individualCount")

#And the other way around, from wide format to long format:
ldata <- melt(wdata, variable.name = "scientificName", value.name = "individualCount")

#Plotting
#In this example, data for one species is extracted from an OBIS dataset.
#Density and depth are visualized using the ggplot2 package:

require(robis)
require(dplyr)
require(reshape2)
require(ggplot2)

data <- occurrence(resourceid = 586)
data <- occurrence("Lophelia")

afil <- data %>%
  filter(scientificName == "Lophelia pertusa") %>%
  group_by(locality) %>%
  summarise(n = mean(individualCount), lon = mean(decimalLongitude), lat = mean(decimalLatitude), depth = mean(minimumDepthInMeters))

ggplot() + geom_point(data = afil, aes(lon, lat, size = n, colour = depth)) +
  scale_colour_distiller(palette = "Spectral") +
  theme(panel.background = element_blank()) + coord_fixed(ratio = 1) + scale_size(range = c(2, 12))

##### _____ Get OBIS Data and Map It #####
##### get OBIS data #####
require(leaflet)

data <- occurrence("Myctophiformes")

# QC Flagging
qcflag <- function(qc, number) {
  mask <- 2^(number-1)
  return(sapply(qc, function(x) {
    return(sum(bitwAnd(x, mask) > 0))
  }))
}

data$qcnum <- qcflag(data$qc, c(24, 28))

colors <- c("red", "orange", "green")[data$qcnum + 1]

##### filter data in OBIS #####

data <- data %>%
  filter(collectionCode != 'NOAA_DSC_RTP')


##### map OBIS data in leaflet #####
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=data,
                      lat = data$decimalLatitude,
                      lng = data$decimalLongitude,
                      radius=5,
                      weight=0,
                      fillColor=colors,
                      fillOpacity=1,
                      popup = paste("scientificName:", data$scientificName, "<br>",
                                    "depth:", data$depth, "<br>",
                                    "locality:", data$locality, "<br>",
                                    "datasetName:", data$datasetName, "<br>",
                                    "collectionCode:", data$collectionCode, "<br>",
                                    "eventDate:", data$eventDate, "<br>",
                                    "institutionID:", data$institutionID, "<br>",
                                    "footprintWKT:", data$footprintWKT, "<br>",
                                    "ownerInstitutionCode:", data$ownerInstitutionCode))
m

##### _____ subsetting of filt to d #####
d <- filt  %>%
  filter(Flag == "0",
         # #DatasetID %in% head(unique(DatasetID), n=15),
         # DatasetID == "Hall-Spencer_etal_2007"|
         DatasetID == 'Bianchi_C_2011'
         #   DatasetID == 'BOEM_RB-09-07'|
         #   DatasetID == 'NOAA_AFSC_Longline_Survey'
  )

##### checking #####
table(unique(factor(d$DataProvider)))
table(unique(factor(d$RecordType)))
table(unique(factor(d$DataContact)))
table(factor(d$DatasetID), useNA = 'always')

##### checking #####
x <- d %>% arrange(ObservationYear) %>% group_by(DatasetID) %>%
  summarize(
    RecordType_list = toString(unique(RecordType)),
    N_Records=n(),
    # DatasetID_list = toString(unique(DatasetID)),
    DataProvider_list = toString(unique(DataProvider)),
    Repository_list = toString(unique(Repository)),
    ObservationYear_list = toString(unique(ObservationYear)),
    Vessel_list = toString(unique(Vessel)),
    VehicleName_list = toString(unique(VehicleName)),
    WebSite_list = toString(unique(WebSite)),
    #ImageURL = toString(unique(ImageURL)),
    PI_list = toString(unique(PI)),
    PIAffiliation_list = toString(unique(PIAffiliation)),
    Citation_list = toString(unique(Citation)),
    DataContact_list = toString(unique(DataContact)),
    Reporter_list = toString(unique(Reporter)),
    SurveyID_list = toString(unique(SurveyID))
  )

View(x)
##### _____ map it data using leaflet #####
#install.packages("leaflet")
#x <- sub

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=d,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", d$CatalogNumber, "<br>",
                        "ScientificName:", d$ScientificName, "<br>",
                        "RecordType:", d$RecordType, "<br>",
                        "Vessel:", d$Vessel, "<br>",
                        "DatasetID:", d$DatasetID, "<br>",
                        "SurveyID:", d$SurveyID, "<br>",
                        "SampleID:", d$SampleID, "<br>",
                        "TrackingID:", d$TrackingID, "<br>",
                        "Station:", d$Station, "<br>",
                        "Locality:", d$Locality, "<br>",
                        "ImageURL:", d$ImageURL, "<br>",
                        "Latitude:", d$Latitude, "<br>",
                        "Observation Year:", d$ObservationYear))
m


##### _____ faceted histogram
x <- filt %>%
  arrange(ObservationYear) %>%
  filter(
    #grepl('Hard', Habitat)
    #Family == 'Primnoidae',
    DatasetID == 'NOAA_VO-05-08'
    #as.numeric() > 15
    )

ggplot(x,aes(x=Temperature)) +
  geom_histogram()+
  facet_grid(~Genus)+theme_bw()

##### _____ map it data using leaflet #####
#install.packages("leaflet")
d <- filt %>%
  arrange(ObservationYear) %>%
  filter(
    #grepl('Hard', Habitat)
    #Family == 'Primnoidae',
    DatasetID == 'NOAA_VO-05-08'
  )

x <- filt %>%
  arrange(ObservationYear) %>%
  filter(
    #grepl('Hard', Habitat)
    #Family == 'Primnoidae',
    DatasetID == 'NOAA_VO-05-08',
    as.numeric(Density) > 15)



library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=d,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=1,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", d$CatalogNumber, "<br>",
                        "ScientificName:", d$ScientificName, "<br>",
                        "RecordType:", d$RecordType, "<br>",
                        "Vessel:", d$Vessel, "<br>",
                        "DatasetID:", d$DatasetID, "<br>",
                        "SurveyID:", d$SurveyID, "<br>",
                        "SampleID:", d$SampleID, "<br>",
                        "TrackingID:", d$TrackingID, "<br>",
                        "Station:", d$Station, "<br>",
                        "Locality:", d$Locality, "<br>",
                        "ImageURL:", d$ImageURL, "<br>",
                        "Observation Year:", d$ObservationYear))

m <- addCircleMarkers(m, data=x,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=1,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", x$CatalogNumber, "<br>",
                        "ScientificName:", x$ScientificName, "<br>",
                        "RecordType:", x$RecordType, "<br>",
                        "Vessel:", x$Vessel, "<br>",
                        "DatasetID:", x$DatasetID, "<br>",
                        "SurveyID:", x$SurveyID, "<br>",
                        "SampleID:", x$SampleID, "<br>",
                        "TrackingID:", x$TrackingID, "<br>",
                        "Station:", x$Station, "<br>",
                        "Locality:", x$Locality, "<br>",
                        "ImageURL:", x$ImageURL, "<br>",
                        "Observation Year:", x$ObservationYear))

m



##### _____ bringing in Dave Packer data and rbinding.

##### _____ get modified and original dtaasets via Excel #####
setwd("C:/rworking/digs/indata")
one <- read.xlsx('20181129_0_GOM 2013 national database for DSCRTP_DPacker.xlsx', sheet = 5)
two <- read.xlsx('20181129_0_GOM 2013 national database for DSCRTP_DPacker.xlsx', sheet = 6)
three <- read.xlsx('20181129_0_GOM 2013 national database for DSCRTP_DPacker.xlsx', sheet = 7)
four <- read.xlsx('20181129_0_GOM 2013 national database for DSCRTP_DPacker.xlsx', sheet = 8)
five <- read.xlsx('20181129_0_GOM 2013 national database for DSCRTP_DPacker.xlsx', sheet = 9)
six <- read.xlsx('20181129_0_GOM 2013 national database for DSCRTP_DPacker.xlsx', sheet = 10)
seven <- read.xlsx('20181129_0_GOM 2013 national database for DSCRTP_DPacker.xlsx', sheet = 11)
eight <- read.xlsx('20181129_0_GOM 2013 national database for DSCRTP_DPacker.xlsx', sheet = 12)
nine <- read.xlsx('20181129_0_GOM 2013 national database for DSCRTP_DPacker.xlsx', sheet = 13)

x <- rbind(one,two,three,four,five,six,seven,eight,nine)

x <- x %>% filter(
  ImageFilePath != 'R',
  is.na(Latitude) == F
                   )
x$Latitude <-  as.numeric(x$Latitude)
x$Longitude <-  as.numeric(x$Longitude)

##### looking at duplicate records in Packer dataset #####

nineless <- nine %>% dplyr::select(-CruiseComments)
setdiff(names(nineless), names(sub))
subless <- dplyr::select_(sub, .dots = names(nineless))

sublessd <- distinct(subless)
length(sublessd$ImageFilePath)
setdiff(names(nineless), names(sublessd))

##### select all sub where = Tow 8 #####
x <- sub %>% filter(EventID == 'Tow 8')

##### lopping off the end of the Packer dataset (duplicate)
sub_distinct <- sub[0:(914-190),]

##### write to excel (non-looping) #####
setwd("C:/rworking/digs/outdata")
write.xlsx(sub,'work.xlsx', row.names = FALSE)

##### write to google drive #####

setwd("C:/rworking/digs/indata")
write.csv(sub, 'sub.csv')
sub %>%
  write.csv("sub.csv", row.names = FALSE)
subsheet <- gs_upload("sub.csv")
#xsheet
gs_browse(subsheet, ws = 1)

##### publish new schema online #####

# get the latest version from Google Drive

s <- gs_title('2019_DSCRTP_National_Database_Schema')
#s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
s <- gs_read(s)
names(s)
View(s)

# public

s_public <- s %>% dplyr::select(FieldName, FieldOrder, FieldDescription, ValidValues, UsageNotes, InternalUseOnly)

# write to google drive

setwd("C:/rworking/digs/indata")

s_public %>%
  write.csv("20190314-0_DSCRTP_National_Database_Data_Dictionary.csv", row.names = FALSE)
SPubGsheet <- gs_upload("20190314-0_DSCRTP_National_Database_Data_Dictionary.csv")
#xsheet
gs_browse(SPubGsheet, ws = 1)

##### publish new data submission template #####

s <- gs_title('2019_DSCRTP_National_Database_Schema')
#s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
s <- gs_read(s)
names(s)
# View(s)

##### template work #####

s.template <- s %>% filter(
  'DataSubmissionTemplate' == '1',
  PointHist == 'R'|
    PointHist == 'D' | PointNew == 'R'|
    PointNew == 'D' | PointProgram == 'R'|
    PointProgram == 'D' |

    TransHist == 'R'|
    TransHist == 'D' | TransNew == 'R'|
    TransNew == 'D' | TransProgram == 'R'|
    TransProgram == 'D' |

    TrawlHist == 'R'|
    TrawlHist == 'D' | TrawlNew == 'R'|
    TrawlNew == 'D' | TrawlProgram == 'R'|
    TrawlProgram == 'D'

) %>% dplyr::select(
  PointHist, PointNew,
  PointProgram, TransHist, TransNew, TransProgram,
  TrawlHist, TrawlNew, TrawlProgram, FieldDescription, ValidValues, FieldName)

s.template <- t(s.template)
s.template <- as.data.frame(s.template)
dim(s.template)

setwd("C:/rworking/digs/indata")

s.template %>%
  write.csv("20190315-0_DSCRTP_National_Database_DataSubmissionTemplate.csv", row.names = TRUE, col.names = FALSE)
SPubGsheet <- gs_upload("20190315-0_DSCRTP_National_Database_DataSubmissionTemplate.csv")
#xsheet
gs_browse(SPubGsheet, ws = 1)

##### metadata tab in submission template #####
x <- s %>% filter(FieldName %in% c('DataProvider', 'DataContact', 'Citation', 'Repository', 'Modified', 'Reporter', 'ReporterComents',
                             'SurveyID', 'Vessel', 'VehicleName', 'PI', 'PIAffiliation', 'SamplingEquipment',
                             'DepthMethod', 'NavType', 'LocationAccuracy', 'Purpose', 'SurveyComments',
                             'Website', 'RecordType', 'IdentifiedBy', 'IdentificationQualifier', 'IdentificationDate',
                             'IdentificationComments')) %>% dplyr::select(FieldDescription, ValidValues, FieldName)

# create row order
z <- c('DataProvider', 'DataContact', 'Citation', 'Repository', 'Modified', 'Reporter', 'ReporterComents',
         'SurveyID', 'Vessel', 'VehicleName', 'PI', 'PIAffiliation', 'SamplingEquipment',
         'DepthMethod', 'NavType', 'LocationAccuracy', 'Purpose', 'SurveyComments',
         'Website', 'RecordType', 'IdentifiedBy', 'IdentificationQualifier', 'IdentificationDate',
         'IdentificationComments')

# implement row ordering based on row order vector above (using specific order in factors of FieldName)

x <- x %>%
  slice(match(z,FieldName))

##### output #####

setwd("C:/rworking/digs/outdata")

x %>%
  write.csv("20190315-0_DSCRTP_National_Database_DataSubmission_metadata.csv", row.names = TRUE)
View(x)



##### leaflet map with clusters #####
addMarkers(data=specdf,
           lng=~specLon,
           lat=~specLat,
           clusterOptions = markerClusterOptions(),
           clusterId = "Muliple Collections",
           popup=~paste("Specimen: ",shortSpec,"Field ID: ",fieldID, sep=" "))


##### DatasetID queries #####
dataset <- 'NOAA' #NOAA_SWFSC_AST'
#'NOAA_SH-10-11', 'DWH_NRDA_NF-10-14', 'DWH_NRDA_WS-14-06'

x <- indata  %>%
  arrange(ObservationDate)  %>%
  filter(
    # grepl('Olympic', DataProvider)
    # ObservationYear == '2010'
    # is.na(RecordType) ==  T
    # RecordType 'literature'
    # Flag == '1'
    grepl(dataset, DatasetID),
    # grepl('Lophelia', ScientificName),
    # IndividualCount == '1'
    # grepl('Stone', PI)
    # SurveyID == 'SJ-04-07'|
    #   SurveyID == 'SJ-05-09'
  ) %>%
  group_by(ObservationYear, DatasetID, gisMEOW) %>% #Flag, FlagReason, CatalogNumber, SampleID
  summarise(
    n=n(),
    #gisMEOW = paste(unique(gisMEOW), collapse=" | "),
    Locality = paste(unique(Locality), collapse=" | "),
    SurveyID = paste(unique(SurveyID), collapse=" | "),
    DataProvider = paste(unique(DataProvider), collapse=" | "),
    Repository = paste(unique(Repository), collapse=" | "),
    ScientificName = paste(unique(ScientificName), collapse=" | "),
    TypeStatus = paste(unique(TypeStatus), collapse=" | "),
    VernacularNameCategory = paste(unique(VernacularNameCategory), collapse=" | "),
    RecordType = paste(unique(RecordType), collapse=" | "),
    #DatasetID = toString(unique(DatasetID)),
    WebSite = paste(unique(WebSite), collapse=" | "),
    #ObservationYear = paste(unique(ObservationYear), collapse=" | "),
    ObservationDate = paste(unique(ObservationDate), collapse=" | "),
    Vessel = paste(unique(Vessel), collapse=" | "),
    VehicleName = paste(unique(VehicleName), collapse=" | "),
    NumberImages = paste(unique(ImageURL), collapse=" | "),
    PI = paste(unique(PI), collapse=" | "),
    Citation = paste(unique(Citation), collapse=" | "),
    DataContact = paste(unique(DataContact), collapse=" | "),
    Reporter = paste(unique(Reporter), collapse=" | ")
    #FlagReason = paste(unique(FlagReason), collapse=" | ")
  )

View(x)

##### write it #####
# write it
setwd('C:/rworking/digs/indata')
ndb %>%
  write.csv("20190416-0_DatasetID_DWH_NRDA_HC-11-09_DSCRTP_NatDB_20190402-7.csv", row.names = FALSE)

##### writing to ArcGIS geodatabase and shapefile #####

sub_geo <- ndb
coordinates(sub_geo) <- c("Longitude", "Latitude")
proj4string(sub_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

fgdb_path <- 'C:/data/aprx/geomorph/geomorph.gdb'
arc.write(file.path(fgdb_path, 'geomorph_test'), data=sub_geo)

##### looking at what's in the incoming edits #####

dataset <- 'DWH_NRDA_NF-10-14' #'DWH_NRDA_HC-11-09', 'DWH_NRDA_NF-10-14'

edits <-  df2 %>% filter(grepl(dataset, DatasetID)) %>%
  group_by(DatasetID, Citation, AccessionID, SurveyComments, Locality, Vessel, SurveyID,
                   VehicleName, ObservationYear,
                   ObservationDate, DataProvider,
                   PI, IdentifiedBy, RecordType,
                   Reporter, DataContact,
                   Vessel, Repository, WebSite) %>%
  summarize(n=n())

View(edits)

##### looking at datasetID keys #####

dataset <- 'DWH' #'DWH_NRDA_HC-11-09', 'DWH_NRDA_NF-10-14'

oldkey <-  key %>% filter(grepl(dataset, DatasetID)) %>%
  group_by(DatasetID, class, title, method, auto_abstract,
           Revised_Dataset_Level_Citation, hand_abstract, Hourigan.Comments) %>%
  summarize(n=n())

View(oldkey)

newkey <-  key.new %>% filter(grepl(dataset, DatasetID)) %>%
  group_by(DatasetID, class, title, method, auto_abstract,
           Revised_Dataset_Level_Citation, hand_abstract, Hourigan.Comments) %>%
  summarize(n=n())

View(newkey)


# d <- filt %>% filter(grepl('Deep Sea Coral', Repository)) %>%
#   group_by(Reporter, PI, DataProvider, DatasetID, Vessel, Repository) %>%
#   summarize(n=n())
#
# View(d)
##### SurveyID queries #####
survey <- 'M2-10-06' #'NOAA_M2-10-06_AUV'

f <-  df2 %>% filter(grepl(survey, SurveyID)) %>%
  group_by(Locality, Vessel, SurveyID, EventID,
           VehicleName, ObservationYear,
           ObservationDate, DataProvider, PI,
           Reporter, DataContact, DatasetID,
           Vessel, Repository, WebSite, ImageFilePath) %>%
  summarize(n=n())
View(f)

##### Vessel queries #####

vessel <- 'McArthur II' #'NOAA_M2-10-06_AUV'

v <-  filt %>% filter(grepl(vessel, Vessel)) %>%
  group_by(Locality, Vessel, SurveyID, EventID,
           VehicleName, ObservationYear,
           ObservationDate, DataProvider, PI,
           Reporter, DataContact, DatasetID,
           Vessel, Repository, WebSite, ImageFilePath,
           Latitude, Longitude
           ) %>%
  summarize(n=n())
View(v)

##### map it #####
vessel <- 'McArthur II' #'NOAA_M2-10-06_AUV'
v <- filt %>% filter(grepl(vessel, Vessel))
d <- v

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=d,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", d$CatalogNumber, "<br>",
                        "ScientificName:", d$ScientificName, "<br>",
                        "RecordType:", d$RecordType, "<br>",
                        "Vessel:", d$Vessel, "<br>",
                        "DatasetID:", d$DatasetID, "<br>",
                        "SurveyID:", d$SurveyID, "<br>",
                        "SampleID:", d$SampleID, "<br>",
                        "TrackingID:", d$TrackingID, "<br>",
                        "Station:", d$Station, "<br>",
                        "Locality:", d$Locality, "<br>",
                        "ImageURL:", d$ImageURL, "<br>",
                        "Latitude:", d$Latitude, "<br>",
                        "Observation Year:", d$ObservationYear))
m

##### returning records that match a list of CatalogNumbers#####
setwd("C:/rworking/digs/indata")
list <- read.xlsx('list.xlsx', sheet = 1, colNames = FALSE)
list <- list$X1

d <- sub %>% filter(CatalogNumber %in% list) %>%
      group_by(CatalogNumber, Flag, FlagReason, DepthInMeters, LocationComments, ScientificName,
               IndividualCount, CategoricalAbundance, Cover) %>%
      summarize(n=n())

##### _____ leaflet comparison: compare two subsets using leaflet #####

# make two subsets for comparison
d <- filt %>% filter(grepl('HBOI_SJ-10-07', DatasetID), RecordType == 'specimen')
sub <- filt %>% filter(grepl('NOAA_SJ-10-07', DatasetID))

# map both subsets.
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=d,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", d$CatalogNumber, "<br>",
                        "ScientificName:", d$ScientificName, "<br>",
                        "RecordType:", d$RecordType, "<br>",
                        "Vessel:", d$Vessel, "<br>",
                        "DatasetID:", d$DatasetID, "<br>",
                        "SurveyID:", d$SurveyID, "<br>",
                        "SampleID:", d$SampleID, "<br>",
                        "TrackingID:", d$TrackingID, "<br>",
                        "Station:", d$Station, "<br>",
                        "Locality:", d$Locality, "<br>",
                        "ImageURL:", d$ImageURL, "<br>",
                        "Latitude:", d$Latitude, "<br>",
                        "Observation Year:", d$ObservationYear))

m <- addCircleMarkers(m, data=sub,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=3,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", sub$CatalogNumber, "<br>",
                        "ScientificName:", sub$ScientificName, "<br>",
                        "RecordType:", sub$RecordType, "<br>",
                        "Vessel:", sub$Vessel, "<br>",
                        "DatasetID:", sub$DatasetID, "<br>",
                        "SurveyID:", sub$SurveyID, "<br>",
                        "SampleID:", sub$SampleID, "<br>",
                        "TrackingID:", sub$TrackingID, "<br>",
                        "Station:", sub$Station, "<br>",
                        "Locality:", sub$Locality, "<br>",
                        "ImageURL:", sub$ImageURL, "<br>",
                        "Latitude:", sub$Latitude, "<br>",
                        "Observation Year:", sub$ObservationYear))

m

##### list the content of a google drive #####
target <- drive_get(as_id("https://drive.google.com/drive/folders/0B8lqJ4X0l6pTcnFxbTV5LVhoSms"))
drive_ls(target, recursive = F)
##### read in some shapefiles #####
setwd("C:/data/BaseLayers/NDB_data_in_AOI/") #C:\data\BaseLayers\GoldenCrabAOI\NDB_data_in_AOI
d <- readOGR(".", "aoi")

##### _____ leaflet comparison: compare two subsets using leaflet #####

# make two subsets for comparison
#d <- filt %>% filter(grepl('HBOI_SJ-10-07', DatasetID), RecordType == 'specimen')
sub <- filt %>% filter(Longitude > -86, Longitude < -84, Latitude > 25, Latitude < 28)
sub2 <- sub %>% filter(DatasetID == 'NOAA_NF-17-08')

setwd('C:/data/aprx/GoldenCrab/tables')
sub %>%
  write.csv("db.csv", row.names = FALSE)


# map both subsets.
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=d,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", d$CatalogNumber, "<br>",
                        "ScientificName:", d$ScientificName, "<br>",
                        "RecordType:", d$RecordType, "<br>",
                        "Vessel:", d$Vessel, "<br>",
                        "DatasetID:", d$DatasetID, "<br>",
                        "SurveyID:", d$SurveyID, "<br>",
                        "SampleID:", d$SampleID, "<br>",
                        "TrackingID:", d$TrackingID, "<br>",
                        "Station:", d$Station, "<br>",
                        "Locality:", d$Locality, "<br>",
                        "ImageURL:", d$ImageURL, "<br>",
                        "Latitude:", d$Latitude, "<br>",
                        "Observation Year:", d$ObservationYear))

m <- addCircleMarkers(m, data=sub,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=3,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", sub$CatalogNumber, "<br>",
                        "ScientificName:", sub$ScientificName, "<br>",
                        "RecordType:", sub$RecordType, "<br>",
                        "Vessel:", sub$Vessel, "<br>",
                        "DatasetID:", sub$DatasetID, "<br>",
                        "SurveyID:", sub$SurveyID, "<br>",
                        "SampleID:", sub$SampleID, "<br>",
                        "TrackingID:", sub$TrackingID, "<br>",
                        "Station:", sub$Station, "<br>",
                        "Locality:", sub$Locality, "<br>",
                        "ImageURL:", sub$ImageURL, "<br>",
                        "Latitude:", sub$Latitude, "<br>",
                        "Observation Year:", sub$ObservationYear))

m





##### list new DatasetID's #####
list <- setdiff(filt$DatasetID, key$DatasetID)
d <- filt %>% filter(DatasetID %in% list) %>%
  group_by(DatasetID) %>%
  summarise(
    RecordType_list = paste(unique(RecordType), collapse= " | "),
    N_Records=n(),
    DatasetID_list = paste(unique(DatasetID), collapse= " | "),
    # DataProvider_list = paste(unique(DataProvider), collapse= " | "),
    Repository_list = paste(unique(Repository), collapse= " | "),
    ObservationYear_list = paste(unique(ObservationYear), collapse= " | "),
    Vessel_list = paste(unique(Vessel), collapse= " | "),
    VehicleName_list = paste(unique(VehicleName), collapse= " | "),
    WebSite_list = paste(unique(WebSite), collapse= " | "),
    # ImageURL = paste(unique(ImageURL), collapse= " | "),
    PI_list = paste(unique(PI), collapse= " | "),
    PIAffiliation_list = paste(unique(PIAffiliation), collapse= " | "),
    Citation_list = paste(unique(Citation), collapse= " | "),
    DataContact_list = paste(unique(DataContact), collapse= " | "),
    Reporter_list = paste(unique(Reporter), collapse= " | "),
    SurveyID_list = paste(unique(SurveyID), collapse= " | "),
  )

View(d)





##### FishCouncilRegion query #####

z <- 'Caribbean'

yo <-  filt %>% filter(grepl(z, FishCouncilRegion)) %>%
  group_by(Locality, Vessel, SurveyID, EventID,
           VehicleName, ObservationYear,
           ObservationDate, DataProvider, PI,
           Reporter, DataContact, DatasetID,
           Vessel, Repository, WebSite, ImageFilePath) %>%
  summarize(n=n())
View(yo)

table(yo$FishCouncilRegion)

##### _____ extracting data based on a set of CatalogNumbers #####


##### get CatalogNumbers from Google Drive file #####
cat <- gs_title('SEDCI Regional Deep Coral Species List')
# gs_browse(key)
cat <- gs_read(cat)
cat <- data.frame(cat)

##### create a list of CatalogNumber #####
list <- cat$Catalog.
length(list)

##### filter using the list of CatalogNumber #####
d <- indata %>% filter(CatalogNumber %in% list)
length(d$CatalogNumber)
setdiff(d$CatalogNumber, list)
setdiff(list, d$CatalogNumber)
setwd("C:/rworking/digs/indata")
write.xlsx(d,'20190522_Subset_DSCRTP_NatDB_20190418-0_RPMcGuinn', row.names = FALSE)


##### images to folder operations #####

# getting urls from file
d <- filt %>% filter(is.na(ImageURL) == F, ScientificName == 'Lophelia pertusa')
d <- d[sample(nrow(d), 10),]
length(d$CatalogNumber)
urls <- d$ImageURL

# just using part of the URL for the file name
setwd("C:/rworking/deepseatools/outdata/imageset")
for (url in urls) {
  download.file(url, destfile = basename(url), mode = "wb")
}

##### Select records that have images and save to a folder with data driven file nameing #####
# be careful not to include any variable in the file name that
d <- filt %>% filter(is.na(ImageURL) == F)
d <- d[sample(nrow(d), 10),]

setwd("C:/rworking/digs/outdata/imageset")

for( i in 1:length(d$CatalogNumber)){
  download.file(as.character(d$ImageURL[i]),
                destfile = paste(d$CatalogNumber[i],
                                 d$ScientificName[i], d$IndividualCount[i], basename(as.character(d$ImageURL[i])),
                                 sep = '_'),
                mode = "wb")
}

##### _____ Howell/DSCRTP schema export #####
# here is the Google Drive file
# https://docs.google.com/spreadsheets/d/1YReS2qa1VZPFUvalLD40JhPk-sFwvv8y8l_giOi_dc0/edit?usp=sharing

###### import a list of CatalogNumber(s) #####

setwd("C:/rworking/digs/indata")
list<-read.csv("catlist.csv", header = T) #DSCRTP_NatDB_20181005-0.csv # DSCRTP_NatDB_20181211-2.csv
list <- list$CatalogNumber
d <- x %>% filter(CatalogNumber %in% list) %>%
  dplyr::select("SampleID", "CatalogNumber", "ImageFilePath", "ImageURL", "RecordType", "ScientificName",
                "VerbatimScientificName", "AphiaID", "ScientificNameAuthorship", "TaxonRank", "Morphospecies",
                "IdentifiedBy", "IdentificationComments", "IdentificationVerificationStatus", "Locality", "Latitude", "Longitude",
                "DepthInMeters", "Repository", "Citation", "Modified")

##### checking #####

length(list)
length(d$CatalogNumber)

length(c("SampleID", "CatalogNumber", "ImageFilePath", "ImageURL", "RecordType", "ScientificName",
         "VerbatimScientificName", "AphiaID", "ScientificNameAuthorship", "TaxonRank", "MorphoSpecies",
         "IdentifiedBy", "IdentificationComments", "IdentificationVerificationStatus", "Locality", "Latitude", "Longitude",
         "DepthInMeters", "Repository", "Citation", "Modified"))

###### extract duplicates from incoming list #####

list[duplicated(list)]
dim(d)

##### write xls file #####

setwd("C:/rworking/digs/indata")
write.xlsx(d,'20190524_Subset_DSCRTP_NatDB_20190418-0_RPMcGuinn.xlsx', row.names = FALSE)


##### inserting links 'hrefs' for map popups #####
# add a proper link to images for map popup in leaflet
d$ImageURLPop <- paste(sep = "", "<b><a href=", as.character(d$ImageURL),">LINK</a></b>")

# this doesn't work, but trying to get to open in new tab #
paste(sep = "", "<b><a href=", as.character(d$ImageURL[1]), " target=\"_blank\"",">LINK</a></b>")


##### working on new database corrections from Tom #####
setdiff(indata$DatasetID, indata2$DatasetID)
setdiff(indata2$DatasetID, indata$DatasetID)
setdiff(filt$DatasetID, filt2$DatasetID)
setdiff(filt2$DatasetID, filt$DatasetID)

##### make a DatasetID Inspection #####
x <- filt %>%
  filter(grepl('CT', DatasetID)) %>%
           group_by(DatasetID, EventID) %>% summarize(n=n()) %>% View






##### _____ exploring DatasetID in next DB version #####
##### bringing in datasetID key #####
# create a list of files (or single file) that meets title query
x <- drive_find(q = "name contains '20190719-0_DatasetID_Key_DSCRTP'")

# browse to it
drive_find(q = "name contains '20190719-0_DatasetID_Key_DSCRTP'") %>% drive_browse()

# getting the id as a character string
y <- x$id

# this downloads the file to the s○pecified path
dl <- drive_download(as_id(y), path = "C:/rworking/deepseatools/indata/20190709-0_DatasetID_Key_DSCRTP.xlsx", overwrite = TRUE)

# read the file into R as a data frame
key <- read.xlsx(dl$local_path)

# clean up
rm(y)
rm(x)

##### exploring DatasetID's #####

# # DSCRTP_NatDB_20190718-0
# table(indata$DatabaseVersion)

# get valid IDs
y <- filt %>%
  group_by(DatasetID) %>%
  summarize(n=n())

# length(y$DatasetID)
# 226 IDS

# getting list of only valid DatasetID's (from unflagged)
valid <- y$DatasetID

# subset DatasetID key by valid IDs
yo <- key %>%
  filter(DatasetID %in% valid)

# # checking
# length(yo$DatasetID)
# # [1] 226

yo2 <- merge(id, col3 , all.x = T)

# checking
names(yo2)

##### writing a csv #####
##### write CSV of x #####

setwd("C:/rworking/deepseatools/indata")

yo2 %>%
  write.csv(paste("20190719-0_DatasetID_Key_DSCRTP",".csv", sep = ''), row.names = FALSE)

##### testing #####

z <- setdiff(y$DatasetID, key$DatasetID)
z <- setdiff(key$DatasetID, y$DatasetID)
View(z)

yo <-  filt %>%
  filter(DatasetID %!in% z) %>%
  group_by(DatasetID) %>%
  summarise(n=n())

View(yo)

yo <- key %>% filter(grepl('PSBF', DatasetID)) %>%
  group_by(DatasetID, Comments) %>%
  summarise(n=n())

View(yo)

yo <- filt %>%
  filter(DatasetID == 'ASIZ') %>%
  group_by(DatasetID, DataProvider, WebSite, Repository, SampleID) %>%
  summarize(n=n())

View(yo)

yo <- filt %>%
  filter(DatasetID == 'ASIZ') %>%
  group_by(DatasetID, DataProvider, WebSite, Repository, SampleID) %>%
  summarize(n=n())


##### _____ #####

##### define the 'not in' function and use #####
'%!in%' <- function(x,y)!('%in%'(x,y))
valid <- key$DatasetID
length(valid)

yo <-  indata %>%
  filter(DatasetID %!in% valid) %>%
  group_by(DatasetID) %>%
  summarise(
    n=n(),
    Flags = length(Flag),
    FlagReasons = paste(unique(FlagReason), collapse = " | ")
    )

View(yo)

##### getting rid or tibble problem #####

library(tidyverse)
options(warn = 1)
tbl <- tibble(a = c(1,1), b = c(2,2))
is.null(tbl$c)

#> Warning: Unknown or uninitialised column: 'c'.
#> [1] TRUE

tbl %>% has_name("c")
#> [1] FALSE
has_name(tbl, "c")
#> [1] FALSE





##### map #####
# make two subsets for comparison
d$Latitude <- as.numeric(d$Latitude)
d$Longitude <- as.numeric(d$Longitude)

x <- d %>% filter(EventID == "D2-EX1605L1-13")
sub <- d

# map both subsets.
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1)


m <- addCircleMarkers(m, data=sub,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=3,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1)


m




##### _____ exploring the diversity metrics #####

x <- "20190719_0_CAPSTONE_AnalysisHiDensity_THourigan.xlsx"
setwd("C:/rworking/deepseatools/indata")
d <- read.xlsx(x, sheet = 1)
d <- d %>% filter(EventID != 'D2-EX1605L1-13',
                  is.na(ScientificName) == F)
##### setting depth categories #####
d$DepthCat[as.numeric(d$DepthInMeters) <= 1000] <- "shallow"
d$DepthCat[as.numeric(d$DepthInMeters) > 1000] <- "deep"
d$DepthCat <- factor(d$DepthCat, levels = c("shallow", "deep"))

##### setting location categories #####
d$loc[as.numeric(d$Latitude) > 10] <- "north"
d$loc[as.numeric(d$Latitude) < 10] <- "south"

##### setting temperature categories #####
d$temp_cat[as.numeric(d$Temperature) > 5] <- "warm: > 5"
d$temp_cat[as.numeric(d$Temperature) >= 3 & as.numeric(d$Temperature) <= 5] <- "cold: 3 to 5"
d$temp_cat[as.numeric(d$Temperature) < 3] <- "colder: less than 3"
d$temp_cat <- factor(d$temp_cat, levels = c("warm: > 5", "cold: 3 to 5", "colder: less than 3"))

##### setting oxygen categories #####
# qplot(as.numeric(d$Oxygen))
d$ox_cat[as.numeric(d$Oxygen) >= 4] <- "rich"
d$ox_cat[as.numeric(d$Oxygen) < 4] <- "poor"

##### longitude cat #####
d$longloc[as.numeric(d$Longitude) > 90] <- "west"
d$longloc[as.numeric(d$Longitude) < 90] <- "east"
##### create site X species matrix #####

library(vegan)
site.sp <- dcast(d, EventID ~ ScientificName, value.var = "IndividualCount", fun.aggregate = sum)

# creating a site variable
site.sp$site <- site.sp$EventID

# getting rid of non-needed variables
site.sp <- site.sp %>%
  dplyr::select(-EventID)

# # moving the site variable to the beginning
# col_idx <- grep("site", names(site.sp))
# site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# # names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  dplyr::select(-site)

# making it a matrix
site.sp <- as.matrix(site.sp)

x <- d %>%
  filter(
    #DepthCat == 'deep',
    #EventID != "D2-EX1605L1-13"
    #Phylum == "Porifera"
  )

##### create site X species matrix #####
library(vegan)
site.sp <- dcast(x, EventID ~ ScientificName, value.var = "IndividualCount", fun.aggregate = sum)

# setwd("C:/rworking/digs/outdata")
# write.csv(site.sp, 'site.sp.csv')

# creating a site variable
site.sp$site <- site.sp$EventID

# getting rid of non-needed variables
site.sp <- site.sp %>%
  dplyr::select(-EventID)

# # moving the site variable to the beginning
# col_idx <- grep("site", names(site.sp))
# site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# # names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  dplyr::select(-site)

# making it a matrix
site.sp <- as.matrix(site.sp)

##### running NMDS starting with site X species matrix #####
#install.packages("vegan")
library(vegan)
NMDS <- metaMDS(site.sp, distance = "bray", binary = F, k=3, trymax = 30)


##### extracting the site and species scores for use with ggplot2 #####
#Using the scores function from vegan to extract the site scoresand convert to a data.frame
site.scores <- as.data.frame(scores(NMDS))
site.scores$site <- rownames(site.scores)  # create a column of site names, from the rownames of data.scores
# head(site.scores)  #look at the data
# merge information from d (to get categories)
site.scores <- merge(site.scores, x, by.x = 'site', by.y = 'EventID')
#site.scores
site.scores <- site.scores %>% group_by(site) %>% summarise(NMDS1 = NMDS1[1],
                                                            NMDS2 = NMDS2[1],
                                                            DepthCat = DepthCat[1],
                                                            temp_cat = temp_cat[1],
                                                            loc = loc[1])





# extracting species scores
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
# head(species.scores)  #look at the data
species.scores$spec_code <- 1:(length(species.scores$species))


##### plotting NMDS #####
ggplot() +
  geom_point(data=site.scores,aes(x=NMDS2,y=NMDS1, color=DepthCat), size=5) + # add the point markers
  geom_text(data=site.scores,aes(x=NMDS2,y=NMDS1, label = site), size=4, position = position_nudge(y = -0.02)) + # add the text
  #geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2), shape = 3, size=4) +
  coord_equal() +
  theme_bw(base_size = 15, base_family = "Cambria") +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


ggplot() +
  geom_point(data=site.scores,aes(x=NMDS2,y=NMDS1, color=temp_cat), size=5) + # add the point markers
  geom_text(data=site.scores,aes(x=NMDS2,y=NMDS1, label = site), size=4, position = position_nudge(y = -0.02)) + # add the text
  #geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2), shape = 3, size=4) +
  coord_equal() +
  theme_bw(base_size = 15, base_family = "Cambria") +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

ggplot() +
  geom_point(data=site.scores,aes(x=NMDS2,y=NMDS1, color=loc), size=5) + # add the point markers
  geom_text(data=site.scores,aes(x=NMDS2,y=NMDS1, label = site), size=4, position = position_nudge(y = -0.02)) + # add the text
  #geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2), shape = 3, size=4) +
  coord_equal() +
  theme_bw(base_size = 15, base_family = "Cambria") +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


ggplot() +
  #geom_point(data=site.scores,aes(x=NMDS2,y=NMDS1, color=loc), size=5) + # add the point markers
  geom_text(data=species.scores,aes(x=NMDS2,y=NMDS1, label = spec_code), size=4, position = position_nudge(y = -.03)) + # add the text
  geom_point(data=species.scores,aes(x=NMDS2,y=NMDS1), shape = 3, size=4) +
  coord_equal() +
  theme_bw(base_size = 15, base_family = "Cambria") +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())





##### looking at diversity metrics
H <- diversity(site.sp)
simp <- diversity(site.sp, "simpson")
invsimp <- diversity(site.sp, "inv")

## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(site.sp, 2) - 1

## Fisher alpha
alpha <- fisher.alpha(site.sp)

## Species richness (S) and Pielou's evenness (J):
S <- specnumber(site.sp) ## rowSums(site.sp > 0) does the same...
J <- H/log(S)

# plot matrix
pairs(cbind(H, simp, invsimp, unbias.simp, alpha, J), pch="+", col="blue")

##### prep to map it #####

df <- data.frame(H)
# View(plots_df)
df$EventID <- row.names(df)
# names(d)
x <- merge(df, d, all.x = T)
# length(d$EventID)
# length(unique(d$EventID))
x <- d %>% group_by(EventID) %>% summarize(n=n(),
                                           alpha = paste(mean(as.numeric(alpha)), collapse = ' | '),
                                           Latitude = paste(mean(as.numeric(Latitude)), collapse = ' | '),
                                           Longitude = paste(mean(as.numeric(Longitude)), collapse = ' | '),
                                           DepthInMeters = paste(mean(as.numeric(DepthInMeters), collapse = ' | ')))



##### map it #####
x$Latitude <- as.numeric(x$Latitude)
x$Longitude <- as.numeric(x$Longitude)
x$alpha <- as.numeric(alpha)
x$DepthInMeters <- as.numeric(x$DepthInMeters)

x <- x %>% filter(x$DepthInMeters > 1500)

pal <- colorNumeric(
  palette = "Reds",
  domain = as.numeric(x$alpha))


m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      #fillColor= "green",
                      color = ~pal(alpha),
                      fillOpacity=1,
                      popup = paste("EventID:", x$EventID, "<br>"))

m

qplot(x$DepthInMeters, x$alpha)
dang <- scatter.smooth(x=x$DepthInMeters, y=x$alpha, main="Depth ~ Alpha")
cor(x$DepthInMeters, x$alpha)
linearMod <- lm(DepthInMeters ~ alpha, data=x)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
modelSummary <- summary(linearMod)


##### _____ MBARI analysis #####
d <- filt %>% filter(DatasetID == 'MBARI')

##### create site X species matrix #####

library(vegan)
d$EventID <- paste(d$ObservationYear, d$Vessel, d$EventID)

site.sp <- dcast(d, EventID ~ ScientificName, value.var = "IndividualCount")

# creating a site variable
site.sp$site <- site.sp$EventID

# getting rid of non-needed variables
site.sp <- site.sp %>%
  dplyr::select(-EventID)

# # moving the site variable to the beginning
# col_idx <- grep("site", names(site.sp))
# site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# # names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  dplyr::select(-site)

# making it a matrix
site.sp <- as.matrix(site.sp)


##### looking at diversity metrics
H <- diversity(site.sp)
simp <- diversity(site.sp, "simpson")
invsimp <- diversity(site.sp, "inv")

## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(site.sp, 2) - 1

## Fisher alpha
alpha <- fisher.alpha(site.sp)

## Species richness (S) and Pielou's evenness (J):
S <- specnumber(site.sp) ## rowSums(site.sp > 0) does the same...
J <- H/log(S)

# plot matrix
pairs(cbind(H, simp, invsimp, unbias.simp, alpha, J), pch="+", col="blue")

##### prep to map it #####

df <- data.frame(unbias.simp)
# View(plots_df)
df$EventID <- row.names(df)
# names(d)
x <- merge(df, d, all.x = T)
# length(d$EventID)
# length(unique(d$EventID))
x <- d %>% group_by(EventID) %>% summarize(n=n(),
                                           unbias.simp = paste(mean(as.numeric(unbias.simp)), collapse = ' | '),
                                           Latitude = paste(mean(as.numeric(Latitude)), collapse = ' | '),
                                           Longitude = paste(mean(as.numeric(Longitude)), collapse = ' | '),
                                           DepthInMeters = paste(mean(as.numeric(DepthInMeters), collapse = ' | ')))


x$Latitude <- as.numeric(x$Latitude)
x$Longitude <- as.numeric(x$Longitude)
x$unbias.simp <- as.numeric(unbias.simp)
x$DepthInMeters <- as.numeric(x$DepthInMeters)


y <- x %>% filter(x$unbias.simp > .1)
qplot(y$DepthInMeters, y$unbias.simp)
scatter.smooth(x=y$DepthInMeters, y=y$unbias.simp)
scatter.smooth(x=y$Latitude, y=y$unbias.simp)
scatter.smooth(x=y$Longitude, y=y$unbias.simp)

cor(y$DepthInMeters, y$alpha)

##### _____ North Pacific analysis #####
d <- filt %>% filter(FishCouncilRegion == 'North Pacific')

##### create site X species matrix #####

library(vegan)
d$EventID <- paste(d$ObservationYear, d$Vessel, d$EventID)

site.sp <- dcast(d, EventID ~ ScientificName, value.var = "IndividualCount")

# creating a site variable
site.sp$site <- site.sp$EventID

# getting rid of non-needed variables
site.sp <- site.sp %>%
  dplyr::select(-EventID)

# # moving the site variable to the beginning
# col_idx <- grep("site", names(site.sp))
# site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# # names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  dplyr::select(-site)

# making it a matrix
site.sp <- as.matrix(site.sp)


##### looking at diversity metrics
H <- diversity(site.sp)
simp <- diversity(site.sp, "simpson")
invsimp <- diversity(site.sp, "inv")

## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(site.sp, 2) - 1

## Fisher alpha
alpha <- fisher.alpha(site.sp)

## Species richness (S) and Pielou's evenness (J):
S <- specnumber(site.sp) ## rowSums(site.sp > 0) does the same...
J <- H/log(S)

# plot matrix
pairs(cbind(H, simp, invsimp, unbias.simp, alpha, J), pch="+", col="blue")

##### prep to map it #####

df <- data.frame(H)
# View(plots_df)
df$EventID <- row.names(df)
# names(d)
x <- merge(df, d, all.x = T)
# length(d$EventID)
# length(unique(d$EventID))
x <- d %>% group_by(EventID) %>% summarize(n=n(),
                                           H = paste(mean(as.numeric(H)), collapse = ' | '),
                                           Latitude = paste(mean(as.numeric(Latitude)), collapse = ' | '),
                                           Longitude = paste(mean(as.numeric(Longitude)), collapse = ' | '),
                                           DepthInMeters = paste(mean(as.numeric(DepthInMeters), collapse = ' | ')))


x$Latitude <- as.numeric(x$Latitude)
x$Longitude <- as.numeric(x$Longitude)
x$H <- as.numeric(H)
x$DepthInMeters <- as.numeric(x$DepthInMeters)


y <- x %>% filter(x$H > .1, x$DepthInMeters < 1000)
qplot(y$DepthInMeters, y$H)
scatter.smooth(x=y$DepthInMeters, y=y$H)
scatter.smooth(x=y$Latitude, y=y$H)
scatter.smooth(x=y$Longitude, y=y$H)

cor(y$DepthInMeters, y$alpha)


##### _____ SouthEast analysis #####
d <- filt %>% filter(FishCouncilRegion == 'South Atlantic')

##### create site X species matrix #####

library(vegan)
d$EventID <- paste(d$ObservationYear, d$Vessel, d$EventID)

site.sp <- dcast(d, EventID ~ ScientificName, value.var = "IndividualCount")

# creating a site variable
site.sp$site <- site.sp$EventID

# getting rid of non-needed variables
site.sp <- site.sp %>%
  dplyr::select(-EventID)

# # moving the site variable to the beginning
# col_idx <- grep("site", names(site.sp))
# site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# # names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  dplyr::select(-site)

# making it a matrix
site.sp <- as.matrix(site.sp)


##### looking at diversity metrics
H <- diversity(site.sp)
simp <- diversity(site.sp, "simpson")
invsimp <- diversity(site.sp, "inv")

## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(site.sp, 2) - 1

## Fisher alpha
alpha <- fisher.alpha(site.sp)

## Species richness (S) and Pielou's evenness (J):
S <- specnumber(site.sp) ## rowSums(site.sp > 0) does the same...
J <- H/log(S)

# plot matrix
pairs(cbind(H, simp, invsimp, unbias.simp, alpha, J), pch="+", col="blue")

##### prep to map it #####

df <- data.frame(H)
# View(plots_df)
df$EventID <- row.names(df)
# names(d)
x <- merge(df, d, all.x = T)
# length(d$EventID)
# length(unique(d$EventID))
x <- d %>% group_by(EventID) %>% summarize(n=n(),
                                           H = paste(mean(as.numeric(H)), collapse = ' | '),
                                           Latitude = paste(mean(as.numeric(Latitude)), collapse = ' | '),
                                           Longitude = paste(mean(as.numeric(Longitude)), collapse = ' | '),
                                           DepthInMeters = paste(mean(as.numeric(DepthInMeters), collapse = ' | ')))


x$Latitude <- as.numeric(x$Latitude)
x$Longitude <- as.numeric(x$Longitude)
x$H <- as.numeric(H)
x$DepthInMeters <- as.numeric(x$DepthInMeters)


y <- x %>% filter(x$H > .1)
qplot(y$DepthInMeters, y$H)
scatter.smooth(x=y$DepthInMeters, y=y$H)
scatter.smooth(x=y$Latitude, y=y$H)
scatter.smooth(x=y$Longitude, y=y$H)

cor(y$DepthInMeters, y$alpha)


##### _____ GOMEX analysis #####
d <- filt %>% filter(FishCouncilRegion == 'Gulf of Mexico')

##### create site X species matrix #####

library(vegan)
d$EventID <- paste(d$ObservationYear, d$Vessel, d$EventID)

site.sp <- dcast(d, EventID ~ ScientificName, value.var = "IndividualCount")

# creating a site variable
site.sp$site <- site.sp$EventID

# getting rid of non-needed variables
site.sp <- site.sp %>%
  dplyr::select(-EventID)

# # moving the site variable to the beginning
# col_idx <- grep("site", names(site.sp))
# site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# # names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  dplyr::select(-site)

# making it a matrix
site.sp <- as.matrix(site.sp)


##### looking at diversity metrics
H <- diversity(site.sp)
simp <- diversity(site.sp, "simpson")
invsimp <- diversity(site.sp, "inv")

## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(site.sp, 2) - 1

## Fisher alpha
alpha <- fisher.alpha(site.sp)

## Species richness (S) and Pielou's evenness (J):
S <- specnumber(site.sp) ## rowSums(site.sp > 0) does the same...
J <- H/log(S)

# plot matrix
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha, J), pch="+", col="blue")

##### prep to map it #####

df <- data.frame(H)
# View(plots_df)
df$EventID <- row.names(df)
# names(d)
x <- merge(df, d, all.x = T)
# length(d$EventID)
# length(unique(d$EventID))
x <- d %>% group_by(EventID) %>% summarize(n=n(),
                                           H = paste(mean(as.numeric(H)), collapse = ' | '),
                                           Latitude = paste(mean(as.numeric(Latitude)), collapse = ' | '),
                                           Longitude = paste(mean(as.numeric(Longitude)), collapse = ' | '),
                                           DepthInMeters = paste(mean(as.numeric(DepthInMeters), collapse = ' | ')))


x$Latitude <- as.numeric(x$Latitude)
x$Longitude <- as.numeric(x$Longitude)
x$H <- as.numeric(H)
x$DepthInMeters <- as.numeric(x$DepthInMeters)


y <- x %>% filter(x$H > .1)
qplot(y$DepthInMeters, y$H)
scatter.smooth(x=y$DepthInMeters, y=y$H)
scatter.smooth(x=y$Latitude, y=y$H)
scatter.smooth(x=y$Longitude, y=y$H)

cor(y$DepthInMeters, y$alpha)












##### calling erddap #####
library(rerddap)
d <- tabledap("deep_sea_corals",
              fields=c('CatalogNumber', 'latitude', 'longitude', 'ScientificName', 'ImageURL',
                       'Vessel', 'RecordType', 'DatasetID', 'SurveyID', 'SampleID', 'TrackingID',
                       'Station', 'Locality', 'ObservationYear', 'Genus', 'Phylum', 'TaxonRank',
                       'DepthInMeters', 'ScientificNameAuthorship'),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/")

##### creating a summary statistics table #####
# Here’s another example that again uses the states.csv dataset.

# Say we wanted to create a table with summary statistics for five of the variables in this dataset:

  sumstat <- states %>%

  # Select and rename five variables
  select(
    `Black (%)` = blkpct,
    `Attend church (%)` = attend_pct,
    `Supported Bush in 2000 (%)` = bush00,
    `Supported Obama in 2008 (%)` = obama08,
    `Women in State Legislature (%)` = womleg
  ) %>%

  # Find the mean, st. dev., min, and max for each variable
  summarise_each(funs(mean, sd, min, max)) %>%

  # Move summary stats to columns
  gather(key, value, everything()) %>%
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%

  # Set order of summary statistics
  select(variable, mean, sd, min, max) %>%

  # Round all numeric variables to one decimal point
  mutate_each(funs(round(., 1)), -variable)

sumstat

# variable mean  sd  min  max
# 1              Attend church (%) 38.9 9.4 22.0 60.0
# 2                      Black (%) 10.3 9.7  0.4 36.8
# 3     Supported Bush in 2000 (%) 50.4 8.7 31.9 67.8
# 4    Supported Obama in 2008 (%) 50.5 9.5 32.5 71.8
# 5 Women in State Legislature (%) 23.2 7.3  8.8 37.8
# Write to .txt

write.table(sumstat, file = "sumstats.txt", sep = ",", quote = FALSE, row.names = F)

# Again, the sumstats.txt file will end up in your working directory,
# and you can use steps 3 and 4 from the Overview section above to import this file into Word.


##### _____ Dashboard links list #####

# index page: https://deepseacoraldata.noaa.gov/DatasetID_Table/DatasetID_Table_20190718-0.html
# dashboard page: https://deepseacoraldata.noaa.gov/Dataset%20Summaries/NOAA_FGBNMS_PSBF-I.html

d <- filt %>%  filter(grepl("Bassett", DataContact) |
                        grepl("Bassett", Reporter) |
                        grepl("Salgado", Reporter) |
                        grepl("Salgado", DataContact) |
                        grepl("Dubick", Reporter) |
                        grepl("Dubick", DataContact) |
                        grepl("Wickes", Reporter) |
                        grepl("Wickes", DataContact) |
                        grepl("Shuler", Reporter) |
                        grepl("Shuler", DataContact) |
                        grepl("Frometa", DataContact) |
                        grepl("Frometa", Reporter) |
                        grepl("Etnoyer", Reporter) |
                        grepl("Proux", DataContact) |
                        grepl("Proux", Reporter)
                      )

d$DashboardURL <- paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/",d$DatasetID,".html", sep = '' )

x <-d %>% group_by(DatasetID) %>%
  summarize(DataContact = paste(unique(DataContact), collapse=" | "),
            Reporter = paste(unique(Reporter), collapse=" | "),
            DashboardURL = paste(unique(DashboardURL), collapse=" | "))

View(x)

setwd("C:/rworking/deepseatools/indata")
write.xlsx(x,'20190822_0_DCEL_related_datasets_RPMcGuinn.xlsx',row.names = FALSE)

##### another way to create a pasted url list in a for loop ######

# index page: https://deepseacoraldata.noaa.gov/DatasetID_Table/DatasetID_Table_20190718-0.html
# dashboard page: https://deepseacoraldata.noaa.gov/Dataset%20Summaries/NOAA_FGBNMS_PSBF-I.html

d <- filt %>%  filter(grepl("Bassett", DataContact) |
                        grepl("Bassett", Reporter) |
                        grepl("Salgado", Reporter) |
                        grepl("Salgado", DataContact) |
                        grepl("Dubick", Reporter) |
                        grepl("Dubick", DataContact) |
                        grepl("Wickes", Reporter) |
                        grepl("Wickes", DataContact) |
                        grepl("Shuler", Reporter) |
                        grepl("Shuler", DataContact) |
                        grepl("Frometa", DataContact) |
                        grepl("Frometa", Reporter) |
                        grepl("Etnoyer", Reporter) |
                        grepl("Proux", DataContact) |
                        grepl("Proux", Reporter)
)



y <- NA
for (id in levels(factor(d$DatasetID))){
  x <- paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/",id,".html", sep = '' )
  y <- append(y,x)
}





##### CitationMaker #####
sub$CitationMaker <- paste(sub$DataProvider,'. ',
                               min(sub$ObservationYear),' to ',
                               max(sub$ObservationYear),'. ',
                               'Coral or sponge occurrence observations submitted to the NOAA National Database for Deep Sea Corals (www.deepseacoraldata.noaa.gov)', '. ',
                               'DSCRTP Dataset ID: ', sub$DatasetID, '. ',
                               'DSCRTP Accession ID: ',sub$AccessionID, '. ',
                               'Record type: ', sub$RecordType, '. ',
                               'Vessel(s): ', sub$Vessel,'. ',
                               'Sampling vehicle: ', sub$VehicleName,'. ',
                               'Survey ID: ', sub$SurveyID,'. ',
                               'Principle investigator: ', sub$PI,'. ',
                               'Data contact: ', sub$DataContact,'. ',
                               'Reporter: ', sub$Reporter,'. ',
                               'Repository: ', sub$Repository,'. ',
                               'Web site [last accessed on YYYY-MM-DD]: ', sub$WebSite,'.',
                               sep = '')

unique(sub$CitationMaker)


##### getting Darwin Core terms #####
https://raw.githubusercontent.com/tdwg/dwc/master/dist/simple_dwc_horizontal.csv





##### combine packer tow cam sets #####
setwd("C:/rworking/deepseatools/indata")
x <- read.csv("20190815-0_NOAA_NEFSC_Connecticut_ISIS2_TowCam_Packer_Part_2_2013_2013.csv", header = T)
y <- read.csv("20190819-4_NOAA_NEFSC_Connecticut_ISIS2_TowCam_Packer_Part_3_2013_2013.csv", header = T)
yo <- rbind(x,y)
#write.csv(yo, "20190828-0_NOAA_NEFSC_Connecticut_ISIS2_TowCam_Packer_Part_2_and_3_2013_2013.csv")
z <- filt %>% filter(DatasetID == "NOAA_CT-13-07")

##### checking #####
filt %>% filter(grepl('Messing', PI) | grepl('Messing', IdentifiedBy)) %>%
  arrange(ObservationYear) %>%
  group_by(DatasetID, PI, IdentifiedBy, Vessel, ObservationYear, Locality) %>%
  summarize(n = n()) %>% View()

##### checking #####
# DSCRTP_NatDB_20181012-1
db <- 'DSCRTP_NatDB_20170301-0.csv' #20170301-0 and 20170323-0

setwd("C:/rworking/deepseatools/indata")
indata<-read.csv(db, header = T)
filt <- indata %>%
  filter(Flag == "0")

filt %>% filter(CatalogNumber == '458474') %>%
  group_by(Station, CatalogNumber, ObservationDate) %>%
  summarize(n = n()) %>% View

filt %>% filter(Station == '11-Mar') %>%
  group_by(CatalogNumber, Station, ObservationDate, Locality) %>%
  summarize(n = n()) %>% View

##### Muriceides kÃ¼kenthali #####
filt %>% filter(grepl('kenthali', ScientificName)) %>%
  group_by(ScientificName, CatalogNumber) %>% summarize(n=n()) %>% View()
  )

##### list of taxa #####

x <- wm_records_taxamatch(name = c("Adelogorgia cf. phyllosclera", "Putamayo", "Adelogorgia", "Lophelia pertusa"))
bind_rows(x, .id = "column_label")

##### DepthChecking loop (getting things out of the loop with a data.frame) #####

setwd("C:/rworking/deepseatools/indata")
master <- read.csv("master.csv", header = T)
incoming <- read.csv("incoming.csv", header = T)
incoming$index <- seq(1:length(incoming$ScientificName))

df <- data.frame(ScientificName = character(),
                 index = numeric(),
                 test1 = character(),
                 test2 = character(),
                 stringsAsFactors=FALSE)

for (id in incoming$index){
  x <- incoming %>% filter(index == id)
  y <- master %>% filter(ScientificName == x$ScientificName)
  z <- x$DepthInMeters > y$MinimumDepthInMeters  # if this is true and
  r <- x$DepthInMeters < y$MaximumDepthInMeters # this is true, then all is good
  d <- data.frame(ScientificName=x$ScientificName,
                  index = x$index,
                  test1 = z,
                  test2 = r,
                  stringsAsFactors=FALSE)
  df <- rbind(df, d)
}

##### species accumulation curves #####
data(BCI)
sp1 <- specaccum(BCI)
sp2 <- specaccum(BCI, "random")
sp2
summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

##### obistools #####
#match_taxa()performs interactive taxon matching with the World Register of Marine Species.
names <- c("Madracis", "Lophelia")
match_taxa(names)

##### woods hole oceanographic institute #####

filt %>% filter(grepl('Woods Hole', DataProvider) |
                  grepl('Woods Hole', PIAffiliation) |
                  grepl('WHOI_AT', DatasetID)) %>%
                  group_by(DatasetID, PIAffiliation, DataProvider, PI, Vessel, gisMEOW, ObservationYear) %>%
                  summarize(n=n()) %>% View()








##### #####
##### get OBIS data #####

obisdata <- occurrence("Bathypterois")
obisdata <- occurrence("Ipnopidae")

##### _____ filter data in OBIS #####

# table(obisdata$collectionCode, useNA = 'always')

x <- obisdata# %>%
  #filter(collectionCode == 'DEEPWATER SYSTEMATICS') # 'ARC' 'SAE Biological records'

#table(x$decimalLatitude, useNA = 'always')
x$decimalLatitude <- as.numeric(x$decimalLatitude)
x$decimalLongitude <- as.numeric(x$decimalLongitude)

##### map OBIS data in leaflet #####
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      lat = x$decimalLatitude,
                      lng = x$decimalLongitude,
                      radius=5,
                      weight=0,
                      fillColor=colors,
                      fillOpacity=1,
                      popup = paste("scientificName:", x$scientificName, "<br>",
                                    "basisOfRecord:", x$basisOfRecord, "<br>",
                                    "bibliographicCitation:", x$bibliographicCitation, "<br>",
                                    "depth:", x$depth, "<br>",
                                    "locality:", x$locality, "<br>",
                                    "datasetName:", x$datasetName, "<br>",
                                    "collectionCode:", x$collectionCode, "<br>",
                                    "eventDate:", x$eventDate, "<br>",
                                    "institutionID:", x$institutionID, "<br>",
                                    "footprintWKT:", x$footprintWKT, "<br>",
                                    "ownerInstitutionCode:", x$ownerInstitutionCode,"<br>",
                                    "associatedMedia:", x$associatedMedia))
m

##### _____ DarwinCore emof #####

##### bring in emof table #####

setwd("C:/rworking/deepseatools/indata")
emof <- read.csv("DSCRTP_EMOF_Subset_2019-10-11.csv", header = T)

##### extract a single numerical variable and summarize #####

table(emof$measurementType)
var <- "cover"
x <- emof %>% filter(measurementType == var)
View(table(x$measurementValue))

##### extract just records of a particular level of measurementValue #####

y <- x %>% filter(measurementValue == 'DeepWorker 6 DSR/V')
length(y$occurrenceID)

##### extract a single numerical variable and summarize #####

table(emof$measurementType)
var <- "temperature"
x <- emof %>% filter(measurementType == var)
x$measurementValue <- as.character(x$measurementValue)
x$measurementValue <- as.numeric(x$measurementValue)
summary(x$measurementValue)
hist(x$measurementValue)

##### checking on depth issues #####

x <- sub %>% filter(CatalogNumber == '971624') #%>%
  #dplyr::select(Ocean, CatalogNumber, Flag, Ocean, Latitude, Longitude, DepthInMeters,
                gisCRMDepth, gisGEBCODepth, gisEtopoDepth, gisIHOSeas)


##### map it ######

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                     # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", x$CatalogNumber, "<br>",
                        "DepthInMeters:", x$DepthInMeters, "<br>",
                        "gisCRMDepth:", x$gisCRMDepth, "<br>",
                        "gisGEBCODepth:", x$gisGEBCODepth, "<br>",
                        "gisLandCheck:", x$gisLandCheck, "<br>",
                        "Latitude:", x$Latitude, "<br>",
                        "Longitude:", x$Longitude, "<br>",
                        "ImageURL:", x$ImageURL
                      ))
m

##### write x to shapefile #####
#install.packages('arcgisbinding')
library(arcgisbinding)
arc.check_product()

x <- sub %>% filter(VernacularNameCategory == 'gorgonian coral')
x_geo <- x

coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

fgdb_path <- 'C:/data/aprx/geozones/geozones.gdb'
arc.write(file.path(fgdb_path, 'x_geo'), data=x_geo, overwrite = TRUE)


##### checking incoming against standardized tables #####
Vessel <- length(setdiff(unique(sub$Vessel), unique(filt$Vessel)))
VehicleName <- length(setdiff(unique(sub$VehicleName), unique(filt$VehicleName)))
PI <- length(setdiff(unique(sub$PI), unique(filt$PI)))
PIAffiliation <- length(setdiff(unique(sub$PIAffiliation), unique(filt$PIAffiliation)))
Repository <- length(setdiff(unique(sub$Repository), unique(filt$Repository)))
IdentifiedBy <- length(setdiff(unique(sub$IdentifiedBy), unique(filt$IdentifiedBy)))
IdentificationQualifier <- length(setdiff(unique(sub$IdentificationQualifier), unique(filt$IdentificationQualifier)))
DataProvider <- length(setdiff(unique(sub$DataProvider), unique(filt$DataProvider)))
DatasetID <- length(setdiff(unique(sub$DatasetID), unique(filt$DatasetID)))
SurveyID <- length(setdiff(unique(sub$SurveyID), unique(filt$SurveyID)))

filt %>% filter(Vessel == unique(sub$Vessel)) %>% group_by(SurveyID) %>% summarize(n=n())
filt %>% filter(Vessel == unique(sub$Vessel)) %>% group_by(DatasetID) %>% summarize(n=n())

##### Rearranging Columns #####

##### find missing variables #####
namevector<-setdiff(names(tax), names(x))

for(i in namevector)
  indata[,i] <- NA

## Step 12: Match Template Column Order #####
indata<-indata[,c(TMPL_FullVariables)]

##### _____ get modified and original datasets via Excel #####
setwd("C:/rworking/deepseatools/indata")
sub <- read.xlsx('20191108-1_NOAA_OER_EX1806_NCarolina_Morrison_Sautter_2018_2018-TH.xlsx', sheet = 1)

##### working on checking identifiedby #####

x <- sub %>% filter(sub$RecordType == 'specimen') %>%
  group_by(IdentifiedBy) %>%
  summarize(RecordType = paste(unique(RecordType), collapse=" | "),
            Class = paste(unique(Class), collapse=" | "),
            Order = paste(unique(Order), collapse=" | "),
            ScientificName = paste(unique(ScientificName), collapse = " | "),
            SampleID = paste(unique(SampleID), collapse = " | "))


##### write file to csv
setwd("C:/rworking/deepseatools/indata")
x %>%
  write.csv("20191118-0_summary of IdentifiedBy at version 20191108-1_RPMcGuinn.csv", row.names = FALSE)

##### rgbif #####
x <- occ_search(scientificName = "Ursus americanus", limit = 50)

setwd("C:/rworking/deepseatools/indata/dwca-nmnh_extant_dwc-a-v1.25")
sub <- read.table("occurrence.txt", header = T, fill = TRUE)


d <- data.frame(x$data)

##### helping Meredith  E. #####

x <- filt %>% filter(DatasetID == 'NOAA_NWFSC_Bottom_Trawl_Survey') %>%
  group_by(TrackingID, EventID, DatasetID) %>%
  summarize(n=n())


##### export 'sub' to GIS file x_geo (in geozones aprx) #####

setwd('c:/rworking/deepseatools/code')
source('export_to_gis.R')

##### httr and GET from REST URLs #####
library(httr)
library(jsonlite)
# A simple GET request
node <- 'https://service.ncddc.noaa.gov/arcgis/rest/services/OceanExploration/OE_OkeanosDives/MapServer/13'
r <- GET(node)
str(content(r))
r <- content(r, as = "text", encoding = "UTF-8")
df <- fromJSON(r, flatten = TRUE)

##### This should allow you to plot the dive directly from the internet #####
# and the plot your points from a CSV based on the dive (i.e. EventID)

library(leaflet)
#install.packages("leaflet.esri")
library(leaflet.esri)
leaflet() %>%
  addEsriBasemapLayer(esriBasemapLayers$Oceans) %>%
  addEsriFeatureLayer(
    url = "https://service.ncddc.noaa.gov/arcgis/rest/services/OceanExploration/OE_OkeanosDives/MapServer/65",
    useServiceSymbology = TRUE, markerType ="marker")

x <- jsonlite::fromJSON("https://service.ncddc.noaa.gov/arcgis/rest/services/OceanExploration/OE_OkeanosDives/MapServer/65")

data_url <- "https://service.ncddc.noaa.gov/arcgis/rest/services/OceanExploration/OE_OkeanosDives/MapServer/"
x <- download.file(data_url, "65.geojson")



##### _____ New corals vs. old corals #####

rm(filt)
rm(indata)

##### Getting old corals and sponges #####

setwd("C:/rworking/deepseatools/indata")
indata_old<-read.csv("DSCRTP_NatDB_20170807-1.csv", header = T)
filt_old <- indata_old %>%
  filter(Flag == "0")

##### Getting new corals and sponges #####

setwd("C:/rworking/deepseatools/indata")
indata_new<-read.csv("DSCRTP_NatDB_20190920-0.csv", header = T)
filt_new <- indata_new %>%
  filter(Flag == "0")

##### Bringing in most recent corals #####

# 20191125-1_University_of_Hawaii_Smith_Durden_Kilo_Moana_KM1808_2018_2018
# 20191125-1_HBOI_Pisces_and_Nancy_Foster_Harter_Reed_Farrington_2012_2017
# 20191118-1_HBOI_Walton_Smith_Cuba_Reed_Farrington_2017_2017
# 20191118-0_NOAA_OER_EX1806_NCarolina_Morrison_Sautter_2018_2018
# 20191112-0_University_of_Hawaii_Kelley_Nautilus_NA101_2018_2018
# 20191107-3_NOAA_OER_EX1803_GOMEX_Wagner_2018_2018

setwd("C:/rworking/deepseatools/indata")
a <-read.csv("20191125-1_University_of_Hawaii_Smith_Durden_Kilo_Moana_KM1808_2018_2018.csv", header = T)
filt_a <- a %>%
  filter(Flag == "0")

setwd("C:/rworking/deepseatools/indata")
b <-read.csv("20191125-1_HBOI_Pisces_and_Nancy_Foster_Harter_Reed_Farrington_2012_2017.csv", header = T)
filt_b <- b %>%
  filter(Flag == "0")

setwd("C:/rworking/deepseatools/indata")
c <-read.csv("20191118-1_HBOI_Walton_Smith_Cuba_Reed_Farrington_2017_2017.csv", header = T)
filt_c <- c %>%
  filter(Flag == "0")

setwd("C:/rworking/deepseatools/indata")
d <-read.csv("20191118-0_NOAA_OER_EX1806_NCarolina_Morrison_Sautter_2018_2018.csv", header = T)
filt_d <- d %>%
  filter(Flag == "0")

setwd("C:/rworking/deepseatools/indata")
e <-read.csv("20191112-0_University_of_Hawaii_Kelley_Nautilus_NA101_2018_2018.csv", header = T)
filt_e <- e %>%
  filter(Flag == "0")

setwd("C:/rworking/deepseatools/indata")
f <-read.csv("20191107-3_NOAA_OER_EX1803_GOMEX_Wagner_2018_2018.csv", header = T)
filt_f <- f %>%
  filter(Flag == "0")

##### Putting together one file that includes all #####

filt_new <- rbind(filt, filt_a, filt_b, filt_c, filt_d, filt_e, filt_f)

##### Create a not-in function #####
`%ni%` = Negate(`%in%`)

##### Find only new records since after FY17 #####
filt_new_2_only <- filt_new_2 %>% filter(CatalogNumber %ni% filt_old$CatalogNumber)

##### _____ export to ArcGIS
old <- filt_old %>% dplyr::select(CatalogNumber, Latitude, Longitude)
new <- filt_new_2_only %>% dplyr::select(CatalogNumber, Latitude, Longitude)

##### filtering out a problem with longitude #####
new_fixed <- new %>% filter(as.numeric(Longitude) > -200)

##### install packages #####

library(arcgisbinding)
arc.check_product()

##### create spdf #####

old_geo <- old
coordinates(old_geo) <- c("Longitude", "Latitude")
proj4string(old_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### create feature-class #####

fgdb_path <- 'C:/data/aprx/RTC2020/RTC2020.gdb'
arc.write(file.path(fgdb_path, 'old_geo'), data=old_geo, overwrite = TRUE)

##### create spdf

new_geo <- new_fixed
coordinates(new_geo) <- c("Longitude", "Latitude")
proj4string(new_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### create feature-class #####

fgdb_path <- 'C:/data/aprx/RTC2020/RTC2020.gdb'
arc.write(file.path(fgdb_path, 'new_geo'), data=new_geo, overwrite = TRUE)

##### data request from Peter Etnoyer on 2019-12-03 #####

# No of Swiftia in Gulf of Mexico added to DB in years 2016-2019
# No of Swiftia in Gulf of Mexico available in years <= to 2015
#
# No of Hypnorgorgia in Gulf of Mexico added to DB in years 2016-2019
# No of Hypnogorgia in Gulf of Mexico available in years <= to 2015

yo <- filt %>% filter(Genus == "Hypnogorgia" | Genus == "Swiftia", FishCouncilRegion == "Gulf of Mexico")

yo$EntryDateCat <- as.Date(yo$EntryDate) < as.Date('2015-12-30')

yo %>% group_by(EntryDateCat, Genus) %>% summarize(n=n(), IndividualCount = sum(IndividualCount))

#### _____ export to ArcGIS
yo_geo <- yo %>% dplyr::select(CatalogNumber, Latitude, Longitude)

##### install packages #####

library(arcgisbinding)
arc.check_product()

##### create spdf #####

coordinates(yo_geo) <- c("Longitude", "Latitude")
proj4string(yo_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### create feature-class #####

fgdb_path <- 'C:/data/aprx/geozones/geozones.gdb'
arc.write(file.path(fgdb_path, 'yo_geo'), data=yo_geo, overwrite = TRUE)

##### match function #####

name <- na.omit(check[match(USNM$SampleID, check$SampleID),])

##### creating spatial lines: from Elizabeth Gugliotti on 20191213 #####

library(sp)
library(maptools)
library(leaflet)
# Creating the points_to_line function
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {

  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)

  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }

  # If there is only one path...
  if (is.null(id_field)) {

    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))

    return(lines)

    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {

    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])

    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))

    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }

    return(sp_lines)
  }
}


#Create nav_lines
nav_lines <- points_to_line (data=rovnav,
                             long='Long',
                             lat='Lat',
                             id_field='TransectNumber',
                             sort_field='TC')

#have to rename rownames of your data to the row names of nav_lines
rownames(nav_unique)<-c("line1","line2","line3","line4","line5","line6","line7","line8","line9","line10","line11","line12","line13","line14","line15","line16","line17","line18","line19","line20","line21","line22","line23","line24","line25","line26","line27","line28","line29","line30","line31","line32","line33","line34","line35","line36","line37","line38","line39","line40","line41","line42","line43","line44","line45","line46","line47","line48","line49","line50","line51","line52","line53","line54","line55","line56","line57","line58","line59","line60","line61","line62","line63","line64","line65","line66","line67","line68","line69")

#Create Spatial Data Frame that has lines plus your data
SLDF = SpatialLinesDataFrame(nav_lines, nav_unique)

pal<-colorNumeric("viridis", domain = NULL)
leaflet(data=SLDF) %>%
  addTiles()%>%
  addPolylines(color = ~pal(Dive))

##### github source #####

script <- getURL("https://raw.githubusercontent.com/opetchey/RREEBES/Beninca_development/Beninca_etal_2008_Nature/report/functions/indirect_method_functions.R", ssl.verifypeer = FALSE)

yo <-readr::read_csv("https://raw.githubusercontent.com/Atrebas/atrebas.github.io/master/post/2019-03-03-datatable-dplyr.R")


class(yo)

##### quick filters datasetiD #####
x <- "OET_NA101"

date <- filt %>% filter(DatasetID == x) %>%
  group_by(ObservationDate) %>%
  summarize(n=n())
View(date)

type <- filt %>% filter(DatasetID == x) %>%
  group_by(RecordType) %>%
  summarize(n=n())
View(type)

vessel <- filt %>% filter(DatasetID == x) %>%
  group_by(Vessel) %>%
  summarize(n=n())
View(vessel)

equip <- filt %>% filter(DatasetID == x) %>%
  group_by(SamplingEquipment) %>%
  summarize(n=n())
View(equip)

vehicle <- filt %>% filter(DatasetID == x) %>%
  group_by(VehicleName) %>%
  summarize(n=n())
View(vehicle)

web <- filt %>% filter(DatasetID == x) %>%
  group_by(WebSite) %>%
  summarize(n=n())
View(web)

citation <- filt %>% filter(DatasetID == x) %>%
  group_by(Citation) %>%
  summarize(n=n())
View(citation)

purpose <- filt %>% filter(DatasetID == x) %>%
  group_by(Purpose) %>%
  summarize(n=n())
View(purpose)

comments <- filt %>% filter(DatasetID == x) %>%
  group_by(SurveyComments) %>%
  summarize(n=n())
View(comments)

survey <- filt %>% filter(DatasetID == x) %>%
  group_by(Vessel, SurveyID, ObservationYear) %>%
  summarize(n=n())
View(survey)

locality <- filt %>% filter(DatasetID == x) %>%
  group_by(Locality) %>%
  summarize(n=n())
View(locality)


##### quick summary for Tom Hourigan RTC #####

x <- filt %>%
  filter(DatasetID == "NOAA_SS-12-08" |
           DatasetID == "NOAA_AE-14-04" |
           DatasetID == "NOAA_VA-14-08") %>%
  group_by(DatasetID, EventID, VernacularNameCategory) %>%
  summarize(sum_IndividualCount = sum(IndividualCount),
            n_records = n())
  #View()

setwd("C:/rworking/deepseatools/indata")
write.xlsx(x, "20190109-0_summary_Bering_Aleutian_dropcam_NDB_20191217-0_RPMcGuinn.xlsx")


##### get image from Google Drive #####
head(d$Image)
i = ''
i <- gs_title(i)
gs_browse(taxfl)






























##### trouble-shooting dataset NOAA_PU-11-08 #####
x <- filt %>% filter(DatasetID == 'NOAA_PU-11-08') %>%
  group_by(ObservationDate, ObservationYear, SamplingEquipment) %>%
  summarize(n=n())

table(filt$SamplingEquipment)

x <- filt %>% filter(DatasetID == 'NOAA_PU-14-13') %>%
  group_by(ObservationDate, ObservationYear, SamplingEquipment) %>%
  summarize(n=n())

table(filt$SamplingEquipment)

##### helping Lindsey Kraatz #####

# get list of file names in Google Folder

x <- drive_ls(path = 'NOAA')
x <- arrange(x, desc(name))

setwd("C:/rworking/deepseatools/indata")

write.xlsx(x, '20190117-0_Lindsey Kraatz_images.xlsx')

# initial separation of file names

y <- x %>% separate(name, c('ScientificName', 'DepthInMeters', 'Locality'), sep = ',')

# extract depths

y$DepthInMeters2 <- as.numeric(str_extract(y$DepthInMeters, "[0-9]+"))

##### get images #####
#29	Plumarella sp., 494 m West Florida Escarpment, NOAA-Pelagic Research Services.jpg

name <- "Plumarella"
yo <- 29
z <- filt %>% filter(is.na(ImageURL) == F,
                #ScientificName == y$ScientificName[yo],
                grepl(name, ScientificName)
                DepthInMeters > y$DepthInMeters2[yo] - 1,
                DepthInMeters < y$DepthInMeters2[yo] + 1
                )

setwd("C:/rworking/deepseatools/indata/imageset")
for(i in 1:length(z$CatalogNumber)){
  download.file(as.character(z$ImageURL[i]),
                destfile = paste(z$CatalogNumber[i],
                                 z$ScientificName[i],
                                 z$DepthInMeters[i],
                                 z$Locality[i],
                                 basename(as.character(z$ImageURL[i])),
                                 sep = '_'),
                mode = "wb")
}





