##### Heading #####
# author: Robert P. McGuinn
# date started: 20200113
# purpose: publish a set of standardized tables for NDB each quarter

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

version <- '201901217-0'

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

