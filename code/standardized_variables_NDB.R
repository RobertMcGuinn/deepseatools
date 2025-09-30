##### Heading #####
# author: Robert P. McGuinn
# date started: 20200113
# purpose: publish a set of standardized tables for NDB each quarter

##### packages #####
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
# library(arcgisbinding)
# arc.check_product()
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

##### load National DB #####
## old
setwd("C:/rworking/deepseatools/indata")
indata_old <- read.csv("DSCRTP_NatDB_20200408-1.csv", header = T)
filt_old <- indata_old %>%
  filter(Flag == "0")

## new
setwd("C:/rworking/deepseatools/indata")
indata <- read.csv("DSCRTP_NatDB_20200710-2.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

##### strip off DatasetID #####
datasetID_old <- unique(filt_old$DatasetID)
datasetID_new <- unique(filt$DatasetID)

## checking
# setdiff(datasetID_old, datasetID_new)
# setdiff(datasetID_new, datasetID_old)

##### create standardized tables of key variables at latest DB version #####
##### _IdentificationQualifier #####
version <- unique(filt$DatabaseVersion)

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

setwd("C:/rworking/deepseatools/reports/")
x %>%
  write.csv(paste("IdentificationQualifier_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

##### _SurveyID #####
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

x %>%
  write.csv(paste("SurveyID_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

##### _DataProvider #####
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

x %>%
  write.csv(paste("DataProvider_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

##### _DatasetID #####
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

x %>%
  write.csv(paste("DatasetID_NatDB_", version, ".csv", sep = ''), row.names = FALSE)


##### _Repository #####
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

x %>%
  write.csv(paste("Repository_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

##### _Vessel #####

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

x %>%
  write.csv(paste("Vessel_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

##### _VehicleName #####

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

x %>%
  write.csv(paste("VehicleName_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

##### _PI #####

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

x %>%
  write.csv(paste("PI_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

##### _PIAffiliation #####

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

x %>%
  write.csv(paste("PIAffiliation_NatDB_", version, ".csv", sep = ''), row.names = FALSE)

##### _IdentifiedBy #####
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

x %>%
  write.csv(paste("IdentifiedBy_NatDB_", version, ".csv", sep = ''), row.names = FALSE)



##### check #####
filt %>% pull(DatasetID) %>% unique() %>% print()
