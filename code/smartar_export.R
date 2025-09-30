##### Header #####
# author: Robert P. McGuinn
# date_started: 20191217
# purpose: export specific fields for SMARTAR project.
# google_drive: https://drive.google.com/open?id=1B7HsNo-JqKZ4FEzmbMVhd4QgxU6fAmJn

##### load packages #####
library(openxlsx)
library(tidyverse)
library(googlesheets)
library(googledrive)
library(magrittr)

##### load NDB #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")


##### creating a list of CatalogNumbers of interest #####
setwd("C:/rworking/deepseatools/indata")
cats <- read.xlsx('catalognumbers.xlsx', sheet = 1)
cats <- as.character(cats$CatalogNumber)

##### defining fields from NDB that feed SMARTAR tables #####

otufields <- c('CatalogNumber',
               'OperationalTaxonomicUnit',
               'ScientificName',
               'LifeScienceIdentifier',
               'ScientificNameAuthorship',
               'TaxonRank',
               'Morphospecies',
               'VerbatimScientificName',
               'IdentificationComments',
               'HighlightImageFilePath',
               'HighlightImageURL')

imagefields <- c('CatalogNumber',
                 'OperationalTaxonomicUnit',
                 'ImageFilePath',
                 'RecordType',
                 'IdentifiedBy',
                 'IdentificationDate',
                 'IdentificationComments',
                 'IdentificationVerificationStatus',
                 'TypeStatus',
                 'ImageFilePath',
                 'Locality',
                 'Latitude',
                 'Longitude',
                 'MinimumDepthInMeters',
                 'MaximumDepthInMeters',
                 'DataProvider',
                 'Citation',
                 'Modified',
                 'VerbatimScientificName',
                 'SampleID',
                 'CMECSSubstrate',
                 'Habitat',
                 'MaximumSize',
                 'CatalogNumber')

##### setting up LifeScienceIdentifier #####

filt$LifeScienceIdentifier <- paste('urn:lsid:marinespecies.org:taxname', filt$AphiaID, sep = ':')

##### making the otu table #####
otu_table <- filt %>% filter(CatalogNumber %in% cats) %>%  dplyr::select(otufields)

##### making the image table #####
image_table <- filt %>% filter(CatalogNumber %in% cats) %>%  dplyr::select(imagefields)

#####  write to excel (non-looping) #####
setwd("C:/rworking/deepseatools/indata")
write.xlsx(otu_table,'20191218-0_otu_table_RPMcGuinn.xlsx', row.names = FALSE)

setwd("C:/rworking/deepseatools/indata")
write.xlsx(image_table,'20191218-0_image_table_RPMcGuinn.xlsx', row.names = FALSE)

##### -OR- just generate an ERDDAP query #####
# example query: https://ecowatch.ncddc.noaa.gov/erddap/tabledap/deep_sea_corals.csv?ShallowFlag%2CDatasetID%2CCatalogNumber%2CSampleID%2CRepository%2CScientificName%2CVernacularNameCategory%2CTaxonRank%2CIdentificationQualifier%2CLocality%2Clatitude%2Clongitude%2CDepthInMeters%2CDepthMethod%2CObservationDate%2CSurveyID%2CStation%2CEventID%2CSamplingEquipment%2CLocationAccuracy%2CRecordType%2CDataProvider&CatalogNumber=845&CatalogNumber=879
otudap <- c('CatalogNumber',
               'OperationalTaxonomicUnit',
               'ScientificName',
               'LifeScienceIdentifier',
               'ScientificNameAuthorship',
               'TaxonRank',
               'Morphospecies',
               'VerbatimScientificName',
               'IdentificationComments',
               'HighlightImageFilePath',
               'HighlightImageURL')

imagedap <- c('CatalogNumber',
                 'OperationalTaxonomicUnit',
                 #'ImageFilePath',
                 'RecordType',
                 'IdentifiedBy',
                 'IdentificationDate',
                 'IdentificationComments',
                 'IdentificationVerificationStatus',
                 'TypeStatus',
                 #'ImageFilePath',
                 'Locality',
                 'latitude',
                 'longitude',
                 'MinimumDepthInMeters',
                 'MaximumDepthInMeters',
                 'DataProvider',
                 'Citation',
                 'Modified',
                 'VerbatimScientificName',
                 'SampleID',
                 'CMECSSubstrate',
                 'Habitat',
                 'MaximumSize'
                 )


x <- paste('https://ecowatch.ncddc.noaa.gov/erddap/tabledap/deep_sea_corals.csv?',
      paste(imagedap, sep="", collapse="%2C"),
      '&CatalogNumber=',
      paste(cats, sep="", collapse="&CatalogNumber="), sep = '')



