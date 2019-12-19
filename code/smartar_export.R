##### Header #####
# author: Robert P. McGuinn
# date_started: 20191217
# purpose: export specific fields for SMARTAR project.
# google_drive: https://drive.google.com/open?id=1B7HsNo-JqKZ4FEzmbMVhd4QgxU6fAmJn

##### load packages #####

# install.packages('openxlsx')
library(openxlsx)
# library(sp)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('rerddap')
# library(rerddap)
#install.packages('leaflet')
# library(leaflet)
# install.packages('extrafont')
# library(extrafont)
# install.packages('RColorBrewer')
# library(RColorBrewer)
# install.packages('googlesheets')
library(googlesheets)
# install.packages('googledrive')
library(googledrive)
# library(rmarkdown)
# library(knitr)
#install.packages('maps')
# library(maps)
#install.packages('rgdal')
# library(rgdal)
#install('raster')
# library(raster)
#install.packages('spocc')
# library(spocc)
#install.packages('arcgisbinding')
#install.packages('refinr')
# library(refinr)
# # install.packages('marmap')
# library(marmap)
# #install.packages('prettydoc')
# library(prettydoc)
# #install.packages('robis')
# library(robis)
# #install.packages('devtools')
# library(devtools)
# install.packages('digest')
# install.packages('data.table')
# install.packages('DBI')
# install.packages('assertthat')
# install.packages('Rcpp')
# install.packages('rerddap')
# install.packages('magrittr')
# install.packages('curl')
library(magrittr)
# library(Rcpp)
# library(assertthat)
# library(DBI)
# library(devtools)
# library(data.table)
# library(digest)
# library(rerddap)
# library(curl)
# library(reshape2)
# # install.packages('gdoc')
# # install.packages('compareDF')
# #install.packages('compareDF')
# library(compareDF)
# #install.packages('flextable')
# library(flextable)
# #install.packages('officer')
# library(officer)
# #install.packages('kableExtra')
# #library(kableExtra)
# #install.packages('worrms')
# library(worrms)
# # install.packages('taxize')
# library(taxize)
# # install.packages('devtools')
# library(devtools)
# #devtools::install_github('iobis/obistools')
# library(obistools)
# library(jsonlite) #https://cran.r-project.org/web/packages/jsonlite/
# library(httr)
# # install.packages('rgbif')
# library(rgbif)

##### load NDB #####
# setwd("C:/rworking/deepseatools/indata")
# indata<-read.csv("DSCRTP_NatDB_20190920-0.csv", header = T)
# filt <- indata %>%
#   filter(Flag == "0")

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



