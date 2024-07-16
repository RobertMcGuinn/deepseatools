##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose:

##### linkage #####
filename <- '132586' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### create sub #####
sub <- filt %>% filter(CatalogNumber == '533764')

##### read in the data inventory sheet from google drive #####
sub$image_credit <- paste('Image: ', sub$ScientificName, ' at a depth of ', sub$DepthInMeters, ' meters at ', sub$Locality,
                          ' in the ', sub$gisMEOW, '. ',
                          'Data provider: ', sub$DataProvider,'. ','Image date: ', sub$ObservationDate, '. ',
                          'Coral occurrences submitted to the NOAA, National Database for Deep Sea Corals and Sponges (www.deepseacoraldata.noaa.gov/data)', '. ',
                          #'DSCRTP Accession ID: ',sub$AccessionID, '. ',
                          #'Record type: ', sub$RecordType, '. ',
                          #'Vessel(s): ', sub$Vessel,'. ',
                          #'Sampling vehicle: ', sub$VehicleName,'. ',
                          #'Survey ID: ', sub$SurveyID,'. ',

                          'Principal investigator(s): ', sub$PI,'. ',
                          'Reporter: ', sub$Reporter, '. ',
                          'DSCRTP Dataset ID: ', sub$DatasetID, '. ',
                          'DSCRTP Database version: ', sub$DatabaseVersion, '. ',
                          'CatalogNumber: ', sub$CatalogNumber,
                          #'Data contact: ', sub$DataContact,'. ',
                          #'Reporter: ', sub$Reporter,'. ',
                          #'Repository: ', sub$Repository,'. ',
                          # 'Web site [last accessed on YYYY-MM-DD]: ', sub$WebSite,'.',
                          sep = '')
sub$image_credit




