##### Header #####
# author: Robert McGuinn | robert.mcguinn@noaa.gov | rpm@alumni.duke.edu
# date started: 20241023
# purpose: create a Darwin Core compliant Minimum Viable Product (MVP) for export from the
#       NOAA National Database for Deep Sea Corals and Sponges
#     \ingest to OBIS / GBIF

##### packages #####
library(tidyverse)
library(dplyr)
library(rmarkdown)
library(knitr)
library(flextable)
library(googlesheets4)
library(googledrive)
library(openxlsx)
library(scales)
library(RColorBrewer)
library(naniar)

##### load NDB from local file (manual: take a lood a version in mod_load_current)#####
source('C:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### check the version #####
unique(filt$DatabaseVersion)

##### creating errdap_link field called 'references' #####
## this creates a single atomized link to erddap per CatalogNumber
filt$references <- paste('https://www.ncei.noaa.gov/erddap/tabledap/deep_sea_corals.csv?ShallowFlag%2CDatasetID%2CCatalogNumber%2CSampleID%2CCitation%2CRepository%2CScientificName%2CVernacularNameCategory%2CTaxonRank%2CIdentificationQualifier%2CLocality%2Clatitude%2Clongitude%2CDepthInMeters%2CDepthMethod%2CObservationDate%2CSurveyID%2CStation%2CEventID%2CSamplingEquipment%2CLocationAccuracy%2CRecordType%2CDataProvider&CatalogNumber=',
                          filt$CatalogNumber, sep = '')

##### fix Depth issues #####
filt$MinimumDepthInMeters <- ifelse(test = is.na(filt$MinimumDepthInMeters) == T,
                                    yes = filt$DepthInMeters,
                                    no = filt$MinimumDepthInMeters)
filt$MaximumDepthInMeters <- ifelse(test = is.na(filt$MaximumDepthInMeters) == T,
                                    yes = filt$DepthInMeters,
                                    no = filt$MaximumDepthInMeters)

##### OPTIONAL: fix species issues #####
# filt$AphiaID <- ifelse(test = filt$AphiaID == '602367', yes = "125286", no = filt$AphiaID)
# filt$AphiaID <- ifelse(test = filt$AphiaID == '1287836', yes = "1287835", no = filt$AphiaID)
# filt$AphiaID <- ifelse(test = filt$AphiaID == '169028', yes = "133874", no = filt$AphiaID)
# filt$AphiaID <- ifelse(test = filt$AphiaID == '288491', yes = "209983", no = filt$AphiaID)

##### check #####
# oldaphiaid <- '602367'
# newaphiaid <- '125286'
#
# oldaphiaid <- '1287836'
# newaphiaid <- '1287835'
#
# oldaphiaid <- '169028'
# newaphiaid <- '133874'
#
# oldaphiaid <- '288491'
# newaphiaid <- '209983'

# yo <- filt %>% filter(AphiaID == oldaphiaid) %>% pull(ScientificName)
# length(yo)
# unique(yo)
#
# yo <- filt %>% filter(AphiaID == newaphiaid) %>% pull(ScientificName)
# length(yo)
# unique(yo)

##### get rid of records with missing ObservationDate #####
filt <- filt %>% filter(is.na(ObservationDate) == F)

##### create a minimum list of fields to release to OBIS #####
obis_fields <- c('DatabaseVersion',
                 'DatasetID',
                 'ScientificName',
                 'IndividualCount',
                 'AphiaID',
                 'CatalogNumber',
                 'ObservationDate',
                 'Latitude',
                 'Longitude',
                 'LocationAccuracy',
                 'MinimumDepthInMeters',
                 'MaximumDepthInMeters',
                 'RecordType',
                 'ImageURL',
                 'Locality',
                 'Citation',
                 'references'
                 )

##### filter for fields being passed to OBIS and change names to match OBIS terms #####
obis <- filt %>%
  dplyr::select(one_of(obis_fields)) %>%
  rename(
    scientificName = 'ScientificName', # verbatim
    scientificNameID = 'AphiaID', # requires modification
    occurrenceID = 'CatalogNumber', # requires modification
    individualCount = 'IndividualCount', # verbatim
    eventDate = 'ObservationDate', # verbatim
    decimalLatitude = 'Latitude', # verbatim
    decimalLongitude = 'Longitude', # verbatim
    coordinateUncertaintyInMeters = 'LocationAccuracy', #verbatim
    minimumDepthInMeters = 'MinimumDepthInMeters', # verbatim
    maximumDepthInMeters = 'MaximumDepthInMeters', # verbatim
    basisOfRecord = 'RecordType', # requires modification
    associatedMedia = 'ImageURL', # verbatim
    locality = 'Locality', # verbatim
    datasetID = 'DatasetID', # verbatim
    associatedReferences = 'Citation' # verbatim
    )

obis$occurrenceStatus <- 'Present'

##### make the required modifications tranformations #####
# paste 'NOAA_DSCRTP:' as prefix on CatalogNumber to create the proper OBIS occurrenceID
# paste 'urn:lsid:marinespecies.org:taxname:'onto AphiaID to create scientificNameID

obis <- obis %>%
  mutate(occurrenceID = paste("NOAA_DSCRTP:", occurrenceID, sep = "")) %>%
  mutate(scientificNameID = paste("urn:lsid:marinespecies.org:taxname:", scientificNameID, sep = ""))

##### recode of DSC RecordType to 'basisOfRecord' # #####
recode_list <- list('specimen' = 'PreservedSpecimen',
                 'literature' = 'HumanObservation',
                 'still image' = 'MachineObservation',
                 'video observation' = 'MachineObservation',
                 'video transect' = 'MachineObservation',
                 'notation' = 'HumanObservation',
                 'catch record' = 'HumanObservation',
                 'specimen/lot' = 'PreservedSpecimen',
                 'still image transect' = 'MachineObservation',
                 'video and still images' = 'MachineObservation',
                 'literature | specimen' = 'PreservedSpecimen',
                 'still image | video observation' = 'MachineObservation',
                 'still image | specimen' = 'PreservedSpecimen',
                 'specimen | video observation' = 'PreservedSpecimen',
                 'video observation | specimen' = 'MachineObservation')

# use RecordType to type crosswalk to apply type valid values:
obis$basisOfRecord <- recode(obis$basisOfRecord, !!!recode_list)

##### work on coordinateUncertaintyInMeters to take out non-numericals  #####
obis$coordinateUncertaintyInMeters <- gsub("[^[:digit:]., ]", "", obis$coordinateUncertaintyInMeters)

##### check #####
# obis %>% pull(coordinateUncertaintyInMeters) %>% table(useNA = 'always')

##### add and delete variables for dwca #####
obis$id <- obis$occurrenceID
obis$kingdom <- 'Animalia'
obis$geodeticDatum <- 'WGS84'
obis <- obis %>%  select(-DatabaseVersion)

##### transform CoordinateUncertaintyInMeters #####
obis$coordinateUncertaintyInMeters <- as.numeric(obis$coordinateUncertaintyInMeters)

##### check #####
# table(obis$basisOfRecord, useNA = 'always')
# obis %>% filter(is.na(basisOfRecord) == T) %>% View()
# filt %>% filter(CatalogNumber == 1606484) %>% pull(AccessionID)

##### OPTIONAL: fix where basisOfRecord is null #####
obis <- obis %>%
  mutate(basisOfRecord = case_when(
    is.na(basisOfRecord) ~ "MachineObservation",
    TRUE ~ basisOfRecord
  ))

##### write out file for submission (manual) #####
today <- '20250721-0'
version <- unique(filt$DatabaseVersion)
setwd('C:/rworking/deepseatools/indata')
obis %>%
  write.csv(paste('dwc_noaa_dsc_rtp_version', '_', version , '_', today, '.txt', sep = ''),
            row.names = FALSE, na = '')

##### check #####
# length(unique(filt$ScientificName))
#
# filt %>% filter(TaxonRank == 'species') %>%
#   pull(AphiaID) %>%
#   unique() %>%
#   length()

##### Use read.delim() to read the file back in for testing #####
data <- read.delim(
  'c:/rworking/deepseatools/indata/dwc_noaa_dsc_rtp_version_20241219-1_20250205-0.txt',
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE)

##### check #####
# data %>%  filter(occurrenceID == "NOAA_DSCRTP:1557739") %>% pull(associatedMedia)
# obis %>%  filter(occurrenceID == "NOAA_DSCRTP:1557739") %>% pull(scientificName)

