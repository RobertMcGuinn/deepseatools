##### Header #####
# author: Robert McGuinn
# date started: 20190121
# purpose: create a Minimum Viable Product (MVP) for export from the
#       NOAA National Database for Deep Sea Corals and Sponges
#       to Abby Benson for ingest to OBIS / GBIF

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
library(extrafont)
#loadfonts()
library(RColorBrewer)
library(naniar)

##### load NDB from local file #####
## [manual]
setwd("C:/rworking/deepseatools/indata")
filename <- "DSCRTP_NatDB_20220801-0.csv"
indata<-read_csv(filename,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = 'ISO-8859-1'),
                 na = c("-999", "NA"))

##### filter the NDB (look closely at how this is done. #####
## this deserves inspection.
flagged <- indata %>%  filter(Flag == "1")
filt <- indata %>% filter(Flag == "0")

## cleanup
rm(indata)

##### creating errdap_link field #####
filt$references <- paste('https://www.ncei.noaa.gov/erddap/tabledap/deep_sea_corals.csv?ShallowFlag%2CDatasetID%2CCatalogNumber%2CSampleID%2CCitation%2CRepository%2CScientificName%2CVernacularNameCategory%2CTaxonRank%2CIdentificationQualifier%2CLocality%2Clatitude%2Clongitude%2CDepthInMeters%2CDepthMethod%2CObservationDate%2CSurveyID%2CStation%2CEventID%2CSamplingEquipment%2CLocationAccuracy%2CRecordType%2CDataProvider&CatalogNumber=',
                          filt$CatalogNumber, sep = '')

##### fix Depth issues #####
filt$MinimumDepthInMeters <- ifelse(test = is.na(filt$MinimumDepthInMeters) == T, yes = filt$DepthInMeters, no = filt$MinimumDepthInMeters)
filt$MaximumDepthInMeters <- ifelse(test = is.na(filt$MaximumDepthInMeters) == T, yes = filt$DepthInMeters, no = filt$MaximumDepthInMeters)

##### fix species issues #####
# filt$ScientificName <- ifelse(test = filt$CatalogNumber == '932164', yes = "Narella", no = filt$ScientificName)
# filt$ScientificName <- ifelse(test = filt$CatalogNumber == '932389', yes = "Narella", no = filt$ScientificName)

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
    datasetID = 'DatasetID'
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

## check

obis %>% pull(coordinateUncertaintyInMeters) %>% table(useNA = 'always')

##### write out file for submission #####
today <- '20220824-0'
version <- unique(filt$DatabaseVersion)
setwd('C:/rworking/deepseatools/indata')
obis %>%
  write.csv(paste(today,'_export_to_OBIS_DSCRTP_NDB_', version , '_RPMcGuinn', '.csv', sep = ''),
            row.names = FALSE)

##### checking #####
library(dplyr)

# this checks for records with duplicate CatalogNumbers.
# There should be none. If you find any, alert the entire team immediately.

x <- indata %>%
  group_by(CatalogNumber) %>%
  filter(n()>1) %>% pull(CatalogNumber) %>% length()

write.csv(x, "c:/rworking/deepseatools/indata/dups.csv")

names(obis)

x <- filt %>% pull(CatalogNumber) %>% length()
y <- obis %>% pull(occurrenceID) %>% length()
x-y


obis %>% pull(DatabaseVersion) %>% table(useNA = 'always')
obis %>% pull(datasetID) %>% table(useNA = 'always')
obis %>%
  filter(is.na(scientificName) == T) %>%
  pull(scientificName) %>%
  table(useNA = 'always')
obis %>% pull(individualCount) %>% table(useNA = 'always')
head(obis$individualCount)
plot(obis$individualCount)
filt %>%
  filter(IndividualCount == '21895') %>%
  pull(RecordType)
obis %>%
  filter(is.na(scientificNameID) == F) %>%
  pull(scientificNameID) %>%
  table(useNA = 'always')
head(obis$scientificName)
head(obis$occurrenceID)
head(obis$eventDate)
head(obis$minimumDepthInMeters)
head(obis$maximumDepthInMeters)
table(obis$basisOfRecord)
obis %>%
  filter(is.na(associatedMedia) == T) %>%
  pull(associatedMedia) %>%
  table(useNA = 'always')
head(obis$references)
table(obis$occurrenceStatus)


##### DarwinCore crosswalk for other fields we might use later on #####
# recordNumber = "TrackingID",
# associatedMedia = "ImageURL",
# associatedReferences = "Citation",
# institutionCode = "Repository",
# vernacularName = "VernacularNameCategory",
# taxonRank = "TaxonRank",
# phylum = "Phylum",
# class = "Class",
# order = "Order",
# family = "Family",
# genus = "Genus",
# subgenus = "Subgenus",
# species = "Species",
# specificEpithet = "Subspecies",
# scientificNameAuthorship = "ScientificNameAuthorship",
# typeStatus = "TypeStatus",
# identificationRemarks = "IdentificationComments",
# identifiedBy = "IdentifiedBy",
# dateIdentified = "IdentificationDate",
# identificationQualifier = "IdentificationQualifier",
# associatedSequences = "AssociatedSequences",
# waterBody = "Ocean",
# country = "Country",
# locality = "Locality",
# locationRemarks = "LocationComments",
# year = "ObservationYear",
# eventTime = "ObservationTime",
# parentEventID = "SurveyID",
# sailingVessel = "Vessel",
# eventRemarks = "SurveyComments",
# eventID = "EventID",
# samplingEquipment = "SamplingEquipment",
# vehicleName = "VehicleName",
# sampleSizeValue = "SampleAreaInSquareMeters",
# footprintWKT = "footprintWKT",
# footprintSRS = "footprintSRS",
# individualCount = "IndividualCount",
# categoricalAbundance = "CategoricalAbundance",
# organismDensity = "Density",
# cover = "Cover",
# verbatimSize = "VerbatimSize",
# minimumSize = "MinimumSize",
# maximumSize = "MaximumSize",
# weight = "WeightInKg",
# condition = "Condition",
# associatedTaxa = "AssociatedTaxa",
# occurrenceRemarks = "OccurrenceComments",
# startLatitude = "StartLatitude",
# startLongitude = "StartLongitude",
# endLatitude = "EndLatitude",
# endLongitude = "EndLongitude",
# verbatimLatitude = "VerbatimLatitude",
# verbatimLongitude = "VerbatimLongitude",
# coordinateUncertaintyInMeters = "LocationAccuracy",
# georeferenceProtocol = "NavType",
# habitat = "Habitat",
# substrate = "Substrate",
# geoformCMECS = "CMECSGeoForm",
# substrateCMECS = "CMECSSubstrate",
# bioticCMECS = "CMECSBiotic",
# temperature = "Temperature",
# salinity = "Salinity",
# oxygen = "Oxygen",
# pH = "pH",
# pHScale = "pHscale",
# pCO2 = "pCO2",
# totalAlkalinity = 'TA',
# dissolvedInorganicCarbon = "DIC",
# type = "RecordType",
# ownerInstitutionCode = "DataProvider",
# modified = "Modified",
# references = "WebSite",
# higherGeography = "gisMEOW")

# # add sampleSizeUnit column and populate all non-NA sampleSizeValue rows with "Square Meters"
# obis$sampleSizeUnit <- ifelse(is.na(obis$sampleSizeValue) == FALSE, "Square Meters", NA)

##### later we will work on emof "extended measurement or fact" variables #####

# # create a list of emof fields and occurenceID
# emof_fields <- emof %>% select(measurementType) %>% pull()
# emof_fields_o <- append(emof_fields[[1]], "occurrenceID", after = 0)
#
# # select from emof dataframe Type, Unit and Method
# emof_tum <- select(emof, measurementType, measurementUnit, measurementMethod)
#
# # pull emof_fields from the main DSC dataset
# # stretch columns into a tall dataframe using gather
# # remove rows with NA values
# # join tall dataframe with emof type, unit method
# # add uuid for each record
# DB_emof_subset <- DB_subset2  %>%
#   select(emof_fields_o) %>%
#   gather(key = "measurementType", value = "measurementValue", -occurrenceID) %>%
#   drop_na() %>%
#   inner_join(emof_tum, by = "measurementType") %>%
#   mutate(measurementID=uuid())
#
# # remove emof fields from DSC Dataset
# DSCRTP_Occurrences <- DB_subset2 %>% select(-one_of(emof_fields))
#
# write_csv(DB_emof_subset, paste("DSCRTP_EMOF_Subset_", Sys.Date(), ".csv"))
#
# write_csv(DSCRTP_Occurrences, paste("DSCRTP_Occurrences_", Sys.Date(), ".csv"))
#
# print("Script Finished")
