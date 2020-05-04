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
library(googlesheets)
library(googledrive)
library(openxlsx)
library(scales)
library(extrafont)
#loadfonts()
library(RColorBrewer)

##### load NDB #####
setwd("C:/rworking/deepseatools/indata")
indata<-read_csv("DSCRTP_NatDB_20200408-1.csv", na = c("-999", "NA"))

##### filter the NDB #####
filt <- indata %>%
  filter(Flag == "0") %>%
  filter(AccessionID != 'NMNH_Smithsonian_update_Q2-2020_THourigan' |
           CatalogNumber != '878942') %>%
  filter(AccessionID != 'NMNH_Smithsonian_update_Q2-2020_THourigan' |
           CatalogNumber != '878945')

##### removing records that do not have AphiaID #####
filt <- filt %>% filter(is.na(filt$AphiaID) == F)

##### creating errdap_link field #####
filt$references <- paste('https://www.ncei.noaa.gov/erddap/tabledap/deep_sea_corals.csv?ShallowFlag%2CDatasetID%2CCatalogNumber%2CSampleID%2CCitation%2CRepository%2CScientificName%2CVernacularNameCategory%2CTaxonRank%2CIdentificationQualifier%2CLocality%2Clatitude%2Clongitude%2CDepthInMeters%2CDepthMethod%2CObservationDate%2CSurveyID%2CStation%2CEventID%2CSamplingEquipment%2CLocationAccuracy%2CRecordType%2CDataProvider&CatalogNumber=',
                          filt$CatalogNumber, sep = '')

##### fix Depth issues #####
filt$MinimumDepthInMeters <- ifelse(test = is.na(filt$MinimumDepthInMeters) == T, yes = filt$DepthInMeters, no = filt$MinimumDepthInMeters)
filt$MaximumDepthInMeters <- ifelse(test = is.na(filt$MaximumDepthInMeters) == T, yes = filt$DepthInMeters, no = filt$MaximumDepthInMeters)

##### fix species issues #####
filt$ScientificName <- ifelse(test = filt$CatalogNumber == '932164', yes = "Narella", no = filt$ScientificName)
filt$ScientificName <- ifelse(test = filt$CatalogNumber == '932389', yes = "Narella", no = filt$ScientificName)

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

##### recode of DSC RecordType to 'basisOfRecord' #####

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
                 'specimen | video observation' = 'PreservedSpecimen')

# use RecordType to type crosswalk to apply type valid values:
obis$basisOfRecord <- recode(obis$basisOfRecord, !!!recode_list)

##### write out file for submission #####
today <- '20200415-1'
version <- unique(filt$DatabaseVersion)
setwd('C:/rworking/deepseatools/indata')
obis %>%
  write.csv(paste(today,'_export_to_OBIS_DSCRTP_NDB_', version , '_RPMcGuinn', '.csv', sep = ''),
            row.names = FALSE)

##### checking #####
library(dplyr)

x <- filt %>%
  group_by(CatalogNumber) %>%
  filter(n()>1)


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

##### later we will work on emof variables #####

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
