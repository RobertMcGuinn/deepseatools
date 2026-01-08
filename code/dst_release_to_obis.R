##### Header #####
# author: Robert McGuinn | robert.mcguinn@noaa.gov | rpm@alumni.duke.edu
# date started: 20241023
# purpose: create a Darwin Core compliant Minimum Viable Product (MVP) for export from the
#  NOAA National Database for Deep Sea Corals and Sponge ingest to OBIS / GBIF

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

##### linkage #####
filename <- 'dst_release_to_obis' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)

##### load the current version of the National Database from local file #####
## creates object called 'filt'
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
table(obis$basisOfRecord, useNA = 'always')
obis %>% filter(is.na(basisOfRecord) == T) %>% View()
filt %>% filter(CatalogNumber == 1606484) %>% pull(AccessionID)

##### OPTIONAL: fix where basisOfRecord is null #####
obis <- obis %>%
  mutate(basisOfRecord = case_when(
    is.na(basisOfRecord) ~ "MachineObservation",
    TRUE ~ basisOfRecord
  ))

##### write out file for submission (manual) #####
today <- '20251001-0'
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
  'c:/rworking/deepseatools/indata/dwc_noaa_dsc_rtp_version_20251001-0_20251001-0.txt',
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE)

##### check #####
# data %>%  filter(occurrenceID == "NOAA_DSCRTP:1557739") %>% pull(associatedMedia)
# obis %>%  filter(occurrenceID == "NOAA_DSCRTP:1557739") %>% pull(scientificName)


set <- as.numeric(gsub(".*:", "", data$occurrenceID))
`%notin%` <- Negate(`%in%`)
x <- filt %>% filter(CatalogNumber %notin% set) %>% pull(CatalogNumber)
filt %>%  filter(CatalogNumber %in% x) %>%
  pull(SampleID)

filt %>% filter(SampleID == 'USNM 75656') %>% pull(Vessel)

##### NOTES: DarwinCore crosswalk for other fields we might use later on #####
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

##### OPTIONAL: EMOF "extended measurement or fact" variables #####

## create a list of emof fields and occurenceID
emof_fields <- emof %>% select(measurementType) %>% pull()
emof_fields_o <- append(emof_fields[[1]], "occurrenceID", after = 0)

## select from emof dataframe Type, Unit and Method
emof_tum <- select(emof, measurementType, measurementUnit, measurementMethod)

## pull emof_fields from the main DSC dataset
## stretch columns into a tall dataframe using gather
## remove rows with NA values
## join tall dataframe with emof type unit method _tum
## add uuid for each record

DB_emof_subset <- DB_subset2  %>%
  select(emof_fields_o) %>%
  gather(key = "measurementType", value = "measurementValue", -occurrenceID) %>%
  drop_na() %>%
  inner_join(emof_tum, by = "measurementType") %>%
  mutate(measurementID=uuid())

## remove emof fields from DSC Dataset
DSCRTP_Occurrences <- DB_subset2 %>% select(-one_of(emof_fields))
write_csv(DB_emof_subset, paste("DSCRTP_EMOF_Subset_", Sys.Date(), ".csv"))
write_csv(DSCRTP_Occurrences, paste("DSCRTP_Occurrences_", Sys.Date(), ".csv"))
print("Script Finished")
