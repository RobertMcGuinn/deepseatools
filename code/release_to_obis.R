##### Header #####
# original author: Matt Dornback
# date started: unknown
# forked by: Robert McGuinn on 20190121
# date started: 20190121
# purpose: create an export from the NOAA National Database for Deep Sea Corals

##### load NDB #####
setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20191217-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

##### create a minimum list of fields to release to OBIS #####
obis_fields <- c('ScientificName',
                 'AphiaID',
                 'CatalogNumber',
                 'ObservationDate',
                 'Latitude',
                 'Longitude',
                 'LocationAccuracy',
                 'MinimumDepthInMeters',
                 'MaximumDepthInMeters',
                 'RecordType'
                 )

##### filter for fields being passed to OBIS #####
obis <- filt %>%
  dplyr::select(one_of(obis_fields)) %>%
  rename(
    scientificName = 'ScientificName', # verbatim
    scientificNameID = 'AphiaID', # requires modification
    occurrenceID = 'CatalogNumber', # requires modification
    eventDate = 'ObservationDate', # verbatim
    decimalLatitude = "Latitude", # verbatim
    decimalLongitude = "Longitude", # verbatim
    coordinateUncertaintyInMeters = "LocationAccuracy", #verbatim
    minimumDepthInMeters = "MinimumDepthInMeters", # verbatim
    maximumDepthInMeters = "MaximumDepthInMeters", # verbatim
    basisOfRecord = 'RecordType' # requires modification
    )

obis$occurrenceStatus <- 'Present'

##### make the required modifications tranformations #####
# append info onto CatalogNumber to create the OBIS occurrenceID
# append info onto AphiaID to create scientificNameID urn:lsid:marinespecies.org:taxname:125294

obis <- obis %>%
  mutate(occurrenceID = paste("NOAA_DSCRTP_Database:", occurrenceID, sep = "")) %>%
  mutate(scientificNameID = paste("urn:lsid:marinespecies.org:taxname:", scientificNameID, sep = ""))

##### crosswalk of DSC RecordType #####

recoders <- list('specimen' = 'PreservedSpecimen',
                 'literature' = 'HumanObservation',
                 'still image' = 'MachineObservation',
                 'video observation' = 'MachineObservation',
                 'video transect' = 'MachineObservation',
                 'notation' = 'HumanObservation',
                 'catch record' = 'HumanObservation',
                 'specimen/lot' = 'PreservedSpecimen',
                 'still image transect' = 'MachineObservation',
                 'video and still images' = 'MachineObservation',
                 'literature | specimen' = 'PreservedSpecimen')
# use RecordType to type crosswalk to apply type valid values:
obis$basisOfRecord <- recode(obis$basisOfRecord, !!!recoders)


##### other fields we might use #####
datasetID = "DatasetID",
catalogNumber = "SampleID",
recordNumber = "TrackingID",
associatedMedia = "ImageURL",
associatedReferences = "Citation",
institutionCode = "Repository",
vernacularName = "VernacularNameCategory",
taxonRank = "TaxonRank",
phylum = "Phylum",
class = "Class",
order = "Order",
family = "Family",
genus = "Genus",
subgenus = "Subgenus",
species = "Species",
specificEpithet = "Subspecies",
scientificNameAuthorship = "ScientificNameAuthorship",
typeStatus = "TypeStatus",
identificationRemarks = "IdentificationComments",
identifiedBy = "IdentifiedBy",
dateIdentified = "IdentificationDate",
identificationQualifier = "IdentificationQualifier",
associatedSequences = "AssociatedSequences",
waterBody = "Ocean",
country = "Country",
locality = "Locality",
locationRemarks = "LocationComments",
year = "ObservationYear",
eventTime = "ObservationTime",
parentEventID = "SurveyID",
sailingVessel = "Vessel",
eventRemarks = "SurveyComments",
eventID = "EventID",
samplingEquipment = "SamplingEquipment",
vehicleName = "VehicleName",
sampleSizeValue = "SampleAreaInSquareMeters",
footprintWKT = "footprintWKT",
footprintSRS = "footprintSRS",
individualCount = "IndividualCount",
categoricalAbundance = "CategoricalAbundance",
organismDensity = "Density",
cover = "Cover",
verbatimSize = "VerbatimSize",
minimumSize = "MinimumSize",
maximumSize = "MaximumSize",
weight = "WeightInKg",
condition = "Condition",
associatedTaxa = "AssociatedTaxa",
occurrenceRemarks = "OccurrenceComments",
startLatitude = "StartLatitude",
startLongitude = "StartLongitude",
endLatitude = "EndLatitude",
endLongitude = "EndLongitude",
verbatimLatitude = "VerbatimLatitude",
verbatimLongitude = "VerbatimLongitude",
coordinateUncertaintyInMeters = "LocationAccuracy",
georeferenceProtocol = "NavType",
habitat = "Habitat",
substrate = "Substrate",
geoformCMECS = "CMECSGeoForm",
substrateCMECS = "CMECSSubstrate",
bioticCMECS = "CMECSBiotic",
temperature = "Temperature",
salinity = "Salinity",
oxygen = "Oxygen",
pH = "pH",
pHScale = "pHscale",
pCO2 = "pCO2",
totalAlkalinity = 'TA',
dissolvedInorganicCarbon = "DIC",
type = "RecordType",
ownerInstitutionCode = "DataProvider",
modified = "Modified",
references = "WebSite",
higherGeography = "gisMEOW")

# add sampleSizeUnit column and populate all non-NA sampleSizeValue rows with "Square Meters"
obis$sampleSizeUnit <- ifelse(is.na(obis$sampleSizeValue) == FALSE, "Square Meters", NA)

# append info onto CatalogNumber to create the OBIS occurrenceID
# append info onto AphiaID to create scientificNameID urn:lsid:marinespecies.org:taxname:125294
DB_subset2 <- DB_subset %>%
  mutate(occurrenceID = paste("NOAA_DSCRTP_Database:", occurrenceID, sep = "")) %>%
  mutate(scientificNameID = paste("urn:lsid:marinespecies.org:taxname:", scientificNameID, sep = ""))


# crosswalk of DSC RecordType to OBIS type
RecordType_type <- list(specimen="PreservedSpecimen", 'literature'="HumanObservation", 'still image'="MachineObservation", 'video observation'="MachineObservation", 'video transect'="MachineObservation", 'notation'="HumanObservation", 'catch record'="HumanObservation")
# use RecordType to type crosswalk to apply type valid values:
DB_subset2$type <- recode(DB_subset2$type, !!!RecordType_type)


# create a list of emof fields and occurenceID
emof_fields <- emof %>% select(measurementType) %>% pull()
emof_fields_o <- append(emof_fields[[1]], "occurrenceID", after = 0)

# select from emof dataframe Type, Unit and Method
emof_tum <- select(emof, measurementType, measurementUnit, measurementMethod)

# pull emof_fields from the main DSC dataset
# stretch columns into a tall dataframe using gather
# remove rows with NA values
# join tall dataframe with emof type, unit method
# add uuid for each record
DB_emof_subset <- DB_subset2  %>%
  select(emof_fields_o) %>%
  gather(key = "measurementType", value = "measurementValue", -occurrenceID) %>%
  drop_na() %>%
  inner_join(emof_tum, by = "measurementType") %>%
  mutate(measurementID=uuid())


# remove emof fields from DSC Dataset
DSCRTP_Occurrences <- DB_subset2 %>% select(-one_of(emof_fields))

write_csv(DB_emof_subset, paste("DSCRTP_EMOF_Subset_", Sys.Date(), ".csv"))

write_csv(DSCRTP_Occurrences, paste("DSCRTP_Occurrences_", Sys.Date(), ".csv"))

print("Script Finished")
