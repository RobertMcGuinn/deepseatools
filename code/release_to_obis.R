### NOTE: Downloading from ERDDAP is very slow, it may take 5-10 minutes for the script to run. 
### If you want to speed up the process you can download the data first and point your DB_path toward the file.
library(readr)
library(tidyr)
library(dplyr)
library(googlesheets)
#install.packages('ids')
library(ids)

#set working directory
setwd('C:/rworking/digs/outdata/obis/')

#path to the DSC Data
#DB_path <- "C:/Users/matt.dornback/R_Working_Directory/DeepSeaCorals/DSCRTP_NatDB_20181005-0/DSCRTP_NatDB_20181005-0.csv"
DB_path <- "https://ecowatch.ncddc.noaa.gov/erddap/tabledap/deep_sea_corals.csv?CatalogNumber%2CVernacularNameCategory%2CVernacularName%2CScientificName%2CVerbatimScientificName%2CTaxonRank%2CAphiaID%2CPhylum%2CClass%2CSubclass%2COrder%2CSuborder%2CFamily%2CSubfamily%2CGenus%2CSubgenus%2CSpecies%2CSubspecies%2CScientificNameAuthorship%2CTypeStatus%2CSynonyms%2CIdentificationComments%2CIdentifiedBy%2CIdentificationDate%2CIdentificationQualifier%2CAssociatedSequences%2CIndividualCount%2CCategoricalAbundance%2CDensity%2CCover%2CVerbatimSize%2CMinimumSize%2CMaximumSize%2CWeightInKg%2CCondition%2CAssociatedTaxa%2COccurenceComments%2CSubstrate%2CHabitat%2CCMECSGeoForm%2CCMECSSubstrate%2CCMECSBiotic%2CTemperature%2CSalinity%2CpH%2CpHscale%2CpCO2%2CTA%2CDIC%2COcean%2CLargeMarineEcosystem%2CCountry%2CFishCouncilRegion%2CLocality%2CStation%2CObservationDate%2CObservationYear%2CObservationTime%2Clatitude%2Clongitude%2CDepthInMeters%2CDepthMethod%2CShallowFlag%2CgisEtopoDepth%2CgisGEBCODepth%2CgisCRMDepth%2CStartLatitude%2CStartLongitude%2CEndLatitude%2CEndLongitude%2CMinimumDepthInMeters%2CMaximumDepthInMeters%2CVerbatimLatitude%2CVerbatimLongitude%2CLocationAccuracy%2CNavType%2CLocationComments%2CSurveyID%2CVessel%2CPI%2CPIAffiliation%2CPurpose%2CSurveyComments%2CEventID%2CSamplingEquipment%2CVehicleName%2CSampleAreaInSquareMeters%2CfootprintWKT%2CfootprintSRS%2COtherData%2CEntryDate%2CReporter%2CReporterEmail%2CReporterComments%2CDataProvider%2CDataContact%2CModified%2CRecordType%2CRepository%2CCitation%2CSampleID%2CTrackingID%2CDatasetID%2CWebsite%2CImageURL%2CSynonymSearchProxy"
# read DSC Database 
### Please be patient this will take a few minutes ###
DSC_DB <- read_csv(DB_path, na = c("NA", -999))


# read DSCRTP Schema from Google Drive
# specific key for DSC Schema google drive sheet
my_sheet_key <- "1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI"
# Register the sheet in googlesheets
my_sheet <- my_sheet_key %>% gs_key(visibility = "private")
# Read in google sheet as a dataframe
Schema <- my_sheet %>% gs_read("fields")
emof <- my_sheet %>% gs_read("emof crosswalk", range = cell_limits(c(3, 1), c(NA, NA)))


# create a list of occurrence fields: filter on ReleaseToOBIS, create a list of FieldName column
Occ_fields <- Schema %>% filter(ReleaseToOBIS==1) %>% select(FieldName) %>% pull()


# in the main database remove flagged records and pick the columns to be released to OBIS
# also included column renaming to OBIS values, tried passing a named vector and couldn't get to work, just pasted all the names in the fuction...
DB_subset <- DSC_DB %>% 
  #filter(Flag==0) %>% 
  select(one_of(Occ_fields)) %>% rename(datasetID = "DatasetID", occurrenceID = "CatalogNumber", catalogNumber = "SampleID", recordNumber = "TrackingID", associatedMedia = "ImageURL", associatedReferences = "Citation", institutionCode = "Repository", scientificName = "ScientificName", vernacularName = "VernacularNameCategory", taxonRank = "TaxonRank", scientificNameID = "AphiaID", phylum = "Phylum", class = "Class", order = "Order", family = "Family", genus = "Genus", subgenus = "Subgenus", species = "Species", specificEpithet = "Subspecies", scientificNameAuthorship = "ScientificNameAuthorship", typeStatus = "TypeStatus", identificationRemarks = "IdentificationComments", identifiedBy = "IdentifiedBy", dateIdentified = "IdentificationDate", identificationQualifier = "IdentificationQualifier", associatedSequences = "AssociatedSequences", waterBody = "Ocean", country = "Country", locality = "Locality", decimalLatitude = "Latitude", decimalLongitude = "Longitude", minimumDepthInMeters = "MinimumDepthInMeters", maximumDepthInMeters = "MaximumDepthInMeters", locationRemarks = "LocationComments", eventDate = "ObservationDate", year = "ObservationYear", eventTime = "ObservationTime", parentEventID = "SurveyID", sailingVessel = "Vessel", eventRemarks = "SurveyComments", eventID = "EventID", samplingEquipment = "SamplingEquipment", vehicleName = "VehicleName", sampleSizeValue = "SampleAreaInSquareMeters", footprintWKT = "footprintWKT", footprintSRS = "footprintSRS", individualCount = "IndividualCount", categoricalAbundance = "CategoricalAbundance", organismDensity = "Density", cover = "Cover", verbatimSize = "VerbatimSize", minimumSize = "MinimumSize", maximumSize = "MaximumSize", weight = "WeightInKg", condition = "Condition", associatedTaxa = "AssociatedTaxa", occurrenceRemarks = "OccurrenceComments", startLatitude = "StartLatitude", startLongitude = "StartLongitude", endLatitude = "EndLatitude", endLongitude = "EndLongitude", verbatimLatitude = "VerbatimLatitude", verbatimLongitude = "VerbatimLongitude", coordinateUncertaintyInMeters = "LocationAccuracy", georeferenceProtocol = "NavType", habitat = "Habitat", substrate = "Substrate", geoformCMECS = "CMECSGeoForm", substrateCMECS = "CMECSSubstrate", bioticCMECS = "CMECSBiotic", temperature = "Temperature", salinity = "Salinity", oxygen = "Oxygen", pH = "pH", pHScale = "pHscale", pCO2 = "pCO2", totalAlkalinity = 'TA', dissolvedInorganicCarbon = "DIC", type = "RecordType", ownerInstitutionCode = "DataProvider", modified = "Modified", references = "WebSite", higherGeography = "gisMEOW")


# add sampleSizeUnit column and populate all non-NA sampleSizeValue rows with "Square Meters"
DB_subset$sampleSizeUnit <- ifelse(is.na(DB_subset$sampleSizeValue) == FALSE, "Square Meters", NA)


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
