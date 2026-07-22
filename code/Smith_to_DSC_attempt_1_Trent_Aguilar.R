##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
          #& Trenton Aguilar, trenton.aguilar@noaa.gov
## purpose: Download GBIF Smithsonian NMNH Inv. Zoology data and convert from DwC to DSCRTP format
## info: gbif dataset with dataset_id <- "821cc27a-e3bb-4bc5-ac34-89ada245069d"

##### packages #####
library(rgbif)
library(tidyverse)
library(worrms)

##### parameters #####
## set taxonomic backbone keys (integer values) for taxa of interest
cnidaria_key <- name_backbone(name = "Cnidaria", rank = "phylum")$usageKey
porifera_key <- name_backbone(name = "Porifera", rank = "phylum")$usageKey

## set datasetID (NMNH)
dataset_id <- "821cc27a-e3bb-4bc5-ac34-89ada245069d"

##### search GBIF #####
# Pulling 100 limit for testing; increase limit or use occ_download() for production
cnidaria <- occ_search(
  datasetKey = dataset_id,
  taxonKey = cnidaria_key,
  country = "US",
  limit = 100
)

porifera <- occ_search(
  datasetKey = dataset_id,
  taxonKey = porifera_key,
  country = "US",
  limit = 100
)

##### bind rows into single dataframe #####
combined <- bind_rows(
  porifera$data,
  cnidaria$data
)

##### define DwC to DSCRTP crosswalk #####
# A named vector matching DSCRTP columns (names) to DwC columns (values) 
# Derived from the obis mapping and NOTE section in dst_tool_release_to_obis.R
dscrtp_crosswalk <- c(
  # Core Occurrence & Taxonomy
  CatalogNumber = "occurrenceID",
  ScientificName = "scientificName",
  AphiaID = "scientificNameID",
  TrackingID = "recordNumber",
  VernacularNameCategory = "vernacularName",
  TaxonRank = "taxonRank",
  Phylum = "phylum",
  Class = "class",
  Order = "order",
  Family = "family",
  Genus = "genus",
  Subgenus = "subgenus",
  Species = "species",
  Subspecies = "specificEpithet",
  ScientificNameAuthorship = "scientificNameAuthorship",
  
  # Event & Location
  ObservationDate = "eventDate",
  ObservationTime = "eventTime",
  ObservationYear = "year",
  Latitude = "decimalLatitude",
  Longitude = "decimalLongitude",
  LocationAccuracy = "coordinateUncertaintyInMeters",
  MinimumDepthInMeters = "minimumDepthInMeters",
  MaximumDepthInMeters = "maximumDepthInMeters",
  StartLatitude = "startLatitude",
  StartLongitude = "startLongitude",
  EndLatitude = "endLatitude",
  EndLongitude = "endLongitude",
  VerbatimLatitude = "verbatimLatitude",
  VerbatimLongitude = "verbatimLongitude",
  Ocean = "waterBody",
  Country = "country",
  Locality = "locality",
  NavType = "georeferenceProtocol",
  
  # Identifications & Status
  TypeStatus = "typeStatus",
  IdentificationComments = "identificationRemarks",
  IdentifiedBy = "identifiedBy",
  IdentificationDate = "dateIdentified",
  IdentificationQualifier = "identificationQualifier",
  RecordType = "basisOfRecord", # Note: using basisOfRecord instead of 'type' for GBIF standard compatibility
  
  # Survey & Sampling
  SurveyID = "parentEventID",
  EventID = "eventID",
  Vessel = "sailingVessel",
  SurveyComments = "eventRemarks",
  SamplingEquipment = "samplingEquipment",
  VehicleName = "vehicleName",
  SampleAreaInSquareMeters = "sampleSizeValue",
  footprintWKT = "footprintWKT",
  footprintSRS = "footprintSRS",
  
  # Biological Data
  IndividualCount = "individualCount",
  CategoricalAbundance = "categoricalAbundance",
  Density = "organismDensity",
  Cover = "cover",
  VerbatimSize = "verbatimSize",
  MinimumSize = "minimumSize",
  MaximumSize = "maximumSize",
  WeightInKg = "weight",
  Condition = "condition",
  AssociatedTaxa = "associatedTaxa",
  
  # Environment & Habitat
  Habitat = "habitat",
  Substrate = "substrate",
  CMECSGeoForm = "geoformCMECS",
  CMECSSubstrate = "substrateCMECS",
  CMECSBiotic = "bioticCMECS",
  Temperature = "temperature",
  Salinity = "salinity",
  Oxygen = "oxygen",
  pH = "pH",
  pHscale = "pHScale",
  pCO2 = "pCO2",
  TA = "totalAlkalinity",
  DIC = "dissolvedInorganicCarbon",
  
  # Metadata & Media
  ImageURL = "associatedMedia",
  Citation = "associatedReferences",
  Repository = "institutionCode",
  DataProvider = "ownerInstitutionCode",
  AssociatedSequences = "associatedSequences",
  OccurrenceComments = "occurrenceRemarks",
  LocationComments = "locationRemarks",
  Modified = "modified",
  WebSite = "references",
  gisMEOW = "higherGeography"
)

##### convert DarwinCore to DSCRTP format #####
# Use any_of() to safely rename and select only the columns that actually exist in the GBIF payload
dscrtp_formatted <- combined %>%
  select(any_of(dscrtp_crosswalk))

# Check the resulting fields
print(names(dscrtp_formatted))

##### Optional: Clean up specific formatting #####
# E.g., Strip the NOAA_DSCRTP: prefix if records originally came from your database
# dscrtp_formatted <- dscrtp_formatted %>%
#   mutate(CatalogNumber = gsub("NOAA_DSCRTP:", "", CatalogNumber))

##### export the file #####
output_filename <- paste0("Smithsonian_DSCRTP_Converted_", Sys.Date(), ".csv")
write_csv(dscrtp_formatted, output_filename)
message(paste("Successfully exported to", output_filename))