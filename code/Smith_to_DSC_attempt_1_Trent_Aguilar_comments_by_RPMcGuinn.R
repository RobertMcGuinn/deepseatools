##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
          #& Trenton Aguilar, trenton.aguilar@noaa.gov
## purpose: Download GBIF Smithsonian NMNH Inv. Zoology data and convert from DwC to DSCRTP format
## info: gbif dataset with dataset_id <- "821cc27a-e3bb-4bc5-ac34-89ada245069d"

##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(current_file)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
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
  CatalogNumber = "occurrenceID", ## Robert: we will need to record 'occurrenceID' in SampleID (but also we are looking for USNM #)
  ScientificName = "scientificName", ## Robert: The combined$scientificName is definitely the best match, but it also includes ScientificNameAuthorship.  Do nothing here.  I can handle.
  AphiaID = "scientificNameID", ## Robert: I could not find 'scientificNameID' in 'combined'.  It would be really good to get this if possible.
  TrackingID = "recordNumber", ## Robert: 'recordNumber' is interesting.  It seems to have notation like the DSCRTP "SurveyID" and "EventID".

  ## Robert: The list below (down to 'scientificNameAuthorship') should be left out of the match.
  ## We would like to autopopulate these using the existing update process.
  VernacularNameCategory = "vernacularName", ## Robert: Could not find 'vernacularName' in 'combined'. This should be matched "VernacularName"
  TaxonRank = "taxonRank",
  Phylum = "phylum",
  Class = "class",
  Order = "order",
  Family = "family",
  Genus = "genus",
  Subgenus = "subgenus",
  Species = "species", ## Robert: this should be matched with "specificEpthet"
  Subspecies = "specificEpithet", ## Robert: this is not the correct match. combined:'specificEpithet' matches with DSCRTP:'Species'
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

install.packages("rgbif")
install.packages("worrms")

## R Function to Map Keys
## Here is a clean, reusable function that takes a GBIF acceptedTaxonKey and returns the corresponding valid AphiaID:
library(rgbif)
library(worrms)

gbif_to_aphia <- function(gbif_key) {

  # Step 1: Get the scientific name from GBIF using the taxon key
  gbif_record <- rgbif::name_usage(key = gbif_key)

  # Extract canonical name (or scientific name if canonical is absent)
  sci_name <- gbif_record$data$canonicalName
  if (is.null(sci_name)) {
    sci_name <- gbif_record$data$scientificName
  }

  if (is.null(sci_name)) {
    warning("Could not find a valid scientific name in GBIF for key: ", gbif_key)
    return(NA)
  }

  # Step 2: Query WoRMS for the scientific name
  worrms_record <- tryCatch({
    worrms::wm_records_name(name = sci_name)
  }, error = function(e) {
    return(NULL)
  })

  # Step 3: Extract and return the accepted/valid AphiaID
  if (!is.null(worrms_record) && nrow(worrms_record) > 0) {
    # 'valid_AphiaID' ensures you get the accepted ID even if the name queried was a synonym
    return(worrms_record$valid_AphiaID[1])
  } else {
    message("No WoRMS entry found for: '", sci_name, "' (Taxon may be strictly terrestrial or unlisted in WoRMS).")
    return(NA)
  }
}

## Usage Example
## Testing with GBIF Key 2435098 (Gadus morhua / Atlantic Cod):
aphia_id <- gbif_to_aphia(2435098)
print(aphia_id)

# Output: [1] 126437
## Vectorized Usage for Multiple Keys
## If you have a vector or dataset of GBIF keys, you can map them in bulk using sapply() or purrr::map_int():

gbif_keys <- c(2435098, 2481914, 5219223)

aphia_ids <- sapply(gbif_keys, gbif_to_aphia)
print(aphia_ids)
