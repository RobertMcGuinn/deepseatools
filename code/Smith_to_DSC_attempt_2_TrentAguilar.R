##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## & Trenton Aguilar, trenton.aguilar@noaa.gov
## purpose: Download GBIF Smithsonian NMNH Inv. Zoology data and convert from DwC to DSCRTP format
## info: gbif dataset with dataset_id <- "821cc27a-e3bb-4bc5-ac34-89ada245069d"

##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(current_file)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')

##### packages #####
# Ensure packages are installed if not already:
# install.packages(c("rgbif", "tidyverse", "worrms"))
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

##### mapping to AphiaID #####
# Function updated to properly extract the name from the gbif_record data object
gbif_to_aphia <- function(gbif_key) {
  
  # Step 1: Get the scientific name from GBIF using the taxon key
  gbif_record <- rgbif::name_usage(key = gbif_key)
  
  # Extract canonical name (or scientific name if canonical is absent) from the gbif API response
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

# Apply the function to the GBIF keys to generate the new AphiaID column
# Note: For large datasets, consider grabbing unique() keys first to limit API calls!
combined$aphiaID <- sapply(combined$taxonKey, gbif_to_aphia)


##### define DwC to DSCRTP crosswalk #####
# A named vector matching DSCRTP columns (names) to DwC columns (values)
dscrtp_crosswalk <- c(
  # Core Occurrence & Taxonomy
  SampleID = "catalogNumber",  # Robert: using catalogNumber for USNM #
  WebSite = "occurrenceID",    # Robert: mapping ArkID (which lives in occurrenceID) to WebSite
  ScientificName = "scientificName", # Robert: Do nothing here. I can handle.
  AphiaID = "aphiaID",         # Pulled from the custom WoRMS function above
  TrackingID = "recordNumber", # Robert: 'recordNumber' is interesting.
  
  # NOTE: Taxonomy fields removed per Robert's instructions to leave out of the match
  
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
  
  # Metadata & Media (WebSite removed from here as it is now mapped to occurrenceID above)
  ImageURL = "associatedMedia",
  Citation = "associatedReferences",
  Repository = "institutionCode",
  DataProvider = "ownerInstitutionCode",
  AssociatedSequences = "associatedSequences",
  OccurrenceComments = "occurrenceRemarks",
  LocationComments = "locationRemarks",
  Modified = "modified",
  gisMEOW = "higherGeography"
)

##### convert DarwinCore to DSCRTP format #####
# Use any_of() to safely rename and select only the columns that actually exist in the GBIF payload
dscrtp_formatted <- combined %>%
  select(any_of(dscrtp_crosswalk))

# Check the resulting fields
print(names(dscrtp_formatted))

##### export the file #####
output_filename <- paste0("Smithsonian_DSCRTP_Converted_", Sys.Date(), ".csv")
write_csv(dscrtp_formatted, output_filename)
message(paste("Successfully exported to", output_filename))
