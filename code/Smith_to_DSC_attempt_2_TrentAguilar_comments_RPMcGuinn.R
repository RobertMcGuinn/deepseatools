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
print(github_link)

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
## RPMcGuinn: I'd like to test how occ_download works for production purposes. Does it produce a different set of variables than (occ_search)
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

dl_request <- occ_download(
  pred("datasetKey", dataset_id),
  pred_in("taxonKey", c(cnidaria_key, porifera_key)), # Pulls both phyla in one query
  pred("country", "US"),
  pred_gte("coordinateUncertaintyInMeters", 0),
  pred_lte("coordinateUncertaintyInMeters", 100),
  # pred_gt("year", 1970), # Uncomment if you want to apply your post-1970 filter
  format = "SIMPLE_CSV"
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
## RPMcGuinn: I like the idea proposed for the production code to grab the unique keys.


##### define DwC to DSCRTP crosswalk #####
# A named vector matching DSCRTP columns (names) to DwC columns (values)
dscrtp_crosswalk <- c(
  # Core Occurrence & Taxonomy
  SampleID = "catalogNumber",  # Robert: using catalogNumber for USNM ## RPMcGuinn: this is perfect!
  WebSite = "occurrenceID",    # Robert: mapping ArkID (which lives in occurrenceID) to WebSite
  ScientificName = "scientificName", # Robert: Do nothing here. I can handle.
  AphiaID = "aphiaID",         # Pulled from the custom WoRMS function above
  TrackingID = "recordNumber", # Robert: 'recordNumber' is interesting.

  # NOTE: Taxonomy fields removed per Robert's instructions to leave out of the match

  # Event & Location
  ObservationDate = "eventDate",
  ObservationTime = "eventTime", ## RPMcGuinn: 'eventTime' not available in 'combined'
  ObservationYear = "year",
  Latitude = "decimalLatitude",
  Longitude = "decimalLongitude",
  LocationAccuracy = "coordinateUncertaintyInMeters", ## RPMcGuinn: This one seems to be unavailable but it is very important.
  MinimumDepthInMeters = "minimumDepthInMeters", ## RPMcGuinn: use 'depth' from 'combined' here
  MaximumDepthInMeters = "maximumDepthInMeters", ## RPMcGuinn: use 'depth' from 'combined' here
  StartLatitude = "startLatitude", ## RPMcGuinn: not available in 'combined' (leave out)
  StartLongitude = "startLongitude", ## RPMcGuinn: not available in 'combined' (leave out)
  EndLatitude = "endLatitude", ## RPMcGuinn: not available in 'combined' (leave out)
  EndLongitude = "endLongitude", ## RPMcGuinn: not available in 'combined' (leave out)
  VerbatimLatitude = "verbatimLatitude", ## RPMcGuinn: not available in 'combined' (leave out)
  VerbatimLongitude = "verbatimLongitude", ## RPMcGuinn: not available in 'combined' (leave out)
  Ocean = "waterBody", ## RPMcGuinn: needs further transformation to match DSCRTP schema (see valid values)
  Country = "country",
  Locality = "locality", ## RPMcGuinn: a further transformation (paste higherGeogaphy and locality and leave in "locality", use " | " as a separator)
  NavType = "georeferenceProtocol", ## RPMcGuinn: not available in 'combined' (leave out)

  # Identifications & Status
  TypeStatus = "typeStatus",## RPMcGuinn: not available in 'combined' (leave out)
  IdentificationComments = "identificationRemarks",## RPMcGuinn: not available in 'combined' (leave out)
  IdentifiedBy = "identifiedBy",
  IdentificationDate = "dateIdentified", ## RPMcGuinn: not available in 'combined' (leave out)
  IdentificationQualifier = "identificationQualifier",
  ## RPMcGuinn: "RecordType" will need further transformation to conform to DSCRTP schema."PRESERVED_SPECIMEN" to "specimen"
  RecordType = "basisOfRecord", # Note: using basisOfRecord instead of 'type' for GBIF standard compatibility

  # Survey & Sampling
  SurveyID = "parentEventID", ## RPMcGuinn: not available in 'combined' (leave out)
  EventID = "eventID", ## RPMcGuinn: not available in 'combined' (leave out)
  Vessel = "sailingVessel", ## RPMcGuinn: not available in 'combined' (leave out)
  SurveyComments = "eventRemarks",## RPMcGuinn: not available in 'combined' (leave out)
  SamplingEquipment = "samplingEquipment",## RPMcGuinn: not available in 'combined' (leave out)
  VehicleName = "vehicleName",## RPMcGuinn: not available in 'combined' (leave out)
  SampleAreaInSquareMeters = "sampleSizeValue", ##RPMcGuinn: not available in 'combined' (leave out)
  footprintWKT = "footprintWKT",## RPMcGuinn: not available in 'combined' (leave out)
  footprintSRS = "footprintSRS",## RPMcGuinn: not available in 'combined' (leave out)
  footprintSRS = "geodeticDatum", ## RPMcGuinn: I added this line.

  # Biological Data
  IndividualCount = "individualCount",
  CategoricalAbundance = "categoricalAbundance", ## RPMcGuinn: not available in 'combined' (leave out)
  Density = "organismDensity",## RPMcGuinn: not available in 'combined' (leave out)
  Cover = "cover",## RPMcGuinn: not available in 'combined' (leave out)
  VerbatimSize = "verbatimSize", ## RPMcGuinn: not available in 'combined' (leave out)
  MinimumSize = "minimumSize",## RPMcGuinn: not available in 'combined' (leave out)
  MaximumSize = "maximumSize",## RPMcGuinn: not available in 'combined' (leave out)
  WeightInKg = "weight",## RPMcGuinn: not available in 'combined' (leave out)
  Condition = "condition",## RPMcGuinn: not available in 'combined' (leave out)
  AssociatedTaxa = "associatedTaxa",## RPMcGuinn: not available in 'combined' (leave out)

  # Environment & Habitat
  Habitat = "habitat",## RPMcGuinn: not available in 'combined' (leave out)
  Substrate = "substrate",## RPMcGuinn: not available in 'combined' (leave out)
  CMECSGeoForm = "geoformCMECS",## RPMcGuinn: not available in 'combined' (leave out)
  CMECSSubstrate = "substrateCMECS",## RPMcGuinn: not available in 'combined' (leave out)
  CMECSBiotic = "bioticCMECS",## RPMcGuinn: not available in 'combined' (leave out)
  Temperature = "temperature",## RPMcGuinn: not available in 'combined' (leave out)
  Salinity = "salinity",## RPMcGuinn: not available in 'combined' (leave out)
  Oxygen = "oxygen",## RPMcGuinn: not available in 'combined' (leave out)
  pH = "pH",## RPMcGuinn: not available in 'combined' (leave out)
  pHscale = "pHScale",## RPMcGuinn: not available in 'combined' (leave out)
  pCO2 = "pCO2",## RPMcGuinn: not available in 'combined' (leave out)
  TA = "totalAlkalinity",## RPMcGuinn: not available in 'combined' (leave out)
  DIC = "dissolvedInorganicCarbon",## RPMcGuinn: not available in 'combined' (leave out)

  # Metadata & Media (WebSite removed from here as it is now mapped to occurrenceID above)
  ImageURL = "associatedMedia",## RPMcGuinn: not available in 'combined' (leave out)
  Citation = "associatedReferences",## RPMcGuinn: not available in 'combined' (leave out)
  Repository = "institutionCode",## RPMcGuinn: will require some transformation.
  DataProvider = "ownerInstitutionCode",## RPMcGuinn: not available in 'combined'. (just assign the name "Smithsonian Institution, National Museum of Natural History" for all.
  AssociatedSequences = "associatedSequences",
  OccurrenceComments = "occurrenceRemarks",
  LocationComments = "locationRemarks",
  Modified = "modified", ## RPMcGuinn: will require modification to match DSCRTP (we only have the date, not the time)
  gisMEOW = "higherGeography" ##RPMcGuinn: not a good match for gisMEOW (leave out, we calculate this)
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
