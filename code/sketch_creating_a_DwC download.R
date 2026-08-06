library(rgbif)
library(tidyverse)

##### Parameters #####
dataset_id   <- "821cc27a-e3bb-4bc5-ac34-89ada245069d" # Smithsonian NMNH Inv. Zoology
cnidaria_key <- 44
porifera_key <- 148

##### 1. Instant Synchronous Search (Takes ~2 seconds) #####
message("Fetching 100 Cnidaria and 100 Porifera records for rapid testing...")

cnidaria_test <- occ_search(
  datasetKey = dataset_id,
  taxonKey   = cnidaria_key,
  country    = "US",
  limit      = 100
)$data

porifera_test <- occ_search(
  datasetKey = dataset_id,
  taxonKey   = porifera_key,
  country    = "US",
  limit      = 100
)$data

combined_test <- bind_rows(cnidaria_test, porifera_test)

##### 2. Schema Padder: Guarantee Critical DwC Columns Exist #####
# occ_search() drops columns if all 200 test rows happen to be NA.
# This utility checks your crosswalk's required fields and pads any missing ones with NA.
pad_missing_dwc_columns <- function(df, required_cols) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    message("Padding missing DwC columns with NA: ", paste(missing_cols, collapse = ", "))
    df[missing_cols] <- NA
  }
  return(df)
}

# All Darwin Core values your DSCRTP crosswalk looks for:
required_dwc_fields <- c(
  "catalogNumber", "occurrenceID", "scientificName", "recordNumber",
  "eventDate", "eventTime", "year", "decimalLatitude", "decimalLongitude",
  "coordinateUncertaintyInMeters", "depth", "waterBody", "country",
  "locality", "higherGeography", "identifiedBy", "identificationQualifier",
  "basisOfRecord", "individualCount", "eventID", "parentEventID",
  "samplingProtocol", "eventRemarks", "occurrenceRemarks",
  "institutionCode", "modified", "geodeticDatum", "associatedSequences",
  "locationRemarks", "taxonKey"
)

# Apply the padder
combined_test_padded <- pad_missing_dwc_columns(combined_test, required_dwc_fields)

##### 3. Ready for Downstream Testing #####
cat("\nTest dataframe ready!\n")
cat(" - Rows:", nrow(combined_test_padded), "\n")
cat(" - Columns:", ncol(combined_test_padded), "\n")
