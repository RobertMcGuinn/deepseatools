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
library(rgbif)
library(tidyverse)
library(worrms)

##### parameters #####
## set taxonomic backbone keys (integer values) for taxa of interest
cnidaria_key <- name_backbone(name = "Cnidaria", rank = "phylum")$usageKey
porifera_key <- name_backbone(name = "Porifera", rank = "phylum")$usageKey

## set datasetID (NMNH)
dataset_id <- "821cc27a-e3bb-4bc5-ac34-89ada245069d"

##### search GBIF (occ_download for production) #####
## RPMcGuinn: Testing how occ_download works for production purposes.
message("Initiating GBIF download request...")
dl_request <- occ_download(
  pred("datasetKey", dataset_id),
  pred_in("taxonKey", c(cnidaria_key, porifera_key)), # Pulls both phyla in one query
  pred("country", "US"),
  pred_gte("coordinateUncertaintyInMeters", 0),
  pred_lte("coordinateUncertaintyInMeters", 100),
  format = "SIMPLE_CSV"
)

# Wait for download to finish, then fetch and import data
occ_download_wait(dl_request)
d_meta <- occ_download_get(dl_request)
combined <- occ_download_import(d_meta)

##### mapping to AphiaID #####
gbif_to_aphia <- function(gbif_key) {
  gbif_record <- rgbif::name_usage(key = gbif_key)
  sci_name <- gbif_record$data$canonicalName
  
  if (is.null(sci_name)) {
    sci_name <- gbif_record$data$scientificName
  }
  if (is.null(sci_name)) {
    return(NA)
  }
  
  worrms_record <- tryCatch({
    worrms::wm_records_name(name = sci_name)
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(worrms_record) && nrow(worrms_record) > 0) {
    return(worrms_record$valid_AphiaID[1])
  } else {
    return(NA)
  }
}

## RPMcGuinn: I like the idea proposed for the production code to grab the unique keys.
# Extract unique keys, run the function, and map them back to save API calls
unique_keys <- unique(combined$taxonKey)
aphia_map <- tibble(
  taxonKey = unique_keys,
  aphiaID = sapply(unique_keys, gbif_to_aphia)
)
combined <- combined %>% left_join(aphia_map, by = "taxonKey")

##### define DwC to DSCRTP crosswalk #####
# Filtered to remove fields not available in the combined dataset based on RPMcGuinn's review
dscrtp_crosswalk <- c(
  SampleID = "catalogNumber",  # RPMcGuinn: this is perfect!
  WebSite = "occurrenceID", 
  ScientificName = "scientificName", 
  AphiaID = "aphiaID",         
  TrackingID = "recordNumber", 
  ObservationDate = "eventDate",
  ObservationYear = "year",
  Latitude = "decimalLatitude",
  Longitude = "decimalLongitude",
  LocationAccuracy = "coordinateUncertaintyInMeters", 
  MinimumDepthInMeters = "depth", # RPMcGuinn: use 'depth' from 'combined' here
  MaximumDepthInMeters = "depth", # RPMcGuinn: use 'depth' from 'combined' here
  Ocean = "waterBody", # Needs further transformation
  Country = "country",
  footprintSRS = "geodeticDatum", # RPMcGuinn: I added this line.
  IndividualCount = "individualCount",
  AssociatedSequences = "associatedSequences",
  OccurrenceComments = "occurrenceRemarks",
  LocationComments = "locationRemarks",
  Repository = "institutionCode"
)

##### convert DarwinCore to DSCRTP format #####
dscrtp_formatted <- combined %>%
  # Rename columns safely
  select(any_of(dscrtp_crosswalk), locality, higherGeography, basisOfRecord, modified, identifiedBy, identificationQualifier) %>%
  
  # RPMcGuinn Data Transformations
  mutate(
    # Locality: paste higherGeogaphy and locality, use " | " as a separator
    Locality = case_when(
      !is.na(higherGeography) & !is.na(locality) ~ paste(higherGeography, locality, sep = " | "),
      !is.na(locality) ~ locality,
      TRUE ~ NA_character_
    ),
    
    # RecordType: "PRESERVED_SPECIMEN" to "specimen"
    RecordType = case_when(
      basisOfRecord == "PRESERVED_SPECIMEN" ~ "specimen",
      TRUE ~ basisOfRecord # Keep other values as is, or adjust as needed
    ),
    
    # DataProvider: just assign the name "Smithsonian Institution..." for all
    DataProvider = "Smithsonian Institution, National Museum of Natural History",
    
    # Modified: we only have the date, not the time
    Modified = substr(as.character(modified), 1, 10), # Assumes ISO format YYYY-MM-DDTHH:MM...
    
    # Identifications
    IdentifiedBy = identifiedBy,
    IdentificationQualifier = identificationQualifier
  ) %>%
  # Clean up temporary DwC columns used for mutations
  select(-locality, -higherGeography, -basisOfRecord, -modified, -identifiedBy, -identificationQualifier)

# Check the resulting fields
print(names(dscrtp_formatted))

##### export the file #####
output_filename <- paste0("Smithsonian_DSCRTP_Converted_", Sys.Date(), ".csv")
write_csv(dscrtp_formatted, output_filename)
message(paste("Successfully exported to", output_filename))

