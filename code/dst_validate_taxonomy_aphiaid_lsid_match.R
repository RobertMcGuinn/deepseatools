##### load national database #####
source('C:/rworking/deepseatools/code/dst_tool_load_current_ndb.R')

##### find the mismatches between AphiaID and LifeScience Identifier #####
# 1. Re-calculate the verification string
filt$lsid_check <- paste0("urn:lsid:marinespecies.org:taxname:", filt$AphiaID)

# 2. Use which() to subset correctly
mismatched_taxa <- filt[which(filt$LifeScienceIdentifier != filt$lsid_check),
                        c("ScientificName", "AphiaID", "LifeScienceIdentifier", "lsid_check")]

# 3. Check results again
nrow(mismatched_taxa)

##### validate_taxonomy #####
## Purpose: Ensure AphiaID (Field 18) matches LifeScienceIdentifier (Field 19)

validate_taxonomy <- function(data) {
  # 1. Define the required WoRMS LSID prefix
  lsid_prefix <- "urn:lsid:marinespecies.org:taxname:"

  # 2. Identify rows where both fields are present but misaligned
  # We use which() to handle NAs correctly as discussed
  tax_mismatches <- which(
    !is.na(data$AphiaID) &
      !is.na(data$LifeScienceIdentifier) &
      data$LifeScienceIdentifier != paste0(lsid_prefix, data$AphiaID)
  )

  # 3. Handle failures
  if (length(tax_mismatches) > 0) {
    bad_data <- data[tax_mismatches, c("ScientificName", "AphiaID", "LifeScienceIdentifier")]

    # Log the specific error for your audit trail
    message("CRITICAL ERROR: Taxonomic misalignment found in ", length(tax_mismatches), " records.")
    print(bad_data)

    # Stop execution to prevent bad data from reaching the National Database
    stop("Aborting patch: Field 18 and Field 19 must be aligned before proceeding.")
  } else {
    message("SUCCESS: All AphiaIDs and LSIDs are properly aligned.")
  }
}

# --- Implementation ---
# Call this function immediately after loading your data zip or patch file
# validate_taxonomy(filt)
