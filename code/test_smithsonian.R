##### packages #####
library(rgbif)
library(tidyr)
library(worrms)

##### Get GBIF taxon keys for the phyla #####
cnidaria_key <- name_backbone(name = "Cnidaria", rank = "phylum")$usageKey
porifera_key <- name_backbone(name = "Porifera", rank = "phylum")$usageKey

##### Set dataset of interest #####
dataset_id <- "821cc27a-e3bb-4bc5-ac34-89ada245069d"

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

##### check #####
setdiff(names(porifera$data), names(cnidaria$data))

##### bind rows #####
combined <- bind_rows(
  porifera$data,
  cnidaria$data
)

##### check #####
table(combined$locality)
table(combined$scientificName)
table(combined$references)
table(combined$hostingOrganizationKey)
table(combined$datasetKey)
table(combined$identifier)
browseURL(combined$identifier[1])
