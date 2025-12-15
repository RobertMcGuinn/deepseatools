##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20251213
## purpose: sync with GBIF Smithsonian NMNH Inv. Zoologogy database on
## info: gbif dataset with dataset_id <- "821cc27a-e3bb-4bc5-ac34-89ada245069d"

##### github_link #####
filename <- 'dst_gbif_smithsonian' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
##### packages #####
library(rgbif)
library(tidyverse)
library(worrms)

##### parameters #####
## set taxonomic backbone keys (integer values) for taxa of interest
## all children taxa are included in search
cnidaria_key <- name_backbone(name = "Cnidaria", rank = "phylum")$usageKey
porifera_key <- name_backbone(name = "Porifera", rank = "phylum")$usageKey

## set datasetID
dataset_id <- "821cc27a-e3bb-4bc5-ac34-89ada245069d"

##### search GBIF #####
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
# setdiff(names(porifera$data), names(cnidaria$data))

##### bind rows into single dataframe #####
combined <- bind_rows(
  porifera$data,
  cnidaria$data
)

##### check #####
# table(combined$locality)
# table(combined$scientificName)
# table(combined$relations)
# table(combined$hostingOrganizationKey)
# table(combined$datasetKey)
# table(combined$identifier)
# browseURL(combined$identifier[1])
# table(combined$collectionID)
#
# combined %>%
#   select(contains("Key")) %>% names()
#
# combined %>%
#   select(!contains("Key")) %>% names()
#
# combined %>% pull(depth) %>% table(useNA = 'always')






