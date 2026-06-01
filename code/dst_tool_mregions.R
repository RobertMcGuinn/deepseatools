##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: YYYYMMDD
## purpose:

##### parameters #####
##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(file_name)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)
documentation_link <- 'https://github.com/ropensci/mregions2'

##### packages #####
library(tidyverse)
library(sf)
library(mregions2)
library(leaflet)

##### Load NOAA NDB occurrence data #####
source('code/dst_tool_load_current_ndb.R')

##### Fetch Specific MEOW Realm Boundaries via Gazetteer #####
# The Gazetteer requires exact string matches to the Spalding et al. (2007) ontology.

# 1. Search for the exact MEOW Realm names
realm_np <- gaz_search("Temperate Northern Pacific")
realm_cip <- gaz_search("Central Indo-Pacific")

# 2. Combine the records and strictly filter for the 'Marine Realm' placeType
# Note the camelCase 'placeType' to match the mregions2 schema.
realm_records <- bind_rows(realm_np, realm_cip) %>%
  filter(placeType == "Realm")

# 3. Retrieve the sf multipolygon geometries using the verified search results
realm_polys <- gaz_geometry(realm_records)

# 4. Join the text names back to the geometries for mapping and filtering
realm_polys <- realm_polys %>%
  left_join(realm_records %>% select(MRGID, preferredGazetteerName), by = "MRGID")

##### Efficient Spatial Subsetting & Joining #####
# Disable s2 temporarily to bypass realm polygon micro-topology errors
sf_use_s2(FALSE)

# 1. Spatially Subset FIRST (Maximum Efficiency)
# This uses spatial indexing to instantly drop all global NDB records
# that do not fall within our two designated MEOW Realms.
ndb_subset <- ndb_sf[realm_polys, ]

# 2. Perform the Attribute Join
# Now we run st_join() ONLY on the few thousand points that actually
# exist in the Temperate Northern Pacific and Central Indo-Pacific.
target_occurrences <- st_join(ndb_subset, realm_polys, join = st_intersects)

# Re-enable s2 for future geometric operations
sf_use_s2(TRUE)

##### Interactive Biogeographic Visualization #####
# Render only the spatially subsetted occurrences.
leaflet(target_occurrences) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(
    radius = 4,
    color = ~ifelse(preferredGazetteerName.x == "Temperate Northern Pacific", "#56B4E9", "#E69F00"),
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>Taxon:</b> ", ScientificName, "<br>",
      "<b>Realm:</b> ", preferredGazetteerName.x, "<br>",
      "<b>Depth:</b> ", DepthInMeters, " m"
    )
  )
