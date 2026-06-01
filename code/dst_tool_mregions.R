##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: YYYYMMDD
## purpose:

##### parameters #####
##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(filename)
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

##### load NOAA NDB occurrence data #####
source('code/dst_tool_load_current_ndb.R')

##### Deactivate the strict s2 spherical geometry engine to bypass IHO topology bugs #####
sf_use_s2(FALSE)

##### Fetch IHO Ocean Boundaries Natively via mregions2 #####
# By using a server-side cql_filter, the Geoserver isolates the shapes BEFORE downloading.
pacific_iho <- mrp_get(
  layer = "iho",
  cql_filter = "name IN ('North Pacific Ocean', 'South Pacific Ocean')"
) %>%
  mutate(TargetRegion = case_when(
    name == "North Pacific Ocean" ~ "Western North Pacific",
    name == "South Pacific Ocean" ~ "Western South Pacific"
  )) %>%
  # Programmatically repair any invalid geometries or self-intersections in the shapes
  st_make_valid()

##### Convert NOAA DSCRTP Data to Spatial Object #####
# Utilizing exact DSCRTP schema column names: Longitude, Latitude
dscrtp_sf <- filt %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  # Filter for the Eastern Hemisphere / Western Pacific (typically between 100°E and 180°E)
  filter(Longitude > 100 & Longitude <= 180) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

##### Spatial Join and Top 50 Tally #####
# Intersect points with IHO polygons and calculate total abundance per species
# Utilizing exact DSCRTP schema column names: ScientificName, IndividualCount
species_by_basin <- st_join(dscrtp_sf, pacific_iho, join = st_intersects) %>%
  filter(!is.na(TargetRegion)) %>%
  filter(!is.na(ScientificName) & ScientificName != "") %>%

  # Standardize IndividualCount: presence-only markers (-999) become 1
  mutate(AdjustedCount = if_else(IndividualCount == -999, 1, as.numeric(IndividualCount))) %>%

  # Group by region and species, then calculate total abundance
  group_by(TargetRegion, ScientificName) %>%
  summarise(TotalAbundance = sum(AdjustedCount, na.rm = TRUE), .groups = "drop") %>%
  st_drop_geometry() %>% # Convert back to a standard data frame for cleaner display

  # Sort and extract the top 50 for each side of the equator
  arrange(TargetRegion, desc(TotalAbundance)) %>%
  group_by(TargetRegion) %>%
  slice_head(n = 50)

print(species_by_basin, n=100)

##### write resulting table #####
write_csv2(species_by_basin, 'indata/20260601_DSCRTP_NatDB_20260416-1_top_50_species_by_basin.csv')
