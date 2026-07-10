##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20260601
## purpose: Top 50 analysis of species in the Western South Pacific vs. the Western North Pacific
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

##### Convert NOAA DSCRTP Data to Spatial Object and filter for Western/Central Pacific #####
# Utilizing exact DSCRTP schema column names: Longitude, Latitude
dscrtp_sf <- filt %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%

  # Filter for Longitude: 100°E to 180° AND -180° to -150°W
  filter(
    (Longitude > 100 & Longitude <= 180) |
      (Longitude >= -180 & Longitude <= -150)
  ) %>%

  # Filter for Latitude: Restrict your north/south bounds here
  # (Example: Restricting to between 50°S and 50°N)
  filter(Latitude >= -30 & Latitude <= 30) %>%

  # Filter for TaxonRank
  filter(TaxonRank == 'species')

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

##### Create the missing top_50_points object #####

# 1. Re-create the base spatial points with their AdjustedCount
points_with_regions <- st_join(dscrtp_sf, pacific_iho, join = st_intersects) %>%
  filter(!is.na(TargetRegion)) %>%
  filter(!is.na(ScientificName) & ScientificName != "") %>%
  mutate(AdjustedCount = if_else(IndividualCount == -999, 1, as.numeric(IndividualCount)))

# 2. Inner join with your summary table.
# This filters the map points down to JUST the top 50 species per region,
# while preserving the geometry and pulling in the 'TotalAbundance' column for the popup.
top_50_points <- points_with_regions %>%
  inner_join(species_by_basin, by = c("TargetRegion", "ScientificName"))
##### Shift the IHO polygons for mapping #####
pacific_iho_shifted <- st_shift_longitude(pacific_iho)

##### Shift the occurrence points for mapping #####
top_50_points_shifted <- st_shift_longitude(top_50_points)

##### Visualize Top 50 Species with Leaflet (Pacific-Centric) #####
# Build the interactive map using the shifted data
leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  # Add the shifted IHO Ocean boundaries
  addPolygons(
    data = pacific_iho_shifted,
    color = "#444444",
    weight = 2,
    fillOpacity = 0.05,
    popup = ~name
  ) %>%

  # Add the shifted occurrence points
  addCircleMarkers(
    data = top_50_points_shifted,
    radius = 4,
    color = ~basin_palette(TargetRegion),
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<strong>Species:</strong> ", ScientificName, "<br>",
      "<strong>Region:</strong> ", TargetRegion, "<br>",
      "<strong>Observation Count:</strong> ", AdjustedCount, "<br>",
      "<strong>Basin Total Abundance:</strong> ", TotalAbundance
    )
  ) %>%

  # Add the legend
  addLegend(
    data = top_50_points_shifted,
    position = "bottomright",
    pal = basin_palette,
    values = ~TargetRegion,
    title = "Top 50 Occurrences by Basin",
    opacity = 1
  )

##### Take a look at the metadata for IHO layer #####
library(mregions2)
library(dplyr)

# mrp_list is a built-in data frame containing metadata for all available layers
iho_metadata <- mrp_list %>%
  filter(layer == "iho")

# View the abstract and layer details
print(iho_metadata$abstract)
glimpse(iho_metadata)
