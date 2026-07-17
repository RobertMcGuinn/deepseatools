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

##### Load IHO Ocean Boundaries Natively via mregions2 #####
# By using a server-side cql_filter, the Geoserver isolates the shapes BEFORE downloading.
pacific_iho <- mrp_get(layer = "iho",
                       cql_filter = "name IN ('North Pacific Ocean', 'South Pacific Ocean')") %>%
  ## create better label
  mutate(TargetRegion = case_when(
    name == "North Pacific Ocean" ~ "North Pacific",
    name == "South Pacific Ocean" ~ "South Pacific"
  )) %>%
  # Programmatically repair any invalid geometries or self-intersections in the shapes
  st_make_valid()

##### Filter and label SubRegions on NOAA DSCRTP data and create spatial data #####
dscrtp_sf <- filt %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%

  # Tag the sub-regions using bounding boxes BEFORE filtering
  mutate(SubRegion = case_when(
    between(Latitude, 22, 30) & between(Longitude, -179, -160) ~ "NWHI",
    between(Latitude, 18.5, 23) & between(Longitude, -161, -154) ~ "MHI",
    between(Latitude, -17, -10) & between(Longitude, -175, -165) ~ "Samoa",
    between(Latitude, 13, 21) & between(Longitude, 144, 146.5) ~ "Mariana",
    TRUE ~ "Other"
  )) %>%

  ## Limit geographic extent further
  # filter(
  #   (Longitude > 100 & Longitude <= 180) |
  #     (Longitude >= -180 & Longitude <= -150)
  # ) %>%
  # filter(Latitude >= -30 & Latitude <= 30) %>%

  ## filter(TaxonRank == 'species') %>%
  filter(Phylum != 'Chordata') %>%

  # Standardize counts globally before spatial conversion
  mutate(AdjustedCount = if_else(IndividualCount == -999, 1, as.numeric(IndividualCount))) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

##### LEG 1: Basin Analysis (North vs. South Pacific) #####
points_basin <- st_join(dscrtp_sf, pacific_iho, join = st_intersects) %>%
  filter(!is.na(TargetRegion)) %>%
  filter(!is.na(ScientificName) & ScientificName != "")

species_by_basin <- points_basin %>%
  group_by(TargetRegion, ScientificName) %>%
  summarise(
    TotalAbundance = sum(AdjustedCount, na.rm = TRUE),
    VernacularName = paste(unique(na.omit(VernacularNameCategory)), collapse = ", "),
    AphiaID = paste0(unique(AphiaID), collapse = '|'),
    .groups = "drop"
  ) %>%
  st_drop_geometry() %>%
  arrange(TargetRegion, desc(TotalAbundance)) %>%
  group_by(TargetRegion) %>%
  slice_head(n = 50)


##### LEG 2: Sub-Region Analysis (NWHI, MHI, Samoa, Mariana) #####
points_subregion <- dscrtp_sf %>%
  # Filter out points that didn't fall into the 4 bounding boxes
  filter(SubRegion != "Other") %>%
  filter(!is.na(ScientificName) & ScientificName != "")

species_by_subregion <- points_subregion %>%
  group_by(SubRegion, ScientificName) %>%
  summarise(
    TotalAbundance = sum(AdjustedCount, na.rm = TRUE),
    VernacularName = paste(unique(na.omit(VernacularNameCategory)), collapse = ", "),
    AphiaID = paste0(unique(AphiaID), collapse = '|'),
    .groups = "drop"
  ) %>%
  st_drop_geometry() %>%
  arrange(SubRegion, desc(TotalAbundance)) %>%
  group_by(SubRegion) %>%
  slice_head(n = 50)

##### Upload to Google Drive as a Multi-Tab Google Sheet #####
library(googledrive)
library(googlesheets4)

folder_id <- as_id("1Ws6LLIXJFXFZwxWrCvPuFl8tkPjlyG65")

# Create a single Google Sheet with two tabs!
safe_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
new_sheet <- gs4_create(
  name = paste0(safe_time, "_DSCRTP_Top_50_Analysis"),
  sheets = list(
    Basin_Top_50 = species_by_basin,
    SubRegion_Top_50 = species_by_subregion
  )
)

# Move it to your specific folder2
drive_mv(file = new_sheet, path = folder_id)

# Write local CSV backups if needed
write_csv(species_by_basin, 'indata/top_50_basin.csv')
write_csv(species_by_subregion, 'indata/top_50_subregion.csv')

##### ----- mapping ----- #####
##### Create the missing top_50_points object #####
## Inner join with your summary table.
# This filters the map points down to JUST the top 50 species per region,
# while preserving the geometry and pulling in the 'TotalAbundance' column for the popup.
top_50_points <- points_with_regions %>%
  inner_join(species_by_basin, by = c("TargetRegion", "ScientificName"))
##### Shift the IHO polygons for mapping #####
pacific_iho_shifted <- st_shift_longitude(pacific_iho)

##### Shift the occurrence points for mapping #####
top_50_points_shifted <- st_shift_longitude(top_50_points)

##### Define map color palette #####
basin_palette <- colorFactor(
  palette = c("#1f78b4", "#33a02c"), # Blue for North, Green for South
  domain = c("North Pacific", "South Pacific")
)
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


# mrp_list is a built-in data frame containing metadata for all available layers
iho_metadata <- mrp_list %>%
  filter(layer == "iho")

# View the abstract and layer details
print(iho_metadata$abstract)
glimpse(iho_metadata)

##### Cartographic Layouts for Leg 2 Sub-Regions #####
library(ggspatial)
library(rnaturalearth)

# 1. Create the spatial points object for Leg 2 (if not already created)
top_50_points_subregion <- points_subregion %>%
  inner_join(species_by_subregion, by = c("SubRegion", "ScientificName"))

# 2. Fetch basic global land polygons for geographic context
world_land <- ne_countries(scale = "medium", returnclass = "sf")

# 3. Define a custom function to build and export a map for any given region
create_subregion_map <- function(region_name) {

  # Isolate the points for the current loop's region
  region_points <- top_50_points_subregion %>% filter(SubRegion == region_name)

  # Extract the bounding box to automatically zoom the map to the data
  bbox <- st_bbox(region_points)

  # Build the cartographic layout
  p <- ggplot() +
    # Add the ocean background and landmasses
    geom_sf(data = world_land, fill = "antiquewhite", color = "grey60", size = 0.2) +

    # Add the occurrence points (colored by species, sized by abundance)
    geom_sf(data = region_points, aes(color = ScientificName, size = AdjustedCount), alpha = 0.7) +

    # Zoom to the region's bounding box (with a 2-degree padding so points aren't cut off)
    # The datum = st_crs(4326) argument ensures graticules (lat/lon grid) are drawn
    coord_sf(
      xlim = c(bbox["xmin"] - 2, bbox["xmax"] + 2),
      ylim = c(bbox["ymin"] - 2, bbox["ymax"] + 2),
      expand = FALSE,
      datum = st_crs(4326)
    ) +

    # Add Cartographic Elements
    annotation_scale(location = "bl", width_hint = 0.3) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering) +

    # Theming and Layout
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = gray(0.8), linetype = "dashed"),
      panel.background = element_rect(fill = "aliceblue"),
      legend.position = "none", # Suppressing the legend because 50 species colors is too cluttered
      plot.title = element_text(face = "bold", size = 14)
    ) +
    labs(
      title = paste("Top 50 Deep-Sea Species Occurrences:", region_name),
      subtitle = "NOAA DSCRTP Database",
      x = "Longitude",
      y = "Latitude"
    )

  # Save the map as a high-res PNG
  filename <- paste0("indata/map_", region_name, "_top_50.png")
  ggsave(filename, plot = p, width = 8, height = 6, dpi = 300, bg = "white")

  # Print a confirmation to the console
  print(paste("Successfully saved cartographic layout:", filename))
}

# 4. Automate the mapping!
# Extract the unique region names and map the function over all of them.
subregions <- unique(top_50_points_subregion$SubRegion)
purrr::walk(subregions, create_subregion_map)
##### Upload Static Maps to Dedicated Google Drive Folder #####
library(googledrive)

# 1. Target the new Google Drive folder ID from your URL
map_folder_id <- as_id("1IMSOnK55iTXUrxA8-ihQsPXg36yzylwY")

# 2. Define a function to handle the upload for each sub-region
upload_subregion_map <- function(region_name) {

  # Define the local file path where the map was saved
  local_file <- paste0("indata/map_", region_name, "_top_50.png")

  # Ensure the file actually exists locally before attempting upload
  if (file.exists(local_file)) {

    # Create a clean, dated filename for Google Drive
    drive_filename <- paste0(format(Sys.time(), "%Y%m%d"), "_map_", region_name, "_top_50.png")

    # Upload the file
    drive_upload(
      media = local_file,
      path = map_folder_id,
      name = drive_filename,
      overwrite = TRUE # Updates the file if you run the script multiple times in one day
    )

    print(paste("Successfully uploaded to Drive:", drive_filename))

  } else {
    warning(paste("Could not find local map file for region:", region_name))
  }
}

# 3. Run the upload loop across all your sub-regions
# (This re-uses the 'subregions' vector created in the mapping step)
purrr::walk(subregions, upload_subregion_map)
