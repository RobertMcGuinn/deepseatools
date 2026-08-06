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

  filter(TaxonRank == 'species') %>%
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

##### Create the missing top_50_points object #####
## Inner join with your summary table.
# This filters the map points down to JUST the top 50 species per region,
# while preserving the geometry and pulling in the 'TotalAbundance' column for the popup.
top_50_points <- points_basin %>%
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

##### Static maps for each region (full DSCRTP database) #####

# 0. Automatically check and install required dependencies
if (!requireNamespace("prettymapr", quietly = TRUE)) {
  message("Installing missing dependency 'prettymapr' required for annotation_map_tile()...")
  install.packages("prettymapr")
}

library(tidyverse)
library(sf)
library(ggspatial)
library(googledrive)
library(jsonlite)

# 1. Prepare Spatial Occurrences Directly from 'filt' & Create Explicit Abundance Bins
filt_subregion_sf <- filt %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  mutate(SubRegion = case_when(
    between(Latitude, 22, 30) & between(Longitude, -179, -160) ~ "NWHI",
    between(Latitude, 18.5, 23) & between(Longitude, -161, -154) ~ "MHI",
    between(Latitude, -17, -10) & between(Longitude, -175, -165) ~ "Samoa",
    between(Latitude, 13, 21) & between(Longitude, 144, 146.5) ~ "Mariana",
    TRUE ~ "Other"
  )) %>%
  filter(SubRegion != "Other") %>%
  mutate(
    AdjustedCount = if_else(IndividualCount == -999, 1, as.numeric(IndividualCount)),
    # Categorize into explicit factor levels so sizes can never be compressed
    AbundanceClass = cut(
      AdjustedCount,
      breaks = c(-Inf, 10, 50, 250, 500, Inf),
      labels = c("1 - 10", "11 - 50", "51 - 250", "251 - 500", "500+"),
      right = TRUE
    )
  ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# 2. Define ESRI World Ocean Basemap Tile URL
esri_ocean <- "https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg"

# 3. Helper Function: Fetch NOAA Ship Okeanos Explorer ROV Dive Locations
load_okeanos_dives <- function(item_id = "d41f1eaf0c0c4e258d63eb05ee91d1f5") {
  message("Fetching Okeanos Explorer ROV Dive Locations from ArcGIS Online...")
  meta_url <- paste0("https://www.arcgis.com/sharing/rest/content/items/", item_id, "?f=json")
  meta_json <- jsonlite::fromJSON(meta_url)

  # Query Layer 0 directly as GeoJSON
  query_url <- paste0(meta_json$url, "/0/query?where=1%3D1&outFields=*&f=geojson")

  dives_sf <- sf::st_read(query_url, quiet = TRUE) %>%
    st_transform(4326)

  message("Successfully loaded ", nrow(dives_sf), " Okeanos Explorer ROV dive sites.")
  return(dives_sf)
}

# Load all Okeanos Explorer ROV dive sites once before mapping loop
okeanos_dives_sf <- load_okeanos_dives()

# 4. Target the Dedicated Google Drive Map Folder
map_folder_id <- as_id("1FbjOfgrZrRwJQwuVV6MWIJGTS103naQG")

# 5. Define Map Production & Drive Upload Function
generate_and_upload_map <- function(region_name) {

  # Filter DSCRTP occurrence points for the specific sub-region
  region_points <- filt_subregion_sf %>% filter(SubRegion == region_name)

  if (nrow(region_points) == 0) {
    message("No occurrence data found for subregion: ", region_name)
    return(NULL)
  }

  # -------------------------------------------------------------------------
  # ASPECT-RATIO-AWARE BOUNDING BOX CALCULATION
  # -------------------------------------------------------------------------
  bbox <- st_bbox(region_points)
  x_span <- bbox["xmax"] - bbox["xmin"]
  y_span <- bbox["ymax"] - bbox["ymin"]

  if (x_span == 0) x_span <- 2
  if (y_span == 0) y_span <- 2

  target_ar <- 1.25
  current_ar <- x_span / y_span

  if (current_ar < target_ar) {
    desired_x_span <- y_span * target_ar
    pad_x <- ((desired_x_span - x_span) / 2) + (x_span * 0.08)
    pad_y <- y_span * 0.08
  } else {
    pad_x <- x_span * 0.12
    pad_y <- y_span * 0.12
  }

  # -------------------------------------------------------------------------
  # FILTER OKEANOS EXPLORER DIVE SITES TO CURRENT MAP WINDOW
  # -------------------------------------------------------------------------
  map_bbox_poly <- st_as_sfc(st_bbox(c(
    xmin = max(-180, bbox["xmin"] - pad_x),
    xmax = min(180,  bbox["xmax"] + pad_x),
    ymin = max(-90,  bbox["ymin"] - pad_y),
    ymax = min(90,   bbox["ymax"] + pad_y)
  ), crs = st_crs(4326)))

  region_dives <- okeanos_dives_sf %>% st_filter(map_bbox_poly)
  message("  -> Found ", nrow(region_dives), " Okeanos Explorer dive sites in ", region_name)
  # -------------------------------------------------------------------------

  # Build the cartographic layout
  p <- ggplot() +
    # 1. ESRI World Ocean Basemap (includes bathymetry, coastlines & relief)
    annotation_map_tile(
      type = esri_ocean,
      zoomin = 1,
      progress = "none"
    ) +

    # 2. DSCRTP Occurrence Points: Mapped to discrete factor 'AbundanceClass'
    geom_sf(
      data = region_points,
      aes(size = AbundanceClass),
      shape = 21,
      fill = "#ff7f0080",  # ~50% transparency so overlaps compound
      color = "#222222",   # Solid dark border keeps individual points legible
      stroke = 0.35,
      show.legend = "point",
      inherit.aes = FALSE
    ) +

    # 3. Okeanos Explorer ROV Dive Sites (Hollow Outlined Triangles - shape = 2)
    geom_sf(
      data = region_dives,
      aes(color = "Okeanos Explorer ROV Dive"),
      shape = 2,           # Open / hollow upward triangle (no fill)
      size = 1.6,          # Smaller marker footprint
      stroke = 0.8,        # Clean, visible stroke
      alpha = 0.95,
      show.legend = "point",
      inherit.aes = FALSE
    ) +

    # HARDCODED MANUAL SIZES: Guarantees a 1.0+ millimeter jump between every class!
    scale_size_manual(
      name = "Observation\nCount",
      values = c(
        "1 - 10"    = 1.2,
        "11 - 50"   = 2.2,
        "51 - 250"  = 3.4,
        "251 - 500" = 4.6,
        "500+"      = 6.0
      ),
      drop = FALSE          # Keeps legend identical across all subregions
    ) +

    # Dedicated Color Scale for Hollow ROV Dive Markers
    scale_color_manual(
      name = "ROV Operations",
      values = c("Okeanos Explorer ROV Dive" = "black")
    ) +

    # CRITICAL FIX: crs = 4326 ensures coordinates are projected properly in degrees
    coord_sf(
      crs = 4326,
      xlim = c(max(-180, bbox["xmin"] - pad_x), min(180, bbox["xmax"] + pad_x)),
      ylim = c(max(-90,  bbox["ymin"] - pad_y), min(90,  bbox["ymax"] + pad_y)),
      expand = FALSE,
      datum = st_crs(4326)
    ) +

    # 4. CARTOGRAPHY ON THE RIGHT SIDE OF THE MAP
    # Scale bar in Bottom-Right ("br")
    annotation_scale(
      location = "br",
      width_hint = 0.18,
      height = unit(0.06, "in"),
      text_cex = 0.6,
      pad_x = unit(0.12, "in"),
      pad_y = unit(0.12, "in")
    ) +
    # North Arrow in Top-Right ("tr")
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      height = unit(0.3, "in"),
      width = unit(0.3, "in"),
      pad_x = unit(0.12, "in"),
      pad_y = unit(0.12, "in"),
      style = north_arrow_fancy_orienteering(text_size = 6)
    ) +

    # 5. ISOLATED LEGENDS: Mirrors the shape = 21 styling in the legend!
    guides(
      size = guide_legend(
        order = 1,
        override.aes = list(
          shape = 21,
          fill = "#ff7f0080",
          color = "#222222",
          stroke = 0.35
        )
      ),
      color = guide_legend(
        order = 2,
        override.aes = list(shape = 2, size = 2.0, stroke = 0.8)
      )
    ) +

    # Theme & Layout Design
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_line(color = gray(0.85), linetype = "dashed"),
      panel.background = element_rect(fill = "aliceblue", color = "black", linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 7.5, color = "grey30", lineheight = 1.15),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = element_text(size = 7.5, face = "bold"),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.14, "in"),
      legend.margin = margin(0, 0, 0, 0),
      axis.title = element_blank(),
      axis.text = element_text(size = 7)
    ) +
    labs(
      title = paste(region_name),
      subtitle = "NOAA's National Database for Deep-sea Corals and Sponges (20260416-1)\n & Okeanos ROV Dives"
    )

  # Define filenames and output paths
  local_filename <- paste0("indata/map_", region_name, "_all_occurrences.png")
  drive_filename <- paste0(format(Sys.time(), "%Y%m%d"), "_map_", region_name, "_all_occurrences.png")

  # Save locally at landscape dimensions: 5.5 in wide x 3.5 in high, 300 DPI PNG
  ggsave(
    filename = local_filename,
    plot = p,
    width = 5.5,
    height = 3.5,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  message("Saved local layout: ", local_filename)

  # Upload to Google Drive
  tryCatch({
    drive_upload(
      media = local_filename,
      path = map_folder_id,
      name = drive_filename,
      overwrite = TRUE
    )
    message("Successfully uploaded to Drive: ", drive_filename)
  }, error = function(e) {
    warning("Drive upload failed for ", region_name, ": ", e$message)
  })
}

# 6. Execute Across All Identified Sub-Regions
all_subregions <- unique(filt_subregion_sf$SubRegion)
purrr::walk(all_subregions, generate_and_upload_map)
