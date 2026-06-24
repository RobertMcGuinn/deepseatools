##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20260612
## purpose:The purpose of this analysis is to facilitate the update
## of the Environmental Sensitivity Index (ESI) atlas for Sector St. Pete, Florida.
## This initiative aims to align regional data with the NOAA ESI data schema, utilizing
## project funding and support from the Florida Fish and Wildlife Conservation Commission (FWRI).
## Specifically, this request seeks to retrieve DSCRTP point location data
## for the defined Area of Interest to ensure the updated atlas maintains consistency with previous regional projects.
## This analysis is performed on behalf of:
## Lauren Szathmary
## Ecologist
## Research Planning, Inc.
## 1121 Park Street, Columbia, SC 29201
## 803-256-7322
## www.researchplanning.com
## google drive folder: https://drive.google.com/drive/folders/1e3E6c_Mw9plaQbpMBFa-jVxnjB-IRNRQ?usp=drive_link

##### parameters #####

##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(current_file)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)
gdrive <- 'https://drive.google.com/drive/folders/1e3E6c_Mw9plaQbpMBFa-jVxnjB-IRNRQ?usp=drive_link'
# browseURL(gdrive)

##### packages #####
library(googledrive)
library(sf)
library(tidyverse)
library(leaflet)

##### load current version of NDB #####
source('code/dst_tool_load_current_ndb.R')

##### Authenticate, Download, and Extract the AOI Shapefile #####
# We use the unique Google Drive ID from your provided URL.
# Note: The first time you run this, R will prompt you to authenticate your Google account in the console/browser.
drive_id <- as_id("1FLMwq8O9eOLBUjiO-_A6TMgyOdV0ce_X")

# Create temporary paths for the download and extraction to keep your working directory clean
zip_path <- tempfile(fileext = ".zip")
extract_dir <- tempdir()

# Download the zipped shapefile
drive_download(drive_id, path = zip_path, overwrite = TRUE)

# Unzip the component files of the shapefile (.shp, .shx, .dbf, .prj, etc.)
unzip(zip_path, exdir = extract_dir)

# Locate the actual .shp file within the extracted directory
shp_file <- list.files(extract_dir, pattern = "\\.shp$", full.names = TRUE)

# Read the shapefile into an sf (simple features) object
aoi_polygon <- st_read(shp_file)

##### Convert the NOAA Database Object ('filt') to a Spatial Object #####
filt_sf <- st_as_sf(filt,
                    coords = c("Longitude", "Latitude"),
                    crs = 4326, # WGS 84 is the standard unprojected CRS for NOAA DSCRTP point data
                    remove = FALSE)

##### Geospatial Harmonization: Coordinate Reference System (CRS) Transformation #####
aoi_polygon <- st_transform(aoi_polygon, crs = st_crs(filt_sf))

##### Perform the Spatial Extraction (Intersection) #####
# We use st_filter to subset the points that geometrically intersect with the AOI.
# This efficiently isolates your cold-water coral and sponge records within the targeted extent.
filt_aoi <- st_filter(filt_sf, aoi_polygon)

##### check (on map) #####
validation_map <- leaflet() %>%
  # Use an ocean-specific basemap for bathymetric context
  addProviderTiles(providers$Esri.OceanBasemap) %>%

  # Add the buffered Area of Interest polygon
  addPolygons(data = aoi_polygon,
              color = "#FF0000",      # Red boundary
              weight = 2,             # Line thickness
              fillColor = "#FF0000",
              fillOpacity = 0.1,      # Transparent fill
              group = "AOI Boundary",
              popup = "Area of Interest Buffer") %>%

  # Add the extracted cold-water coral and sponge occurrences
  addCircleMarkers(data = filt_aoi,
                   radius = 4,
                   color = "#000080",     # Navy blue points
                   stroke = FALSE,        # Remove border for cleaner look
                   fillOpacity = 0.7,
                   group = "Extracted Taxa",
                   # Utilize standard NOAA schema fields for the interactive popup
                   popup = ~paste0("<b>Scientific Name:</b> ", ScientificName, "<br>",
                                   "<b>Depth:</b> ", DepthInMeters, " m<br>",
                                   "<b>Observation Date:</b> ", ObservationDate)) %>%

  # Add a layer control to toggle layers on and off
  addLayersControl(overlayGroups = c("AOI Boundary", "Extracted Taxa"),
                   options = layersControlOptions(collapsed = FALSE))

# Print the map to the RStudio Viewer pane or web browser
print(validation_map)

##### Write the Resulting Subset to CSV #####
# We drop the sf geometry list-column before exporting to ensure a clean,
# standard tabular format for your downstream analyses.
filt_aoi %>%
  st_drop_geometry() %>%
  write_csv("indata/20260612-0_Section_Saint_Pete_AOI_Extracted_DeepSea_Taxa_NDB_Version_20260416-1.csv")
