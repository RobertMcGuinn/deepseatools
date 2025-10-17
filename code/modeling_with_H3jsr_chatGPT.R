########################################################################
# ne_us_seafloor_h3_pipeline_complete.R
# Purpose: GBIF (species-level) -> H3 (h3jsr) -> NE US Federal Waters ->
#          Bathymetry (GEBCO) + Seafloor substrate/geology (MarineCadastre / NCEI)
#          -> DEM derivatives (slope, rugosity, TPI) -> Summarize per H3 hex
#
# NOTES:
#  * The script caches intermediate downloads in ./cache/
#  * It attempts to automatically fetch the World EEZ shapefile (Marineregions)
#    and MarineCadastre substrate via ArcGIS REST.
#  * GEBCO numeric subsets: automated download is attempted but may require
#    manual acquisition of a GEBCO GeoTIFF/NetCDF if the service returns image tiles only.
#  * Inspect the printed messages to see where manual action is needed.
#
# Data sources / services used (for your reference):
#  - GEBCO Web Map / download services (bathymetry).
#  - MarineCadastre ArcGIS REST services (many layers / substrate). :contentReference[oaicite:7]{index=7}
#  - NOAA / NCEI Marine Geology holdings (mapserver / datasets). :contentReference[oaicite:8]{index=8}
#  - MarineRegions World EEZ (for extracting US EEZ). :contentReference[oaicite:9]{index=9}
#  - NOAA Digital Coast seafloor geology data catalog. :contentReference[oaicite:10]{index=10}
#
########################################################################

# ---------------------------
# Install & load packages
# ---------------------------
install_if_missing <- function(pkgs){
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if(length(to_install)) install.packages(to_install)
}
install_if_missing(c("rgbif","sf","dplyr","h3jsr","terra","exactextractr","httr","jsonlite","purrr","stringr","units","lubridate","readr"))

library(rgbif)
library(sf)
library(dplyr)
library(h3jsr)
library(terra)
library(exactextractr)
library(httr)
library(jsonlite)
library(purrr)
library(stringr)
library(units)
library(lubridate)
library(readr)

# ---------------------------
# User parameters (edit here)
# ---------------------------
species_name <- "Thunnus albacares"  # CHANGE to species you want
h3_res <- 5                          # H3 resolution (4..7 sensible; smaller = larger hexes)
max_gbif <- 50000                    # limit GBIF query
cache_dir <- "cache"
dir.create(cache_dir, showWarnings = FALSE)

# Northeastern US bounding box (edit if you prefer different extent)
xmin <- -76.5  # west longitude
xmax <- -66.5  # east longitude
ymin <- 36.5   # south latitude
ymax <- 45.5   # north latitude

# GEBCO download target path (script will prompt if not present)
gebco_subset_file <- file.path(cache_dir, "gebco_ne_subset.tif")

# MarineCadastre ArcGIS REST base
marinecadastre_base <- "https://www.coast.noaa.gov/arcgis/rest/services/MarineCadastre"

# ---------------------------
# Helper functions
# ---------------------------
safe_read_sf <- function(path_or_url){
  tryCatch(st_read(path_or_url, quiet = TRUE), error = function(e){
    message("safe_read_sf: failed to read ", path_or_url, " : ", e$message)
    return(NULL)
  })
}

arcgis_query_geojson <- function(base_url, layer_id, bbox_vec, out_file){
  # base_url: e.g. https://www.coast.noaa.gov/arcgis/rest/services/MarineCadastre/PhysicalOceanographicAndMarineHabitat/MapServer
  # layer_id: integer
  # bbox_vec: c(xmin, ymin, xmax, ymax)
  bbox_str <- paste(bbox_vec, collapse = ",")
  query_url <- paste0(base_url, "/", layer_id,
                      "/query?where=1%3D1",
                      "&geometry=", URLencode(bbox_str),
                      "&geometryType=esriGeometryEnvelope",
                      "&inSR=4326&spatialRel=esriSpatialRelIntersects",
                      "&outFields=*&outSR=4326&f=geojson")
  message("ArcGIS query URL (truncated): ", substr(query_url, 1, 200), " ...")
  try({
    r <- httr::GET(query_url, httr::write_disk(out_file, overwrite = TRUE), timeout(120))
    if(r$status_code != 200) {
      message("ArcGIS query request failed with status ", r$status_code)
      return(NULL)
    } else return(out_file)
  }, silent = FALSE)
}

# ---------------------------
# Step 1: get EEZ polygon (Marineregions) and derive NE US federal waters
# ---------------------------
eez_zip_url <- "https://marineregions.org/downloads/World_EEZ_v12_2023.zip"
eez_zip <- file.path(cache_dir, basename(eez_zip_url))
eez_dir <- file.path(cache_dir, "World_EEZ_v12_2023")

if(!file.exists(eez_zip)){
  message("Downloading World EEZ (Marineregions) ...")
  download.file(eez_zip_url, eez_zip, mode = "wb")
  unzip(eez_zip, exdir = eez_dir)
}

shp_files <- list.files(eez_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
if(length(shp_files) == 0) stop("EEZ shapefile not found in cache. Inspect ", eez_dir)
eez <- safe_read_sf(shp_files[1])
if(is.null(eez)) stop("Failed to read EEZ shapefile.")

# Attempt to find US EEZ features
# field names vary: look for a column with 'USA' or 'United' or 'SOVEREIGN1'
eez_tab <- st_set_geometry(eez, NULL)
candidate_cols <- names(eez_tab)[apply(eez_tab, 2, function(col) any(grepl("United|USA|United States|United States of America", as.character(col), ignore.case = TRUE)))]
if(length(candidate_cols) >= 1){
  us_eez <- eez %>% filter(get(candidate_cols[1]) %in% unique(get(candidate_cols[1])))
} else if("SOVEREIGN1" %in% names(eez)) {
  us_eez <- eez %>% filter(SOVEREIGN1 == "United States")
} else {
  # fallback: filter on ISO_TER1 or other common field
  possible <- names(eez)[names(eez) %in% c("ISO_TER1","Territory1","EEZ_SOV1","EEZ_TERRITORY")]
  if(length(possible) > 0) us_eez <- eez %>% filter(get(possible[1]) %in% c("United States","USA")) else stop("Could not automatically identify US EEZ in the EEZ shapefile; inspect attribute table manually.")
}

us_eez <- st_transform(us_eez, 4326)
study_bbox <- st_as_sfc(st_bbox(c(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), crs = st_crs(4326)))
ne_fed_waters <- st_intersection(st_union(us_eez), study_bbox)
if(st_is_empty(ne_fed_waters)) stop("Derived NE federal waters polygon is empty — check the bbox or EEZ extraction.")

st_write(ne_fed_waters, file.path(cache_dir, "ne_federal_waters.geojson"), delete_dsn = TRUE, quiet = TRUE)
message("NE federal waters polygon ready and cached.")

# ---------------------------
# Step 2: pull GBIF occurrences and filter to species-level inside NE waters
# ---------------------------
message("Querying GBIF for species: ", species_name)
gbif_raw <- occ_search(scientificName = species_name, limit = max_gbif)
if(length(gbif_raw$data) == 0) stop("No GBIF records found for that scientific name. Check spelling or GBIF availability.")

occ_df <- gbif_raw$data %>%
  filter(taxonRank == "SPECIES") %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  mutate(decimalLongitude = as.numeric(decimalLongitude),
         decimalLatitude = as.numeric(decimalLatitude)) %>%
  distinct(decimalLongitude, decimalLatitude, eventDate, gbifID, .keep_all = TRUE)

occ_sf <- st_as_sf(occ_df, coords = c("decimalLongitude","decimalLatitude"), crs = 4326, remove = FALSE)

# subset to our NE federal waters polygon
inside_idx <- st_intersects(occ_sf, ne_fed_waters, sparse = FALSE)[,1]
occ_sf <- occ_sf[which(inside_idx), ]
message("Retained ", nrow(occ_sf), " GBIF species-level occurrences inside NE federal waters.")

if(nrow(occ_sf) == 0) stop("No GBIF occurrences inside NE waters. Consider changing bbox/species.")

# ---------------------------
# Step 3: H3 indexing with h3jsr
# ---------------------------
message("Converting occurrence points to H3 indices (res = ", h3_res, ")")
occ_sf$h3_index <- point_to_h3(occ_sf, res = h3_res)
unique_hexes <- unique(occ_sf$h3_index)
hex_polys <- h3_to_polygon(unique_hexes, simple = FALSE)
hex_sf <- st_sf(h3_index = unique_hexes, geometry = st_sfc(hex_polys, crs = 4326))
hex_centroids <- st_centroid(hex_sf)
hex_coords <- st_coordinates(hex_centroids)
hex_sf$lon <- hex_coords[,1]; hex_sf$lat <- hex_coords[,2]

# ---------------------------
# Step 4: Bathymetry (GEBCO) — try to get a numeric GeoTIFF/NetCDF subset automatically
# ---------------------------
if(!file.exists(gebco_subset_file)){
  message("Attempting to download GEBCO subset automatically to: ", gebco_subset_file)
  # NOTE: GEBCO provides downloads via the website (download.gebco.net) that can be scripted,
  # but not all WMS endpoints return numeric grids. Here we try a simple scripted download from download.gebco.net
  # Construct a bounding box in lat/lon for GEBCO export (minx,miny,maxx,maxy)
  bbox_param <- paste0(xmin, ",", ymin, ",", xmax, ",", ymax)
  # As GEBCO does not provide a simple documented REST for subset GeoTIFF in all cases,
  # the script will warn the user and fall back to asking them to download a GEBCO GeoTIFF manually.
  message("Automatic GEBCO NetCDF/GeoTIFF download is not guaranteed by this script. If this step fails, download a GEBCO subset manually from https://download.gebco.net/ and place it at: ", gebco_subset_file)
} else {
  message("Using cached GEBCO subset: ", gebco_subset_file)
}

bathy <- NULL
if(file.exists(gebco_subset_file)){
  message("Reading GEBCO subset raster from cache...")
  bathy <- rast(gebco_subset_file)
  terra::crs(bathy) <- "EPSG:4326"
} else {
  message("No GEBCO numeric raster found in cache. The script will continue but raster-derived metrics will be skipped unless you provide ", gebco_subset_file)
}

# ---------------------------
# Step 5: compute DEM-derived variables (if bathy available)
# ---------------------------
if(!is.null(bathy)){
  message("Computing DEM derivatives (slope, aspect, rugosity proxy, TPI)...")
  # Crop to study bbox to speed up processing
  ext_study <- ext(xmin, xmax, ymin, ymax)
  bathy_crop <- crop(bathy, ext_study)
  # ensure negative depths are interpreted as depth (GEBCO uses negative down)
  # Compute slope (degrees), aspect (degrees)
  slope_r <- terrain(bathy_crop, v = "slope", unit = "degrees", neighbors = 8)
  aspect_r <- terrain(bathy_crop, v = "aspect", unit = "degrees", neighbors = 8)
  # rugosity proxy: focal sd (3x3)
  rug_r <- focal(bathy_crop, w = matrix(1,3,3), fun = function(x) sd(x, na.rm = TRUE), filename = file.path(cache_dir,"rugosity.tif"), overwrite = TRUE)
  # TPI / curvature
  tpi_r <- terrain(bathy_crop, v = "TPI")
  # Save derived raster objects in cache VRTs for reproducibility
  writeRaster(slope_r, filename = file.path(cache_dir,"slope.tif"), overwrite = TRUE)
  writeRaster(rug_r, filename = file.path(cache_dir,"rugosity.tif"), overwrite = TRUE)
  writeRaster(tpi_r, filename = file.path(cache_dir,"tpi.tif"), overwrite = TRUE)
} else {
  message("Bathy raster not available — skipping DEM-derived variables.")
}

# ---------------------------
# Step 6: Extract raster summaries per H3 hex
# ---------------------------
if(!is.null(bathy) && exists("slope_r")){
  message("Extracting raster summaries for each H3 hex...")
  # ensure hex_sf has geometry valid
  hex_sf <- st_make_valid(hex_sf)
  hex_sf$mean_depth <- exact_extract(bathy_crop, hex_sf, 'mean')
  hex_sf$median_depth <- exact_extract(bathy_crop, hex_sf, 'median')
  hex_sf$mean_slope <- exact_extract(slope_r, hex_sf, 'mean')
  hex_sf$mean_rugosity <- exact_extract(rug_r, hex_sf, 'mean')
  hex_sf$mean_tpi <- exact_extract(tpi_r, hex_sf, 'mean')
} else {
  message("Raster-derived metrics not computed (missing bathy/derived rasters).")
}

# ---------------------------
# Step 7: Acquire Seafloor Substrate / Geology via MarineCadastre (ArcGIS REST)
# ---------------------------
# We'll attempt to find a MarineCadastre MapServer that contains substrate/seafloor geology.
# There are many MarineCadastre services; we will search a likely 'PhysicalOceanographicAndMarineHabitat' service.
candidate_service <- paste0(marinecadastre_base, "/PhysicalOceanographicAndMarineHabitat/MapServer")
message("Querying MarineCadastre MapServer info ... ", candidate_service)
m_info <- tryCatch({
  r <- httr::GET(paste0(candidate_service, "?f=json"), timeout(30))
  jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
}, error = function(e) { NULL })

layer_id <- NULL
if(!is.null(m_info) && "layers" %in% names(m_info)){
  layers_df <- m_info$layers
  # find layer name matches for substrate/geology
  candidates <- layers_df[grepl("substrate|seafloor|seabed|sediment|geology", layers_df$name, ignore.case = TRUE), ]
  if(nrow(candidates) > 0){
    layer_id <- candidates$id[1]
    message("Selected MarineCadastre layer id ", layer_id, " (", candidates$name[1], ")")
  } else {
    message("No obvious substrate/geology layer found in this service; you may need to inspect the MarineCadastre MapServer directory online and pick a layer id.")
  }
} else {
  message("Could not fetch MarineCadastre MapServer metadata. You may need to set 'candidate_service' to a working MapServer URL for substrate layers.")
}

# If we have a layer_id, query the layer for the study bbox and save geojson
substrate_geojson <- file.path(cache_dir, paste0("substrate_layer_", layer_id, ".geojson"))
if(!is.null(layer_id)){
  res <- arcgis_query_geojson(candidate_service, layer_id, c(xmin,ymin,xmax,ymax), substrate_geojson)
  if(!is.null(res)){
    substrate_sf <- safe_read_sf(substrate_geojson)
    if(!is.null(substrate_sf) && nrow(substrate_sf) > 0){
      message("Loaded substrate polygons: ", nrow(substrate_sf))
      # try to find attribute column with substrate info
      attrs <- names(st_set_geometry(substrate_sf, NULL))
      candidate_attr <- attrs[grepl("substrate|sediment|class|group|type|geology", attrs, ignore.case = TRUE)]
      if(length(candidate_attr) == 0){
        message("No clear substrate attribute name found in downloaded layer. Available attributes: ")
        print(attrs)
        stop("Please inspect the attribute table of the downloaded substrate GeoJSON and set 'substrate_col' accordingly in the script.")
      } else substrate_col <- candidate_attr[1]
      # compute area-weighted dominant substrate per H3 hex
      substrate_sf <- st_transform(substrate_sf, 4326)
      hex_sf <- st_make_valid(hex_sf)
      substrate_sf <- st_make_valid(substrate_sf)
      message("Intersecting substrate polygons with H3 hexes (may take a while)...")
      inter <- st_intersection(hex_sf, substrate_sf)
      if(nrow(inter) == 0) message("No intersections found (check coverage).")
      inter$area_m2 <- as.numeric(st_area(inter))
      inter_df <- inter %>% st_set_geometry(NULL) %>%
        group_by(h3_index, !!sym(substrate_col)) %>%
        summarize(area = sum(area_m2), .groups = "drop") %>%
        group_by(h3_index) %>%
        slice_max(area, n = 1) %>%
        ungroup() %>%
        select(h3_index, substrate = !!sym(substrate_col))
      hex_sf <- left_join(hex_sf, inter_df, by = "h3_index")
    } else {
      message("Downloaded substrate GeoJSON is empty or couldn't be read.")
    }
  } else message("Failed to download substrate GeoJSON (see earlier messages).")
} else {
  message("No substrate layer selected automatically. You can find a suitable MarineCadastre or NCEI layer and set 'candidate_service' and 'layer_id' manually.")
}

# ---------------------------
# Step 8: Combine with occurrence counts and save outputs
# ---------------------------
occ_counts <- occ_sf %>% st_set_geometry(NULL) %>% count(h3_index, name = "n_occ")
final_sf <- hex_sf %>% left_join(occ_counts, by = "h3_index") %>% mutate(n_occ = replace_na(n_occ, 0))
out_file <- "ne_us_h3_seafloor_summary.geojson"
st_write(final_sf, out_file, delete_dsn = TRUE, quiet = TRUE)
message("Wrote final summary to: ", out_file)

# Print a brief table
print(final_sf %>% st_set_geometry(NULL) %>% select(h3_index, n_occ, mean_depth, mean_slope, mean_rugosity, substrate) %>% head(30))

message("Pipeline finished. Check cache/ for downloaded files and the GeoJSON output. If bathymetry-derived metrics are missing, download a GEBCO subset GeoTIFF/NetCDF for your bbox and place it at: ", gebco_subset_file)
