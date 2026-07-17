library(terra)

##### Setup and Download #####

# Create the 'bathygap' directory to keep the root tidy
download_dir <- "indata/bathygap"
if (!dir.exists(download_dir)) {
  dir.create(download_dir)
}

# Base export URL for the MapServer
base_url <- "https://gis.ngdc.noaa.gov/arcgis/rest/services/bathy_gap_analysis/MapServer/export"

# Define Area of Interest (AOI) bounding box (xmin, ymin, xmax, ymax)
bbox_coords <- "-90,24,-75,35"
epsg_code <- "4326"

# Define the specific layers we want to download
target_layers <- c(0, 7)

# Loop through our target layers to download and plot each one
for (layer_id in target_layers) {

  query_url <- paste0(
    base_url,
    "?bbox=", bbox_coords,
    "&bboxSR=", epsg_code,
    "&layers=show:", layer_id,
    "&size=1500,1500",
    "&imageSR=", epsg_code,
    "&format=tiff",
    "&f=image"
  )

  dest_file <- file.path(download_dir, paste0("bathy_gap_layer", layer_id, ".tif"))

  cat("Downloading Layer", layer_id, "to", dest_file, "...\n")
  download.file(query_url, destfile = dest_file, mode = "wb", quiet = TRUE)

  # Load the raster and suppress the initial metadata warning
  bathy_raster <- suppressWarnings(rast(dest_file))

  # Apply spatial metadata
  ext(bathy_raster) <- c(-90, -75, 24, 35) # xmin, xmax, ymin, ymax
  crs(bathy_raster) <- "EPSG:4326"

  # FIX: Flip the image vertically to correct the top-left vs bottom-left drawing origin
  bathy_raster <- flip(bathy_raster, direction = "vertical")

  plot(bathy_raster, main = paste("Downloaded Bathy Gap Layer", layer_id))
}


##### The Calculation Function #####

calculate_geodesic_area <- function(input_dir) {
  tif_files <- list.files(path = input_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

  if (length(tif_files) == 0) {
    cat("No .tif files found in the specified directory.\n")
    return(0)
  }

  total_area_km2 <- 0

  for (file in tif_files) {
    cat("Processing", file, "...\n")

    # Load the raster and suppress the initial metadata warning
    r <- suppressWarnings(rast(file))

    # The REST API strips spatial info, so we manually assign it here
    ext(r) <- c(-90, -75, 24, 35)
    crs(r) <- "EPSG:4326"

    # FIX: Flip the image vertically so the ellipsoidal math matches the correct latitudes
    r <- flip(r, direction = "vertical")

    # If the server returned a multi-band image (like RGB), we only need the first band to check for valid data
    r <- r[[1]]

    # Filter out the zeros (change to NA)
    r_valid <- ifel(r == 0, NA, r)

    # Calculate the total geodesic area directly
    area_calc <- expanse(r_valid, unit = "km")

    # Extract the 'area' column and sum it up (in case expanse returns multiple categories)
    raster_area_km2 <- sum(area_calc$area, na.rm = TRUE)
    total_area_km2 <- total_area_km2 + raster_area_km2
  }

  cat("--------------------------------------\n")
  cat("Total Area (km2):", total_area_km2, "\n")
  return(total_area_km2)
}


##### Execute the Calculation #####

# Run the function on the 'indata' directory
calculate_geodesic_area(download_dir)


##### Calculate Spatial Overlap #####

file_layer0 <- file.path(download_dir, "bathy_gap_layer0.tif")
file_layer7 <- file.path(download_dir, "bathy_gap_layer7.tif")

if (file.exists(file_layer0) && file.exists(file_layer7)) {

  # Load the rasters, suppressing the initial extent warnings
  r0 <- suppressWarnings(rast(file_layer0)[[1]])
  r7 <- suppressWarnings(rast(file_layer7)[[1]])

  # 1. Manually assign extent and CRS to Layer 0, then flip it
  ext(r0) <- c(-90, -75, 24, 35)
  crs(r0) <- "EPSG:4326"
  r0 <- flip(r0, direction = "vertical")

  # 2. Manually assign extent and CRS to Layer 7, then flip it
  ext(r7) <- c(-90, -75, 24, 35)
  crs(r7) <- "EPSG:4326"
  r7 <- flip(r7, direction = "vertical")

  # 3. Create an overlap mask
  overlap_raster <- ifel(r0 != 0 & r7 != 0, 1, NA)

  # 4. Calculate the geodesic area (no warnings this time!)
  overlap_calc <- expanse(overlap_raster, unit = "km")
  overlap_area_km2 <- sum(overlap_calc$area, na.rm = TRUE)

  cat("--------------------------------------\n")
  cat("Overlap Area between Layer 0 and Layer 7 (km2):", overlap_area_km2, "\n")

  plot(overlap_raster, main = "Spatial Overlap (Layer 0 & Layer 7)", col = "red", legend = FALSE)

} else {
  cat("Cannot calculate overlap: One or both raster files are missing.\n")
}
