##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20260727
## purpose: Analysis of the 'Bathy Gap Analysis' product for different subsets.

##### parameters #####
##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(current_file)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')

##### packages #####
library(terra)

##### parameters #####

# Define paths to your local data
boundary_file <- "indata/resafmcboundary/SAFMC_jursdction_bndry_po.shp" ##### as shapefile
raster_dir <- "indata/coverage_package"
epsg_code <- "4326"

##### 1. Load and Prep the Clipping Boundary #####

if (!file.exists(boundary_file)) {
  stop(paste("Cannot find the boundary file:", boundary_file))
}

cat("Loading SAFMC boundary shapefile...\n")
clip_poly <- vect(boundary_file)

# Safely handle missing .prj files or reproject if necessary
if (crs(clip_poly) == "") {
  cat("Notice: Shapefile is missing a .prj file. Manually assigning WGS84...\n")
  crs(clip_poly) <- paste0("EPSG:", epsg_code)
} else if (crs(clip_poly, describe = TRUE)$code != epsg_code) {
  cat("Projecting shapefile to WGS84...\n")
  clip_poly <- project(clip_poly, paste0("EPSG:", epsg_code))
}

##### 2. The Processing Function #####

# Added 'target_value' to allow filtering for specific classes (like 63 for lidar)
process_local_footprint <- function(raster_folder, boundary_polygon, target_value = NULL) {

  tif_files <- list.files(path = raster_folder, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

  if (length(tif_files) == 0) {
    cat("No .tif files found in", raster_folder, "\n")
    return(NULL)
  }

  label_text <- ifelse(is.null(target_value), "ALL valid data", paste("Class", target_value))
  cat("\n======================================\n")
  cat("Processing for:", label_text, "\n")
  cat("Found", length(tif_files), "raster files.\n")

  processed_rasters <- list()

  for (file in tif_files) {

    r <- rast(file)

    # Ensure raster matches the WGS84 boundary
    if (crs(r, describe = TRUE)$code != epsg_code) {
      r <- project(r, paste0("EPSG:", epsg_code))
    }

    # Crop and mask to the boundary (Skip if it falls outside)
    r_clipped <- tryCatch({
      crop(r, boundary_polygon, mask = TRUE)
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(r_clipped)) {
      next
    }

    # FILTER LOGIC
    if (is.null(target_value)) {
      # Keep anything greater than 0 (Standard total footprint)
      r_footprint <- ifel(r_clipped > 0, 1, NA)
    } else {
      # Strict filter: ONLY keep pixels that match the target_value exactly
      r_footprint <- ifel(r_clipped == target_value, 1, NA)
    }

    # Only add to our list if the raster actually contains our target data
    # (minmax will return Inf/-Inf if the raster is entirely NA)
    mm <- minmax(r_footprint, compute = TRUE)
    if (!is.infinite(mm[1])) {
      processed_rasters[[file]] <- r_footprint
    }
  }

  if (length(processed_rasters) == 0) {
    cat("No matching data found inside the boundary for this criteria.\n")
    return(NULL)
  }

  ##### 3. Merge and Calculate #####
  cat("Merging footprint layers...\n")
  r_collection <- sprc(processed_rasters)

  # Merge flattens overlapping 1s into a single 1 (no double counting)
  final_footprint <- merge(r_collection)

  cat("Calculating Geodesic Area...\n")
  footprint_calc <- expanse(final_footprint, unit = "km")
  footprint_area_km2 <- sum(footprint_calc$area, na.rm = TRUE)

  poly_area_km2 <- sum(expanse(boundary_polygon, unit = "km"))

  cat("--------------------------------------\n")
  cat("Total SAFMC Boundary Area (km2):", poly_area_km2, "\n")
  cat("Total Mapped Footprint (km2):", footprint_area_km2, "\n")
  cat("Percent Mapped:", round((footprint_area_km2 / poly_area_km2) * 100, 2), "%\n")

  # Visual Verification
  plot(boundary_polygon, main = paste("Footprint within SAFMC:", label_text), axes = FALSE)
  plot(final_footprint, col = "red", add = TRUE, legend = FALSE)

  return(list(
    footprint_area = footprint_area_km2,
    boundary_area = poly_area_km2,
    raster_mask = final_footprint
  ))
}

##### 3. Execute the Calculations #####

# Step 1: Calculate the footprint for ALL data
results_all <- process_local_footprint(raster_dir, clip_poly, target_value = NULL)

# Step 2: Calculate the footprint for ONLY LiDAR (Class 63)
results_focused <- process_local_footprint(raster_dir, clip_poly, target_value = 63)
