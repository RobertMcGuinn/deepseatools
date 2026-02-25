##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20260225
## purpose: inspecting feature service for missing geometry

##### parameters: manual input #####
## add filename, should be the same name as this file
filename <- 'dst_tool_feature_service_geom'

##### linkage #####
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- issuenumber <- sub(".*_(.*)$", "\\1", filename)
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(httr)
library(jsonlite)
library(tidyverse)

# 1. Set up the Base Query URL
base_url <- "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services/DSCRTP_NatDB/FeatureServer/0/query"

# 2. Get ALL Object IDs (Attribute-based: where 1=1)
print("Fetching all Object IDs...")
res_all <- GET(base_url, query = list(
  where = "1=1",
  returnIdsOnly = "true",
  f = "json"
))
ids_all <- content(res_all, as = "parsed")$objectIds

# 3. Get Object IDs using the Spatial Bounding Box
print("Fetching Spatial Object IDs...")
bbox <- '{"xmin": -180, "ymin": -90, "xmax": 180, "ymax": 90, "spatialReference": {"wkid": 4326}}'

res_spatial <- GET(base_url, query = list(
  where = "1=1",
  geometry = bbox,
  geometryType = "esriGeometryEnvelope",
  spatialRel = "esriSpatialRelIntersects",
  returnIdsOnly = "true",
  f = "json"
))
ids_spatial <- content(res_spatial, as = "parsed")$objectIds

# 4. Find the difference (The missing 400 records)
missing_ids <- setdiff(ids_all, ids_spatial)

print(paste("Number of missing records found:", length(missing_ids)))

# 5. Fetch the full attributes and geometry for ONLY the missing records
if(length(missing_ids) > 0) {
  print("Fetching data for missing records...")

  # Convert the missing IDs into a comma-separated string
  ids_string <- paste(missing_ids, collapse = ",")

  # Use POST instead of GET to avoid URL length limits
  res_missing_data <- POST(base_url, body = list(
    objectIds = ids_string,
    outFields = "*",
    returnGeometry = "true",
    f = "json"
  ), encode = "form") # ArcGIS REST APIs expect form-encoded POST bodies

  # Check to make sure we got a successful response before parsing
  if (status_code(res_missing_data) == 200) {
    # Parse the JSON response
    missing_records <- fromJSON(content(res_missing_data, as = "text", encoding = "UTF-8"))

    # Extract the attributes into a flat data frame
    missing_df <- missing_records$features$attributes

    print("Preview of missing records:")
    print(head(missing_df))

    # Optional: Save to CSV for external review
    # write.csv(missing_df, "missing_400_records.csv", row.names = FALSE)
  } else {
    print(paste("Request failed with status code:", status_code(res_missing_data)))
  }

} else {
  print("No missing records found. The counts match.")
}
.
