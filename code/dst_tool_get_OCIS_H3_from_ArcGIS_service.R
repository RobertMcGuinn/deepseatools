##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20260226
## purpose:

##### parameters: manual input #####
## add filename, should be the same name as this file
filename <- 'dst_tool_get_OCIS_H3_from_ArcGIS_service'

##### linkage #####
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- issuenumber <- sub(".*_(.*)$", "\\1", filename)
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(arcgislayers)
library(sf)
library(tidyverse)
library(googlesheets4)

##### define service URL #####
# Based on the discovery that H6 resides at ID 16 in this service
ocis_h6_url <- "https://services.arcgis.com/bDAhvQYMG4WL8O5o/arcgis/rest/services/ocis_sde_ocis_master_view_h6_view/FeatureServer/16"

##### ingest master mesh #####
message("##### SECTION 3: INGESTING H3 MASTER MESH #####")

# 1. Establish the connection to the specific H6 layer
# This 'handshake' validates that Layer 16 is accessible
h3_service <- arc_open(ocis_h6_url)

# 2. Pull the data into an 'sf' spatial object
# arc_select handles the server's 'Max Record Count' paging automatically
h3_mesh <- arc_select(h3_service)

##### validation #####
message("##### SECTION 4: VALIDATION #####")

# Verify we have the correct spatial object type
message(paste("Object Class:", class(h3_mesh)[1]))

# Verify the record count (The scale of the mesh)
message(paste("Success! Loaded", nrow(h3_mesh), "hexagons from the Master Mesh."))

# Check for the H3_INDEX column
if("grid_id" %in% colnames(h3_mesh)) {
  message("grid_id column successfully identified.")
} else {
  message("Warning: grid_id column name might be different. Available columns:")
  print(colnames(h3_mesh))
}

##### quick plot to verify the hexagonal geometry #####
# plot(st_geometry(h3_mesh),
#      main = "OCIS Master Mesh: H3 Resolution 6",
#      col = "aliceblue",
#      border = "steelblue")

##### save to disk #####
# 1. SAVE to disk
# This creates a single file containing the entire sf object
saveRDS(h3_mesh, "indata/OCIS_H6_MasterMesh_Jan2026.rds")
message("H3 Mesh successfully cached to disk.")

##### reload later #####
# 2. RELOAD later (for a fresh R session)
# Use this code to bring the object back instantly
h3_mesh_reloaded <- readRDS("indata/OCIS_H6_MasterMesh_Jan2026.rds")

# Verify it's still an sf object
message(paste("Reloaded object count:", nrow(h3_mesh_reloaded)))

##### check #####
names(h3_mesh)
library(dplyr)
library(tidyr)


##### bringing in data dictionary # Install and load the package
# Since this is a public sheet, you can disable authentication
gs4_deauth()

# Define the URL
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSOF1LDFyQr-2KXt_WjvVDtMsnpSWNR5NmK-b4xcK3QeHipJ-sEDR8-KitBTmQV_TPPJ-t8gMFj-Jmf/pubhtml?gid=1160602972&single=true"

# Read the sheet
# Note: read_sheet often works better with the direct /edit URL,
# but for published sheets, Option 2 is often more reliable.
df <- read_sheet(url)#####
