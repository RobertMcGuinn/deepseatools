##### SECTION 1: LOAD LIBRARIES #####
# Ensure these are installed: install.packages(c("arcgislayers", "sf"))
library(arcgislayers)
library(sf)
library(tidyverse)

##### SECTION 2: DEFINE SERVICE URL #####
# Based on the discovery that H6 resides at ID 16 in this service
ocis_h6_url <- "https://services.arcgis.com/bDAhvQYMG4WL8O5o/arcgis/rest/services/ocis_sde_ocis_master_view_h6_view/FeatureServer/16"

##### SECTION 3: INGEST H3 MASTER MESH #####
message("##### SECTION 3: INGESTING H3 MASTER MESH #####")

# 1. Establish the connection to the specific H6 layer
# This 'handshake' validates that Layer 16 is accessible
h3_service <- arc_open(ocis_h6_url)

# 2. Pull the data into an 'sf' spatial object
# arc_select handles the server's 'Max Record Count' paging automatically
h3_mesh <- arc_select(h3_service)

##### SECTION 4: VALIDATION & VISUALIZATION #####
message("##### SECTION 4: VALIDATION #####")

# Verify we have the correct spatial object type
message(paste("Object Class:", class(h3_mesh)[1]))

# Verify the record count (The scale of the mesh)
message(paste("Success! Loaded", nrow(h3_mesh), "hexagons from the Master Mesh."))

# Check for the H3_INDEX column
if("H3_INDEX" %in% colnames(h3_mesh)) {
  message("H3_INDEX column successfully identified.")
} else {
  message("Warning: H3_INDEX column name might be different. Available columns:")
  print(colnames(h3_mesh))
}

# Generate a quick plot to verify the hexagonal geometry
plot(st_geometry(h3_mesh),
     main = "OCIS Master Mesh: H3 Resolution 6",
     col = "aliceblue",
     border = "steelblue")

##### SECTION 5: DATA SUMMARY #####
# View the first few rows of the attribute table
print(head(as.data.frame(h3_mesh)[, 1:min(3, ncol(h3_mesh))]))
##### SECTION 6: Save to disk #####
# 1. SAVE to disk
# This creates a single file containing the entire sf object
saveRDS(h3_mesh, "indata/OCIS_H6_MasterMesh_Jan2026.rds")
message("H3 Mesh successfully cached to disk.")

##### SECTION 7: Reload later #####
# 2. RELOAD later (for a fresh R session)
# Use this code to bring the object back instantly
h3_mesh_reloaded <- readRDS("indata/OCIS_H6_MasterMesh_Jan2026.rds")

# Verify it's still an sf object
message(paste("Reloaded object count:", nrow(h3_mesh_reloaded)))
##### check #####
names(h3_mesh)
library(dplyr)
library(tidyr)

##### pick through the variables #####

library(dplyr)
library(tidyr)
library(sf)

# 1. Strip geometry for the audit
df_flat <- h3_mesh_reloaded %>% st_drop_geometry()

# 2. Calculate NA Percentage
na_audit <- df_flat %>%
  summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_pct")

# 3. Calculate Unique Value Count
val_audit <- df_flat %>%
  summarise(across(everything(), ~n_distinct(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "unique_vals")

# 4. Join them into the final "Hit List"
audit_results <- left_join(na_audit, val_audit, by = "variable") %>%
  arrange(desc(na_pct))

# 5. Identify the Culls
cut_list <- audit_results %>%
  filter(na_pct > 80 | unique_vals <= 1) %>%
  pull(variable)

# 6. Create the Lean Mesh
h3_mesh_lean <- h3_mesh_reloaded %>%
  select(-all_of(cut_list))

# VERIFY
## View(audit_results)
print(paste("Original count:", ncol(h3_mesh_reloaded)))
print(paste("Marked for deletion:", length(cut_list)))
names(h3_mesh_lean)

##### go further #####
# A surgical strike to remove intermediate water column depths
# Keeping only Surface and Bottom for Temp and Salinity
h3_mesh_final_portal <- h3_mesh_lean %>%
  select(
    -matches("_(30|50|100|200|500)m")
  )

print(paste("Final Portal-Ready Variable Count:", ncol(h3_mesh_final_portal)))




names(h3_mesh_final_portal)



