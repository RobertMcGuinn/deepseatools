##### Header #####
# Author: Robert McGuinn
# Email: rpm@alumni.duke.edu
# Purpose: Folder listings Google Drive using R

##### packages #####
library(tidyverse)
library(googledrive)
library(googlesheets4)

##### **** folder listings on google drive ***** #####
##### authorizations #####
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### get list of folders by name (goes one deep)#####
# Get the folder by name
folder <- drive_get("DSCRTP_Data_Management_top")

# List only folders one level down
drive_listing <- drive_ls(folder) %>%
  filter(drive_resource[[1]]$mimeType == "application/vnd.google-apps.folder")

##### add the link to the google folder #####
drive_listing$link <- paste('https://drive.google.com/drive/folders/', drive_listing$id, sep='')

##### add the priority and who columns #####
drive_listing$priority <- ''
drive_listing$who <- ''

##### write the drive listing out to csv #####
drive_listing %>%
  select(name, priority, who, link) %>%
  write.csv("indata/20250729_drive_listing.csv")

##### write the drive listing to google drive #####
# Get the folder by name
drive_upload("indata/20250729_drive_listing.csv", path = folder, name = "20250729_drive_listing_folder_listing.csv", type = "spreadsheet")

##### list information in a shared drive #####
id <- "0AAbgBr_cP0q1Uk9PVA"
shared_drive_id <- as_id(id)
drive_ls(path = shared_drive_id,
         corpus = "drive",
         shared_drive = shared_drive_id)

##### copy between my_drive and shared drive #####
source_folder <- drive_get("testing_shared")

## set Shared Drive ID
id <- "0AAbgBr_cP0q1Uk9PVA"
shared_drive_id <- as_id(id)

## Create a folder and a 2nd level in the shared drive
target_folder <- drive_mkdir("test2", path = shared_drive_id)
target_folder_L2 <- drive_mkdir("test3", path = target_folder)

# List all files in source folder
files_to_copy <- drive_ls(source_folder)

# Copy each file to the target folder
for (i in seq_len(nrow(files_to_copy))) {
  drive_cp(
    file = files_to_copy[i, ],
    path = target_folder
  )
}

##### deleting from a folder within a particular shared drive #####
## set Shared Drive ID
id <- "0AAbgBr_cP0q1Uk9PVA"
shared_drive_id <- as_id(id)

folder <- drive_get("test2", shared_drive = shared_drive_id)
drive_rm(folder)

##### list Google folders as a tree (with files) #####
# Function to check if a file is a folder
is_folder <- function(file) {
  file$drive_resource[[1]]$mimeType == "application/vnd.google-apps.folder"
}

# Recursive function to print folder tree (includes root)
list_drive_tree <- function(folder = NULL, indent = 0, root_shown = FALSE) {
  # Show root name on first call
  if (!root_shown) {
    root_name <- if (is.null(folder)) "My Drive" else folder$name
    cat(strrep("  ", indent), "- ", root_name, "\n", sep = "")
    root_shown <- TRUE
  }

  # List contents of the current folder
  contents <- drive_ls(path = folder)

  # If empty, stop here
  if (nrow(contents) == 0) return()

  # Loop through contents
  for (i in seq_len(nrow(contents))) {
    item <- contents[i, ]
    is_folder_item <- item$drive_resource[[1]]$mimeType == "application/vnd.google-apps.folder"

    # Print name with indentation
    cat(strrep("  ", indent + 1), "- ", item$name, "\n", sep = "")

    # If it's a folder, recurse
    if (is_folder_item) {
      list_drive_tree(item, indent + 1, root_shown = TRUE)
    }
  }
}

# Start from a specfic folder (My Drive)
folder <- drive_get("DSCRTP_Data_Management_top")
list_drive_tree(folder)

##### list Google folders as a tree (sans files) #####

# Recursive folder tree printer including the root
list_drive_folders_one_level <- function(folder = NULL) {
  # Define the folder name and list contents
  if (is.null(folder)) {
    cat("- My Drive\n")
    contents <- drive_ls(path = NULL)
  } else {
    cat("- ", folder$name, "\n", sep = "")
    contents <- drive_ls(path = folder)
  }

  # Filter to folders only
  if (nrow(contents) > 0) {
    folders <- contents[grepl(
      "application/vnd.google-apps.folder",
      sapply(contents$drive_resource, `[[`, "mimeType")
    )]
  } else {
    folders <- contents[0, ]  # Return empty
  }

  # Print just 1 level of folder names
  if (nrow(folders) > 0) {
    for (i in seq_len(nrow(folders))) {
      cat("  - ", folders$name[i], "\n", sep = "")
    }
  } else {
    cat("  (no subfolders)\n")
  }

  # Return folder metadata (optional)
  invisible(folders)
}



# Start from a specfic folder (My Drive)
folder <- drive_get("DSCRTP_Data_Management_top")
list_drive_folders_tree(folder)











