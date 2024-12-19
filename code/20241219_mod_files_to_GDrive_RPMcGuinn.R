##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20241219
## purpose: moving lots of files to Google Drive

##### linkage #####
filename <- '20241219_mod_files_to_GDrive_RPMcGuinn' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### packages #####
library(googledrive)

##### authorization ####
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### Collect File Paths Along with Folder Structure #####
## use list.files() to retrieve file paths with their relative structure:
## collect all files with relative paths
file_paths <- list.files(path = "c:/documents", full.names = TRUE, recursive = TRUE)

## collect relative paths to maintain folder structure
relative_paths <- list.files(path = "c:/documents", full.names = FALSE, recursive = TRUE)

##### Set root folder in Google Drive and get its ID #####
drive_root <- drive_mkdir("20241219-0_documents")
drive_root_id <- drive_root$id

##### Create subdirectories on Google Drive #####
unique_dirs <- unique(dirname(relative_paths))
unique_dirs <- unique_dirs[order(sapply(strsplit(unique_dirs, "/"), length))]

drive_folder_map <- list()
for (dir in unique_dirs) {
  if (dir == ".") {
    drive_folder_map[[dir]] <- drive_root_id
  } else {
    parent_folder <- dirname(dir)
    parent_id <- if (parent_folder == ".") {
      drive_root_id
    } else if (!is.null(drive_folder_map[[parent_folder]])) {
      drive_folder_map[[parent_folder]]
    } else {
      # Log missing parent and create it
      cat("Parent folder missing, creating:", parent_folder, "\n")
      parent_name <- basename(parent_folder)
      grandparent_folder <- dirname(parent_folder)
      grandparent_id <- ifelse(grandparent_folder == ".", drive_root_id, drive_folder_map[[grandparent_folder]])
      parent_drive_folder <- drive_mkdir(name = parent_name, path = as_id(grandparent_id))
      drive_folder_map[[parent_folder]] <- parent_drive_folder$id
      parent_id <- parent_drive_folder$id
    }

    # Create the current folder
    tryCatch({
      drive_subfolder <- drive_mkdir(name = basename(dir), path = as_id(parent_id))
      drive_folder_map[[dir]] <- drive_subfolder$id
    }, error = function(e) {
      cat("Error creating folder:", dir, "\n")
      print(e)
    })
  }
}

##### Upload Files to Their Corresponding Folders #####
## with the folder structure created, upload the files to their corresponding folders:
for (i in seq_along(file_paths)) {
  local_file <- file_paths[i]
  relative_folder <- dirname(relative_paths[i])
  drive_folder_id <- drive_folder_map[[relative_folder]]

  # Upload file
  drive_upload(media = local_file, path = as_id(drive_folder_id))
}

##### check #####
folder_id <- drive_root$id
# List contents of the folder
contents <- drive_ls(path = folder_id)
# View the contents
print(contents)


