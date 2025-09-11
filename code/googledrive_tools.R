##### Header #####
# Author: Robert McGuinn
# Email: rpm@alumni.duke.edu
# Purpose: Interacting with Google Drive using R
##### packages #####
library(tidyverse)
library(googledrive)
library(googlesheets4)

##### download Google Sheet version of schema for use in R  documents #####
# Register and download Google Sheet
## MANUAL: double check this id is the latest one
s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
schema<- gs_read(s)
#gs_browse(s)

# write out to CSV
setwd("C:/rworking/digs/indata")
write.csv(schema, "2018_DSCRTP_Schema.csv") # manual name change

##### reading in sheet googlesheets4 #####
s <- read_sheet('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')

##### help and documentation for googlesheets. #####
##### connecting and listing
# list files in drive
x <- drive_find(n_max = 50)
View(x)

# list files in drive and browse to them
drive_find(n_max = 2) %>% drive_browse()

# as.id function just extracts that ID of the file(s)
x <- drive_find(n_max = 5)
y <- as_id(x)
class(y)
y

# getting an object from Google Drive by name
x <- drive_get("2020_DSCRTP_National_Database_Schema")
1
as_id(x)
x

# this folder on Google Drive also has a drive ID
# big folder
x <- drive_get("photos")
as_id(x)
#[1] "0B6wAjbIPoNo3Q2c5Y2F4eW1XQ2M"

# get the same ID from URL
as_id("https://docs.google.com/spreadsheets/d/1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI/edit#gid=450007262")
#[1] "0B6wAjbIPoNo3Q2c5Y2F4eW1XQ2M"]

# create the dribble object
x <- as_dribble(as_id("https://docs.google.com/spreadsheets/d/1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI/edit#gid=450007262"))
View(x)
as_id(x)

# create dribble by using name
x <- as_dribble("photos")
View(x)
as_id(x)

# specify dribble using by path
x <- as_dribble("photos/Portugal2015/")
View(x)
as_id(x)

# using google sheets
install.packages("googlesheets")
library(googlesheets)

#list your google sheets
(my_sheets <- gs_ls())

# copy a google sheets to your Google Drive
?gs_gap()
gs_gap() %>%
  gs_copy(to = "Gapminder")

# register your google sheet
gap <- gs_title("Gapminder")
#> Sheet successfully identified: "Gapminder"

# Need to access a sheet you do not own?
# Access it by key if you know it!
(GAP_KEY <- gs_gap_key())
#> [1] "1BzfL0kZUz1TsI5zxJF1WNF01IxvC67FbOJUiiGMZ_mQ"
third_party_gap <- GAP_KEY %>%
  gs_key()
#> Sheet successfully identified: "test-gs-gapminder"

# Need to access a sheet you do not own but you have a sharing link?
# Access it by URL!
(GAP_URL <- gs_gap_url())
#> [1] "https://docs.google.com/spreadsheets/d/1BzfL0kZUz1TsI5zxJF1WNF01IxvC67FbOJUiiGMZ_mQ/"
third_party_gap <- GAP_URL %>%
  gs_url()

# Worried that a spreadsheet's registration is out-of-date?
# Re-register it!
gap <- gap %>% gs_gs()
#> Sheet successfully identified: "Gapminder"

#gs_title(), gs_key(), gs_url(), and gs_gs() return a registered sheet as a googlesheet
#The utility function, extract_key_from_url(), helps you dig the key out of a browser URL. Registering via browser URL is fine, but registering by key is a better idea in the long-run.
#Use gs_browse() to visit the Sheet corresponding to a registered googlesheet in your browser. Optionally, you can specify the worksheet of interest.

gap %>% gs_browse()
gap %>% gs_browse(ws = 2)
gap %>% gs_browse(ws = "Europe")

# list the registration information
gap

# list the worksheet tabs
gs_ws_ls(gap)


#returns the worksheet as an R object
gs_read() returns the contents of a worksheet as a data frame.

oceania <- gap %>%
  gs_read(ws = "Oceania")

##### working with the package markdrive #####
#devtools::install_github("milesmcbain/markdrive")
library(markdrive)
markdrive::gdoc_checkout(filename = "markdrive")
getwd()

##### using gdoc to push to google drive #####
#devtools::install_github("ropenscilabs/gdoc")
library(gdoc)
setwd("C:/rworking/digs/code")
rmarkdown::render('20180806_0_Status_Update_RPMcGuinn.rmd', output_format=gdoc())

##### render to HTML and DOC to a direct location #####
#or just render to local (html and doc are controlled in YAML header)
setwd("C:/rworking/digs/code")
rmarkdown::render('20180806_0_Status_Update_RPMcGuinn.rmd')



##### **** folder listings on google drive ***** #####
##### authorizations #####
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### get folder by name #####
library(googledrive)

# Folder ID from your URL
folder_id <- "0AAbgBr_cP0q1Uk9PVA"

# Get the folder as a dribble
folder <- drive_get(as_id(folder_id))

# List the contents of the folder
drive_ls(folder)

# Find the sub-folder by name
sub_folder <- drive_ls(folder, q = "name = 'Project_Management_DSCRTP' and mimeType = 'application/vnd.google-apps.folder'")

# List contents if found
if (nrow(sub_folder) > 0) {
  drive_ls(sub_folder$id[1])
} else {
  message("Sub-folder not found.")
}



##### get a shared drive ID and list contents #####
id <- "0AAbgBr_cP0q1Uk9PVA"
shared_drive_id <- as_id(id)
drive_ls(path = shared_drive_id,
         corpus = "drive",
         shared_drive = shared_drive_id)

##### delete a folder #####
# Define the URL of the folder you want to delete
folder_url <- "https://drive.google.com/drive/folders/0AAbgBr_cP0q1Uk9PVA"

# Find the folder by its URL
# The as_id() function extracts the folder ID from the URL
folder_to_delete <- drive_get(as_id(folder_url))

# Use drive_trash() to move the folder to the trash
# The verbose = TRUE argument will print a message confirming the action
drive_trash(folder_to_delete, verbose = TRUE)

##### create the basic folder structure for the shared drive #####
target_folder <- drive_mkdir("Project_Management_DSCRTP", path = shared_drive_id)

target_folder <- drive_mkdir("Data_Inventory_and_Archive_DSCRTP", path = shared_drive_id)

target_folder <- drive_mkdir("Site_Characterization_Story_Map_DSCRTP", path = shared_drive_id)

target_folder <- drive_mkdir("Research_and_Custom Reporting_DSCRTP", path = shared_drive_id)
target_folderl2 <- drive_mkdir("Custom_Reporting_DSCRTP", path = target_folder)
target_folderl2 <- drive_mkdir("Research_DSCRTP", path = target_folder)

target_folder <- drive_mkdir("Portal_and_Geoplatform_DSCRTP", path = shared_drive_id)
target_folderl2 <- drive_mkdir("GIS_layers_DSCRTP", path = target_folder)

target_folder <- drive_mkdir("Outreach_Presentations_DSCRTP", path = shared_drive_id)

target_folder <- drive_mkdir("Field_Team_Data_Guidance_DSCRTP", path = shared_drive_id)

target_folder <- drive_mkdir("Backups_DSCRTP", path = shared_drive_id)


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

## delete a folder from shared drive
# folder <- drive_get("test2", shared_drive = shared_drive_id)
# drive_rm(folder)


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
folder <- drive_get("Custom_Analyses")
list_drive_tree(folder)

##### list folders as tree (sans files) #####

# Recursive folder tree printer including the root
list_drive_folders_tree <- function(folder = NULL, indent = 0) {
  # If folder is NULL, start from My Drive root
  if (is.null(folder)) {
    folder_name <- "My Drive"
    cat(strrep("  ", indent), "- ", folder_name, "\n", sep = "")
    contents <- drive_ls(path = NULL)
  } else {
    folder_name <- folder$name
    cat(strrep("  ", indent), "- ", folder_name, "\n", sep = "")
    contents <- drive_ls(path = folder)
  }

  # Filter only folders
  if (nrow(contents) > 0) {
    folders <- contents[grepl("application/vnd.google-apps.folder",
                              sapply(contents$drive_resource, `[[`, "mimeType")), ]
  } else {
    return()
  }

  # Recurse into each subfolder
  for (i in seq_len(nrow(folders))) {
    list_drive_folders_tree(folders[i, ], indent + 1)
  }
}


# Start from a specfic folder (My Drive)
folder <- drive_get("Custom_Analyses")
list_drive_folders_tree(folder)












##### ***** #####
##### move between my_drive and shared drive #####
id <- "1iCvj6RIC42lpOn1qQ4skK0jS41WOo6Am"
source_folder_id <- as_id(id)

## set target folder Id
id <- "1NZ52MBN7IQSW62Fb7zsp6zuHFo4e_ED2"
target_folder_id <- as_id(id)

# List all files in source folder
files_to_copy <- drive_ls(source_folder_id)

# Copy each file to the target folder
for (i in seq_len(nrow(files_to_copy))) {
  drive_cp(
    file = files_to_copy[i, ],
    path = target_folder_id
  )
}

