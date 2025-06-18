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

##### markdrive help ######
# gdoc_checkout(filename = "GOT") Will search your Google drive for Google docs with "GOT" in the name and prompt to download one. After download it will be converted to .md for editing. Let's say the file that was downloaded was my_GOT_theory.docx, my_GOT_theory.md will be created in the working dir.
#
# gdoc_push(filname = "GOT") Will push a markdown file matching the name pattern back to Google drive and update the source document. You could also supply a dribble output from gdoc_checkout. It updates the google doc via a html conversion along the way.
#
# gdoc_render(filename = "./test.Rmd") will render an .md or .Rmd file to html and push it to Google Drive as a google doc. This package includes an Rstudio addin that will do this for the currently active source tab.
#
# Checkout to get the markdown file. Push to "save" your edits to Google drive.

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
# Get folder by name
folder <- drive_get("Team Coordination")
# List contents of the folder
drive_ls(folder)

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

## delete a folder from shared drive
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











