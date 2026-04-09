##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20260409
## purpose:https://vlab.noaa.gov/redmine/issues/155196

##### parameters: manual input #####
## add filename, should be the same name as this file
filename <- 'dst_data_NOAA_NWFSC_Bottom_Trawl_Survey_155196'

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
library(googledrive)

##### authenticate with Google Drive #####
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### ***** NEW Original: 20260409-0 ***** #####
##### load from Google Drive (manual: put in shareable URL to file) #####
file_url <- "https://drive.google.com/file/d/1lhkSKO0n6f95b6M5T6-cI8U00eca0cqw/view?usp=drive_link"
temp_file <- tempfile(fileext = ".csv")
drive_download(as_id(file_url), path = temp_file, overwrite = TRUE)
sub <- read_csv(temp_file)

##### strip off the CatalogNumbers #####
x <- sub %>% filter(is.na(sub$CatalogNumber) == F) %>% pull(CatalogNumber)
length(x)

y <- filt %>%
  filter(DatasetID == 'NOAA_NWFSC_Bottom_Trawl_Survey') %>%
  pull(CatalogNumber)

length(y)

length(y)-length(x)
length(setdiff(y,x))

setdiff(x, y)

woo <- filt %>%
  filter(DatasetID == 'NOAA_NWFSC_Bottom_Trawl_Survey')



