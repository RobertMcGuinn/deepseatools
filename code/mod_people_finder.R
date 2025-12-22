##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: apply a filter for a person and summarize

##### packages #####
library(tidyverse)
library(DT)
library(tidyverse)
library(googlesheets4)
library(googledrive)

##### linkage #####
filename <- 'mod_people_finder' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)

##### parameters #####
person <- 'Hourigan'

##### authentication #####
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### source load NDB (creates filt) #####
# source('c:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### filter and summarize #####
## prepare data with formulas as plain strings
tab <- filt %>%
  filter(grepl(person, PIAffiliation) |
           grepl(person, PI) |
           grepl(person, DataContact) |
           grepl(person, Reporter) |
           grepl(person, IdentifiedBy)) %>%
  mutate(DashLink = paste0('https://www.ncei.noaa.gov/waf/dsc-data/dashboards/',
                           DatasetID, '.html')) %>%
  group_by(DatasetID, DashLink) %>%
  summarize(n = n(),
            DataProvider = paste(unique(DataProvider), collapse = " | "),
            Vessel = paste(unique(Vessel), collapse = " | "),
            ObservationYear = paste(unique(ObservationYear), collapse = " | "),
            PI = paste(unique(PI), collapse = " | "),
            DataContact = paste(unique(DataContact), collapse = " | "),
            Reporter = paste(unique(Reporter), collapse = " | "),
            IdentifiedBy = paste(unique(IdentifiedBy), collapse = " | "),
            .groups = "drop") %>%
  mutate(DatasetID = gs4_formula(
    paste0("=HYPERLINK(\"", DashLink, "\", \"", DatasetID, "\")")
  )) %>%
  select(-DashLink)

##### add the summary file to google drive #####
## create empty sheet
gs_name <- paste0(Sys.Date(), "_", person,"_person_summary", "_NDB_version_", unique(filt$DatabaseVersion))
sheet <- gs4_create(name = gs_name)

## move to target folder
folder_name <- "deepseatools_reports"
folder <- drive_get(folder_name)
if (nrow(folder) == 0) stop("Folder not found in Google Drive")
drive_mv(file = sheet, path = as_id(folder$id))

## write data as USER_ENTERED so formulas work
sheet_write(
  data = tab,
  ss = sheet,
  sheet = "Sheet1"
)











