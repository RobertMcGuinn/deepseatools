##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250723
## purpose: runner plus parameters for the RMarkdown file called
## dst_report_for_database_update.RMD

##### packages #####
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)

##### load NDB ####
##### load the current version of the National Database #####
## creates object called 'filt'
# source('C:/rworking/deepseatools/code/dst_load_current_ndb.R')

##### parameters #####
quarter     <- "Q1, FY-2026"
version     <- as.character(unique(filt$DatabaseVersion))
releasedate <- "2026-01-27"
corrections <- 0
filename <- paste0("dst_report_database_update_", version)
last_db <- 'DSCRTP_NatDB_20251001-0.csv'
this_db <- 'DSCRTP_NatDB_20260121-0.csv'
newdatasetIDs <- c("OET_NA165")
folderurl <- "https://drive.google.com/drive/folders/1KPK1YI-n7EHNuOIKfZJM_EsaDCAUQOl8"

##### linkage #####
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- issuenumber <- sub(".*_(.*)$", "\\1", filename)
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### render #####
rmarkdown::render(
  "code/dst_report_database_update.rmd",
  params = list(
    quarter     = quarter,
    version     = version,
    releasedate = releasedate,
    corrections = corrections,
    filename = filename,
    last_db = last_db,
    this_db = this_db,
    newdatasetIDs = newdatasetIDs,
    folderurl = folderurl
  ),
  output_file = paste0(filename, ".docx"),
  output_dir = "reports/"
)

##### MANUAL: inspection of QA report in Word #####
##### OPTIONAL: load PDF report to specific folder on Google Drive #####
## manual: inspect report for any changes/errors
## manual: SAVE to PDF.
## manual: then Develop Redmine Checklist
## upload the PDF to Google Drive
# drive_upload(paste("reports/", filename,".PDF", sep=''),
#              path = as_id(folderurl),
#              name = paste(filename,".PDF", sep=''),
#              overwrite = T)

##### checking #####
# yo <- filt %>% filter(AphiaID == -999)
# length(yo$CatalogNumber)
# unique(yo$ScientificName)
# unique(yo$DatasetID)
# unique(yo$SurveyID)
# unique(sub$Citation)
# unique(sub$Repository)
# filt %>% filter(grepl("Brooke", PI)) %>% pull(ObservationYear) %>% unique()
# filt %>% filter(AphiaID == -999) %>% pull(ScientificName) %>% table(useNA = 'always')
# filt %>% filter(AphiaID == '-999') %>% pull(Class) %>% table(useNA = 'always')
# filt %>% filter(ScientificName == 'Haliclona (Reniera)') %>% pull(AphiaID)
# filt %>% filter(ScientificName == 'Flabellum (Flabellum) oclairi') %>% pull(AphiaID)


