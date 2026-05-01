##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20260409
## purpose:https://vlab.noaa.gov/redmine/issues/155196

##### parameters: manual input #####
filename <- 'dst_data_NOAA_NWFSC_Bottom_Trawl_Survey_155196' ## same as the this files name

##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(filename)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- gsub(".*_|\\.R$", "", filename)
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(googledrive)

##### authenticate with Google Drive #####
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### ***** NEW Original: 20260410-1_NOAA_NWFSC_Bottom_Trawl_Survey_155196_existing.csv ***** #####
##### load from Google Drive (manual: put in shareable URL to file) #####
url <- 'https://drive.google.com/file/d/1BsO4HQ9qlySGn3nH04MD2rkaDD2ixEbF/view?usp=drive_link'
file_id <- as_id(url)

drive_download(
  as_id(file_id),
  path = "indata/downloaded_data.zip",
  overwrite = TRUE
)

unzip("indata/downloaded_data.zip", exdir = "indata/extracted_data")

files <- list.files("indata/extracted_data")
print(files)

csv_path <- file.path("indata/extracted_data", files[grep("\\.csv$", files)][1])
sub <- read.csv(csv_path)

##### create taxonomy patch #####
source('code/dst_tool_taxonomy_patch_maker.R')

##### check #####
taxonomy_patch %>% filter(VernacularNameCategory == 'insufficient taxonomic resolution') %>% pull(ScientificName) %>% table()
taxonomy_patch %>% filter(ScientificName == "Octocorallia") %>% pull(VernacularNameCategory) %>% table()

##### fix a few "insufficent taxonomic information" in VernacularNameCategory #####
taxonomy_patch <- taxonomy_patch %>%
  mutate(VernacularNameCategory = case_when(
    VerbatimScientificName == "Alcyonacea" & ScientificName == "Octocorallia" ~ "soft coral",
    VerbatimScientificName == "Calcaxonia" & ScientificName == "Scleralcyonacea" ~ "gorgonian coral",
    VerbatimScientificName == "Gorgonacea" & ScientificName == "Octocorallia" ~ "gorgonian coral",
    VerbatimScientificName == "Octocorallia" & ScientificName == "Octocorallia" ~ "octocorallia (unspecified)",
    TRUE ~ VernacularNameCategory  # This keeps all other values the same
  ))


##### write the patch #####
filename <- "20260410-1_NOAA_NWFSC_Bottom_Trawl_Survey_155196_existing_taxonomy_patch.csv"
write.csv(taxonomy_patch,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)


##### ***** NEW Original: 20260410-1_NOAA_NWFSC_Bottom_Trawl_Survey_155196_new.csv ***** #####
##### load from Google Drive (manual: put in shareable URL to file) #####
url <- 'https://drive.google.com/file/d/1_8mukenXn5y834H4Ax5DtVLY5l0oaKUa/view?usp=drive_link'
file_id <- as_id(url)

drive_download(
  as_id(file_id),
  path = "indata/downloaded_data.zip",
  overwrite = TRUE
)

unzip("indata/downloaded_data.zip", exdir = "indata/extracted_data")

files <- list.files("indata/extracted_data")
print(files)

csv_path <- file.path("indata/extracted_data", files[grep("\\.csv$", files)][2])
sub <- read.csv(csv_path)


##### create taxonomy patch #####
source('code/dst_tool_taxonomy_patch_maker.R')

##### fix a few "insufficent taxonomic information" in VernacularNameCategory #####
taxonomy_patch <- taxonomy_patch %>%
  mutate(VernacularNameCategory = case_when(
    VerbatimScientificName == "Alcyonacea" & ScientificName == "Octocorallia" ~ "soft coral",
    VerbatimScientificName == "Calcaxonia" & ScientificName == "Scleralcyonacea" ~ "gorgonian coral",
    VerbatimScientificName == "Gorgonacea" & ScientificName == "Octocorallia" ~ "gorgonian coral",
    VerbatimScientificName == "Octocorallia" & ScientificName == "Octocorallia" ~ "octocorallia (unspecified)",
    TRUE ~ VernacularNameCategory  # This keeps all other values the same
  ))

##### check #####
table(taxonomy_patch$VernacularNameCategory, useNA = 'always')

##### write the patch #####
filename <- "20260410-1_NOAA_NWFSC_Bottom_Trawl_Survey_155196_new_taxonomy_patch.csv"
write.csv(taxonomy_patch,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)






##### ***** Load New Original: 20260415-1_NOAA_NWFSC_Bottom_Trawl_Survey_155196_existing.csv ***** #####
## parameter: copy the shareable URL of the file you want to this spot
url <- 'https://drive.google.com/file/d/1BxizR_dEZNthuSiGsnuK39p8hNQT5jm8/view?usp=drive_link'

file_id <- as_id(url)

drive_download(
  as_id(file_id),
  path = "indata/downloaded_data.zip",
  overwrite = TRUE
)

## clear the extraction directory
extract_dir <- "indata/extracted_data"

## if the directory exists, delete it and everything inside it
if (dir.exists(extract_dir)) {
  unlink(extract_dir, recursive = TRUE)
}

## recreate the directory so it is completely empty
dir.create(extract_dir, recursive = TRUE)

unzip("indata/downloaded_data.zip", exdir = extract_dir)

files <- list.files(extract_dir)
print(files)

csv_path <- file.path(extract_dir, files[grep("\\.csv$", files)][1])
sub <- read.csv(csv_path)
dim(sub)
unique(sub$DatasetID)
unique(sub$SurveyID)
unique(sub$AccessionID)












##### run QA report #####
## set parameters
fileversion <- '20260415-1'
markdownfile <- "code/20250401-0_rmd_accession_qa_dashboard.Rmd"

## create filename by concatenation
filename <- paste0(fileversion,'_',unique(sub$AccessionID))

## render
rmarkdown::render(markdownfile,
                  output_file =  paste(filename,".docx", sep=''),
                  output_dir = 'C:/rworking/deepseatools/reports')

##### load QA report to Google Drive #####
## set parameters
folderurl <- "https://drive.google.com/drive/folders/1CBrFuYS6jCGMdiYRWXRyRKmi-rrKMyGS"

setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".docx", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".docx", sep=''),
             overwrite = T)

##### manual map check #####
library(leaflet)
library(sf)
library(dplyr)

# 1. Update the grouping to include ScientificName and DepthInMeters
# so they are available in the 'points' object.
x <- sub %>%
  filter(ScientificName == 'Pennatuloidea') %>%
  group_by(CatalogNumber, ScientificName, DepthInMeters, Latitude, Longitude, FlagReason, gisCRMDepth) %>%
  summarize(n = n(), .groups = "drop")

points <- st_as_sf(x, coords = c("Longitude", "Latitude"), crs = 4326)

# 2. Create the map with a multi-line popup
leaflet(data = points) %>%
  addProviderTiles("Esri.OceanBasemap") %>%
  addCircleMarkers(
    radius = 4,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.7,
    # Use paste0 to combine labels and data with HTML line breaks (<br/>)
    popup = ~paste0("<b>Catalog:</b> ", CatalogNumber, "<br/>",
                    "<b>Species:</b> ", ScientificName, "<br/>",
                    "<b>CRM_Depth:</b> ", gisCRMDepth, "<br/>",
                    "<b>Depth:</b> ", DepthInMeters, " m")
  )



