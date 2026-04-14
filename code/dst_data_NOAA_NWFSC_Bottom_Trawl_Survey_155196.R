##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20260409
## purpose:https://vlab.noaa.gov/redmine/issues/155196

##### parameters: manual input #####
filename <- 'dst_data_NOAA_NWFSC_Bottom_Trawl_Survey_155196' ## same as the this files name

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





