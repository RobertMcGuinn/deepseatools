##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20250416
## purpose: Pam Goddard and Lauri Sadorus wanted an extraction of data across the dateline.

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
library(robis)

##### authorizations #####
gs4_auth(email = "robert.mcguinn@noaa.gov")
drive_auth(email = "robert.mguinn@noaa.gov")
##### linkage #####
filename <- '20250416_custom_extract_AK_PGoddard_145085' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.Rmd', sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- str_extract(filename, "[^_]+$")
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### source ndb #####
# source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### check ####
print(unique(filt$DatabaseVersion))
print(table(filt$DatabaseVersion))
print(table(filt$FishCouncilRegion))

##### extract #####
sub <- filt %>%
  filter(FishCouncilRegion == 'North Pacific', as.Date(EntryDate) >= as.Date('2021-01-01'))

##### check #####
# sub %>% pull(gisMEOW) %>% unique()
# as.Date(sub$EntryDate)
# class(sub$EntryDate)
# table(sub$EntryDate, useNA = 'always')
# table(sub$DataProvider)
# sub %>% filter(DataProvider == 'Malecha, Pat | Rooper, Chris | Goddard, Pamela') %>% pull(EntryDate) %>% unique()

##### export result to csv (export to CSV) #####
filename <- "20250416_extraction_of_NDB_version_20250409-0_RPMcGuinn.csv"
write.csv(sub,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

##### add file to Google Drive Folder #####
##### upload CSV to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1cs8vL2mybnraoFpfLrt6eQ7NuFyL6Jp9"
setwd("C:/rworking/deepseatools/indata")
drive_upload(filename,
             path = as_id(folderurl),
             name = filename,
             overwrite = T)
