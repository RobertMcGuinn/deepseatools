##### Header #####
## projectname: 20230329_BSEE_Alaska_deep_ESI_RPMcGuinn
## author: Robert McGuinn
## date started:
## forkedfrom: none
## drive:https://drive.google.com/drive/folders/1BSXMVD5khNni1lvwKaT8posa71HwpUJ7?usp=share_link
## purpose: aoi analysis for RPI ESI work for BSEE


##### packages #####
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(sf)

##### authorizations #####
# Set authentication token to be stored in a folder called \.secrets``
options(gargle_oauth_cache = ".secrets")

# Authenticate manually
gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:
list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### load national database (local) #####
setwd("C:/rworking/deepseatools/indata")
filename <- "DSCRTP_NatDB_20221213-0.csv"
indata<-read_csv(filename,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = 'latin9'),
                 na = c("-999", "NA"))

filt <- indata %>%
  filter(Flag == 0)

##### clear all objects besides national database #####
rm(list = setdiff(ls(), "filt"))

##### load data subsets of interest #####
## next file
path <- "C:/rworking/deepseatools/indata/"
filename <- ""
string <- paste(path,filename,".csv",sep = '')
sub <- read_csv(string)
flagged <- sub %>%  filter(Flag == "1")

## next file
path <- "C:/rworking/deepseatools/indata/"
filename <- ""
string <- paste(path,filename,".csv",sep = '')
sub2 <- read_csv(string)
flagged <- sub %>%  filter(Flag == "1")
