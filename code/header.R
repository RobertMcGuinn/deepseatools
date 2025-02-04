##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20250203
## purpose:

##### linkage #####
filename <- '20250203_metrics' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

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

##### metrics #####
## 'NOAA's National Database for Deep-sea Corals and Sponges' (ARCHIVE VERSION)
## https://noaa.maps.arcgis.com/home/item.html?id=b664c55c156b4b7ea9e03d72eacfee74
## Item created: Aug 10, 2022 Item updated: Feb 3, 2025 View count: 65,876 [snapshot on 2025-02-03]

## 'NOAA's National Database for Deep-sea Corals and Sponges' (Current Version)
## https://noaa.maps.arcgis.com/home/item.html?id=aabf6845f3ed47eca8a5a1d538aec7c5
## Item created: Nov 9, 2024 Item updated: Feb 3, 2025 View count: 230 [snapshot on 2025-02-03]

## Characterizing U.S. Deep-Sea Corals and Sponges
## https://noaa.maps.arcgis.com/home/item.html?id=cf7a5b4691e54ae59e1573d594a421c4
## Item created: Oct 12, 2022 Item updated: Aug 23, 2024 View count: 2,409 [snapshot on 2025-02-03]







