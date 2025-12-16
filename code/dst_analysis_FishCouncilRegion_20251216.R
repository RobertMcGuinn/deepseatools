##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20251216
## purpose: FishCouncilRegion summary for Heather Coleman.

##### parameters: manual input #####
## add filename, should be the same name as this file
filename <- 'dst_analysis_FishCouncilRegion_20251216'

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
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)
library(robis)

##### load database #####
source('code/load_current_ndb.r')

##### FishCouncilRegion Summary #####
filt %>%
  filter(as.Date(EntryDate) > as.Date('2021-10-01') &
           as.Date(EntryDate) < as.Date('2025-10-01')) %>%
  pull(FishCouncilRegion) %>% table(useNA = 'always')

##### check #####






