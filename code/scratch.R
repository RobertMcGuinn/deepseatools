##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20241010
## purpose: scratch pad

##### linkage #####
filename <- 'scratch' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
# # redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# # issuenumber <- filename
# # redmine_link <- paste(redmine_path, issuenumber, sep = '')
# # browseURL(redmine_link)

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

##### scratch #####
cats <-
  c(1579138,1579139,1579140,1579141,1579142,1579143,1579144,1579145,1579146)

filt %>% filter(CatalogNumber %in%  cats) %>%
  pull(FishCouncilRegion) %>%
  table(useNA = 'always')

filt %>% filter(ScientificName == 'Desmophyllum pertusum') %>%
  pull(VernacularNameCategory) %>%
  table(useNA = 'always')

filt %>% filter(ScientificName == 'Coenocyathus bowersi') %>%
  pull(VernacularNameCategory) %>%
  table(useNA = 'always')






