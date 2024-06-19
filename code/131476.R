##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240613
## purpose: oe_collab: Ashley M.: NDSF assets funded through DSCRTP
## https://vlab.noaa.gov/redmine/issues/131476

##### linkage #####
filename <- '131476' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")
unique(filt$DatabaseVersion)

##### filter #####
x <- filt %>% filter(grepl('Atlantis', Vessel) |
                       grepl('Alvin', VehicleName) |
                       grepl('Jason', VehicleName) |
                       grepl('Sentry', VehicleName),
                     grepl('NOAA', DatasetID) |
                       grepl('BOEM', DatasetID) |
                       grepl('WHOI', DatasetID))  %>%
  group_by(ObservationYear,
           DatasetID,
           SurveyID,
           Vessel,
           VehicleName,
           RecordType) %>%
  summarize(n=n()) %>%
  arrange(ObservationYear)

View(x)

##### write result #####
write_csv(x, 'c:/rworking/deepseatools/indata/20240614-0_WHOI_obs_in_NDB_for_Ashley_M_RPMcGuinn.csv')

github_link







