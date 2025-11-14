##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20251114
## purpose: see https://vlab.noaa.gov/redmine/issues/152142

##### linkage #####
filename <- '20251114-0_Oceana_Support_152142' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- sub(".*_(.*)$", "\\1", filename)
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
library(robis)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### check #####
## look where IndividualCount is greater than 1000
filt %>% filter(as.numeric(IndividualCount)>1000)%>%
  group_by(DatasetID, RecordType, SamplingEquipment, IndividualCount) %>%
  summarize(n=n()) %>% View()

## look at IndividualCount summary within a given DatasetID
filt %>% filter(DatasetID == "NOAA_AFSC_GOA_Coral_Survey_2022") %>%
  group_by(DatasetID, RecordType, SamplingEquipment) %>%
  summarize(
    min_IndividualCount = min(IndividualCount),
    max_IndividualCount = max(IndividualCount),
    n=n()) %>% View()

## explore transect start and endpoint data
filt %>% filter(DatasetID == "NOAA_AFSC_GOA_Coral_Survey_2022") %>%
  group_by(StartLatitude, StartLongitude, EndLatitude, EndLongitude) %>%
  summarize(n=n()) %>% View()

## explore density values for a particular DatasetID.
filt %>% filter(DatasetID == "NOAA_AFSC_GOA_Coral_Survey_2022") %>%
  group_by(Density,IndividualCount, SampleAreaInSquareMeters) %>%
  summarize(n=n()) %>% View()













