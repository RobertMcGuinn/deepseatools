##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240412
## purpose: working on volumes from inventory

##### linkage #####
filename <- '128852' ## manual: for this code file name, match to redmine
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
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### read in the data inventory sheet from google drive #####
mpdi <- read.csv('c:/rworking/deepseatools/indata/20240412-0_Master Project Deliverable Inventory_DeliverableStatus_AShantharam.csv')

##### check #####
names(mpdi)

##### summarize project inventory #####
summary <- mpdi %>%
  filter(is.na(RawVideoVolume) == F |
           is.na(RawStillImageVolume) == F) %>%
  group_by(ProjectID, Vessel, Vehicle, Year) %>%
  summarize(RawVideoVolume = paste(unique(RawVideoVolume), collapse  = ' | '),
            RawStillImageVolume = paste(unique(RawStillImageVolume), collapse = ' | '))

View(summary)

##### write the result to csv #####
summary %>%
  write.csv('c:/rworking/deepseatools/indata/20240412-0_summary_of_video_and_still_image_volume_RPMcGuinn.csv',
  row.names = F, )


##### database summary #####
summary_NDB <- filt %>%
  group_by(FishCouncilRegion) %>%
  summarize(RecordType = paste(unique(RecordType), collapse = ' | '),
            SamplingEquipment = paste(unique(SamplingEquipment), collapse = ' | '),
            Vessel = paste(unique(Vessel), collapse = ' | '),
            VehicleName = paste(unique(VehicleName), collapse  = ' | '))

View(summary_NDB)

summary_NDB %>%
  write.csv('c:/rworking/deepseatools/indata/20240412-0_summary_of_platforms_by_region_RPMcGuinn.csv',
            row.names = F, )











