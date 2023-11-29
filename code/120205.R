##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20231129
## purpose: see redmine issue

##### linkage #####
## manual input here
filename <- '120205' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
## manual input here
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)

##### load data #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv('20230823_Clarke_RL-19-05.csv')
## exported from original Excel file to (CSV UTF-8)

##### explore #####
##### explore #####

dim(sub)
summary(sub)
names(sub)
table(sub$DataProvider)
table(sub$SurveyID, useNA = 'always')
table(sub$Vessel, useNA = 'always')
table(sub$EventID, useNA = 'always')
table(sub$ObservationDate, useNA = 'always')
table(sub$ScientificName, useNA = 'always')
table(is.na(sub$Latitude))
table(is.na(sub$Longitude))
table(is.na(sub$SampleID))
head(sub$SampleID)
head(sub$TrackingID)
table(is.na(sub$TrackingID))
table(is.na(sub$Condition))
filt %>%
  filter(grepl('Bingo', Reporter)) %>%
  pull(DatasetID) %>%
  table()
filt %>%
  filter(grepl('SH', DatasetID)) %>%
  pull(DatasetID) %>%
  table()




