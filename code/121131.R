##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20231128
## purpose:

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)

##### linkage #####
## manual input here
filename <- '121131' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### load data #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv('20231129-0_HURL_archive_records_SBingo_1972_2013_121131.csv')
## exported from original Excel file to (CSV UTF-8)

##### explore #####
dim(sub)
summary(sub)
names(sub)
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
  filter(grepl('OET_NA134', DatasetID)) %>%
  pull(TrackingID)

dim(sub)
length(setdiff(sub$SampleID, filt$SampleID))
length(setdiff(sub$TrackingID, filt$TrackingID))


length(unique(sub$SampleID))
length(unique(sub$TrackingID))
length(setdiff(unique(sub$SampleID), unique(filt$SampleID)))
filt %>% filter(SampleID %in% sub$SampleID) %>% pull(SampleID) %>% length()
filt %>%
  filter(SurveyID %in% sub$SurveyID) %>%
  pull(SurveyID) %>% unique()






##### taxonomic merge #####
