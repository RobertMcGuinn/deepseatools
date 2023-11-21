##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose: see redmine issue

##### packages #####
library(tidyverse)
library(sf)
library(openxlsx)

##### redmine and github linkage #####
## manual input here
filename <- '20231116_noaavlab_redmine_121131.R' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
## manual input here
issuenumber <- '121131'
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)
##### import #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.xlsx('HURL_ARCHIVE_RECORDS_20230930_SBingo_corrected.xlsx')

##### recode Vessel name problem #####
recode_list <- list('Ka ªimikai O Kanaloa' = 'Kaʻimikai-O-Kanaloa')
sub$Vessel <- recode(sub$Vessel, !!!recode_list)
sub %>% filter(Vessel == 'Ka ªimikai O Kanaloa') %>% pull(Vessel)

##### explore #####
summary(sub)
names(sub)
table(sub$SurveyID, useNA = 'always')
table(sub$Vessel, useNA = 'always')
table(sub$EventID, useNA = 'always')
table(sub$ObservationDate, useNA = 'always')
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












