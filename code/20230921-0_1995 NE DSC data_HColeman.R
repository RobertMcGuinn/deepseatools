##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose: see redmine issue

##### linkage #####
## manual input here
filename <- '20230921-0_1995 NE DSC data_HColeman.R' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
## manual input here
issuenumber <- '120781'
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)







##### import database #####
source("C:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### import dataset #####
setwd('c:/rworking/deepseatools/indata')
filename <- '20230921-0_1995 NE DSC data_HColeman.csv'
sub <- read.csv(filename, header = T)

##### check #####
x <- filt %>% filter(grepl("Edwin", Vessel), ObservationYear == '1995') %>%
  group_by(Vessel, VehicleName, SurveyID,  ObservationYear, ObservationDate, PI, EventID, Latitude, Longitude) %>%
  summarize(n=n())
View(x)

x <- filt %>% filter(grepl("JSL-II", EventID)) %>%
  group_by(ScientificName, Vessel, VehicleName, DatasetID, SurveyID, ObservationYear, ObservationDate, PI, EventID, Latitude, Longitude) %>%
  summarize(n=n())
View(x)

View(sub)
setdiff(sub$System.Dive.No., )

x <- filt %>% filter(DatasetID == 'NURC') %>%
  group_by(ScientificName, Vessel, VehicleName, DatasetID, SurveyID, ObservationYear, ObservationDate, PI, EventID, Latitude, Longitude) %>%
  summarize(n=n())
View(x)




