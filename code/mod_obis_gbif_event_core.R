##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose:

##### linkage #####
filename <- 'mod_obis_gbif_event_core' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### packages #####
library(tidyverse)

##### events #####
filt %>% group_by(Vessel, SurveyID, EventID, ObservationDate) %>%
  summarize(n=n()) %>% View()
