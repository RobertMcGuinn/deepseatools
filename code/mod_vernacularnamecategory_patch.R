##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240103
## purpose: crosswalk between AphiaIDs and VernacularNameCategory

##### linkage #####
## manual input here
filename <- 'mod_vernacularnamecategory_patch.R' ## entry: name of the current file.  make sure to include '.R'
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
browseURL(github_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)

##### grab all AphiaIDs #####
aphiaIDs <- unique(filt$AphiaID)

##### create a crosswalk
crosswalk <- filt %>%
  group_by(AphiaID, VernacularNameCategory) %>%
  summarize(n=n())

##### check #####
length(aphiaIDs)
dim(crosswalk)
length(aphiaIDs) - dim(crosswalk)
names(crosswalk)
class(crosswalk$n)






