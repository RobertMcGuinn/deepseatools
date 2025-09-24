##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250924
## purpose: custom extract for Gulf of Alaska, Pam Goddard and Kresimir
## email: https://mail.google.com/mail/u/0/#all/DRGVkzgsjnQVbgdVplNjWsKsLbqdGqCGFmGdZvPq

##### linkage #####
filename <- '20250924-0_custom_extract_Kresimir' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.Rmd', sep = '')
# browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

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
library(robis)
library(leaflet)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### extract data subset of interest #####
x <- filt %>%
  filter(ObservationYear == '2019',
         FishCouncilRegion == 'North Pacific')


##### map it #####
##### map it data using leaflet #####
#install.packages("leaflet")
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=2,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("ScientificName:", x$ScientificName, "<br>",
                                    "SurveyID:", x$SurveyID, "<br>",
                                    "DatasetID:", x$DatasetID, "<br>",
                                    "ImageURL:", x$ImageURL, "<br>",
                                    "PI:", x$PI, "<br>",
                                    "Observation Year:", x$ObservationYear))
m

##### make summary table #####

x %>% select(DatasetID, SurveyID, PI, ObservationYear) %>% unique()












