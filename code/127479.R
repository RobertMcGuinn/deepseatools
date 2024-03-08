##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240305
## purpose: Alaska density analysis with Arvind Shantharam
## issuename: 20240308_0_AK_Density_Analysis_AShantharam

##### linkage #####
## manual input here
filename <- '127479' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
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

##### load database #####
source('C:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### filter #####
filt %>%
  filter(is.na(Density) == F,
                Density != '-999',
                as.numeric(Longitude) > -154,
                as.numeric(Longitude) < -126) %>%
  View()












