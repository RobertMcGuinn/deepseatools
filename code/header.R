##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose:

##### linkage #####
filename <- 'mod_load_current_ndb' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
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
library(robis)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")








