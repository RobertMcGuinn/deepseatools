##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250808
## purpose: data ingest: OET-NA168

##### manual settings #####
filename <- '148433' ## manual: for this code file name, match to redmine
name <- 'NA156_DARC_Annotations_Full_20250801.tsv'

##### linkage #####
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- pas3e(github_path, filename, '.R', sep = '')
browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
#browseURL(redmine_link)

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

##### load ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### load dataset #####
df <- read_tsv(paste0('indata/', name))










