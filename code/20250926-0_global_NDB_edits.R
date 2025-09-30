##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250903
## purpose: global database edits to NCB version 20250926-0

##### linkage #####
filename <- '20250926-0_global_NDB_edits' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- '149675'
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
##### check #####
x <- filt %>% pull(DatasetID) %>% unique() %>% sort()

# Use grep to return the values (strings) that contain a space ('\\s')
all_strings_with_space <- grep("\\s", x, value = TRUE)
print(all_strings_with_space)

filt %>% pull(DataProvider) %>% unique() %>% sort()









