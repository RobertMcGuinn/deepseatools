##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250818
## purpose:

##### linkage #####
filename <- '20250726-0_testing_feature_service' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)

redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- '148687'
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)

##### options #####
digits = 121

##### load old NDB  (manual) #####
filename <- 'DSCRTP_NaDB_20250409-0.csv'
path <- paste0("C:/rworking/deepseatools/indata/", filename)
filt_old <- read.csv(path, header = T, encoding = 'latin1')

##### load new NDB (clean for GIS) #####
filename <- 'DSCRTP_NatDB_20250714-0_clean.csv'
path <- paste0("C:/rworking/deepseatools/indata/", filename)
filt_clean <- read.csv(path, header = T, encoding = 'latin1')

##### load data from feature service #####
## this script produces 'fs_table'
source('code/mod_load_data_from_feature_service.R')

##### check #####
#dim(filt_old)
dim(filt_clean)
dim(fs_table)
