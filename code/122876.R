##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20231201
## purpose: see redmine issue

##### linkage #####
## manual input here
filename <- '122876' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
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

##### load data #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv('20231122-2_National_Oceanography_Centre_James_Cook_JC073_2012.csv')

##### check #####

