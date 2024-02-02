##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose: see redmine issue

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)

##### linkage #####
## manual input here
filename <- '126062.R' ## for this  code, include .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues'
## manual input here
issuenumber <- '/126062'
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### load new national database (manual) #####
path <- "C:/rworking/deepseatools/indata"
csv <- 'DSCRTP_NatDB_20240115-0.csv'

setwd(path)
indata <- read.csv(csv, header = T, encoding = 'latin1')
## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### load old NDB #####
path <- "C:/rworking/deepseatools/indata"
csv <- 'DSCRTP_NatDB_20230928-0.csv'

setwd(path)
indata <- read.csv(csv, header = T, encoding = 'latin1')
## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.
filt_old <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### load subset of interest #####
path <- "C:/rworking/deepseatools/indata"
csv <- 'Caribbean-NatDB-subset_forRobert_20240201.csv'

setwd(path)
indata <- read.csv(csv, header = T, encoding = 'latin1')
## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.
sub <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### check #####
dim(sub)

sub %>% group_by(VerbatimScientificName, ScientificName,
                 VernacularNameCategory, IdentificationComments,
                 AphiaID, DatasetID) %>%
  summarize(n=n()) %>% View

filt_old %>% filter(CatalogNumber %in% sub$CatalogNumber) %>%
  group_by(VerbatimScientificName, ScientificName,
           VernacularNameCategory, IdentificationComments,
           AphiaID, DatasetID) %>%
  summarize(n=n()) %>% View

filt %>% filter(CatalogNumber %in% sub$CatalogNumber) %>%
  group_by(VerbatimScientificName, ScientificName,
           VernacularNameCategory, IdentificationComments,
           AphiaID, DatasetID) %>%
  summarize(n=n()) %>% View



