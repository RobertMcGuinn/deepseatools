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

##### load all versions (manual) #####
filename <- 'DSCRTP_NatDB_20250409-0.csv'
path <- paste0("C:/rworking/deepseatools/indata/", filename)
sub <- read.csv(path, header = T, encoding = 'latin1')

filename <- 'DSCRTP_NatDB_20250714-0_clean.csv'
path <- paste0("C:/rworking/deepseatools/indata/", filename)
filt_clean <- read.csv(path, header = T, encoding = 'latin1')

filename <- 'feature_service.csv'
path <- paste0("C:/rworking/deepseatools/indata/", filename)
filt_clean_feature <- read.csv(path, header = T, encoding = 'latin1')

##### check for differences #####
dim(filt_clean)
cats <- setdiff(filt_clean$CatalogNumber, filt_clean_feature$CatalogNumber)

sub %>%
  filter(CatalogNumber %in% cats) %>%
  pull(CatalogNumber)

setdiff(cats, sub$CatalogNumber)
x <- setdiff(sub$CatalogNumber, cats)
length(x)

setdiff(filt_clean$CatalogNumber,
        filt_clean_feature$CatalogNumber)

