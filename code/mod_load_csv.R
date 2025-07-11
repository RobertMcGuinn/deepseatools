##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20250710
## purpose: modular: load csv

##### linkage #####
filename <- 'mod_load_csv' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)

##### manual: set parameters #####
filename <- 'DSCRTP_NatDB_20250709-0_changed'

##### load data (manual:change filename) #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv(paste(filename, '.csv', sep = ''), fileEncoding = 'ISO-8859-15')


##### check #####
x <- setdiff(sub$CatalogNumber, filt$CatalogNumber)
length(x)

x <- setdiff(filt$CatalogNumber, sub$CatalogNumber)
length(x)


filt %>%
  filter(as.Date(EntryDate) > as.Date('2021-10-01') & as.Date(EntryDate) < as.Date('2023-10-01')) %>%
  pull(EntryDate) %>% table(useNA = 'always')
















