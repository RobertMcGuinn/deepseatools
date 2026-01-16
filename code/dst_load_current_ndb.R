##### dst_load_current_ndb #####
##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu | robert.mcguinn@noaa.gov
## purpose: modular: load current NDB file and filter out flagged
## input: csv of current database
## output: 'filt'

##### linkage #####
filename <- 'dst_load_current_ndb'
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)

##### packages #####
library(tidyverse)

##### options #####
digits = 121

##### parameter #####
filename <- 'DSCRTP_NatDB_20251223-0.csv'

##### load national database (manual) #####
path <- paste0("C:/rworking/deepseatools/indata/", filename)

## Version History
# "DSCRTP_NatDB_20251223-0.csv" # 'unnamed': DRAFT working file
# "DSCRTP_NatDB_20251001-0.csv" # 'Julie Andrews'
# "DSCRTP_NatDB_20250930-0.csv" # 'unnamed': DRAFT working file
# "DSCRTP_NatDB_20250926-0.csv" # 'unnamed': DRAFT working file
# "DSCRTP_NatDB_20250903-0_CSV" # 'unnamed: working file - MDBC Datasets'
# "DSCRTP_NatDB_20250714-0.csv" # 'Woody Guthrie'
# "DSCRTP_NatDB_20250409-0.csv" # 'Florence Price'
# "DSCRTP_NatDB_20241219-1.csv  # 'Edith Piaf'
# "DSCRTP_NatDB_20241022-1.csv" # 'Shaggy'
# "DSCRTP_NatDB_20240726-0.csv" # 'Mick Jagger (Stanley Kubrick, Mick Stanley, McStanley)'
# "DSCRTP_NatDB_20240723-0.csv" # 'taxonomy patch to be applied here'
# "DSCRTP_NatDB_20240325-0.csv" # 'Aretha Franklin'
# 'DSCRTP_NatDB_20240115-0.csv' (taxonomy patch applied)
# 'DSCRTP_NatDB_20230928-0.csv' (published as '20230828-0')
# "DSCRTP_NatDB_20230620-0.csv"
# "DSCRTP_NatDB_20230620-0_published.csv"
# "DSCRTP_NatDB_20230428-0_FeatureLayer.csv"
## Link to master change log: https://docs.google.com/spreadsheets/d/1psUlMQS1d2rRgsiKWJsCTPleJ7TMKYNV/edit#gid=121019363

indata <- read.csv(path, header = T, encoding = 'latin1')

filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

rm(indata)

## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.

##### clean up everything except core objects ######
# rm(list=setdiff(ls(), c("filt")))

##### write version to console #####
print(unique(filt$DatabaseVersion))
print(dim(filt))


