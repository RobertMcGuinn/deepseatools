##### mod_load_current #####
##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: load current NDB file and filter out flagged
## output: [filt]

##### packages #####
library(tidyverse)

##### options #####
digits = 121

##### load national database (manual) #####
path <- "C:/rworking/deepseatools/indata"
csv <- "DSCRTP_NatDB_20240325-0.csv" # 'Aretha Franklin'
# "DSCRTP_NatDB_20240325-0.csv" # 'Aretha Franklin'
# 'DSCRTP_NatDB_20240115-0.csv' (taxonomy patch applied)
# 'DSCRTP_NatDB_20230928-0.csv'(published as '20230828-0')
# "DSCRTP_NatDB_20230620-0.csv"
# "DSCRTP_NatDB_20230620-0_published.csv"
# "DSCRTP_NatDB_20230428-0_FeatureLayer.csv"
# Link to master change log: https://docs.google.com/spreadsheets/d/1psUlMQS1d2rRgsiKWJsCTPleJ7TMKYNV/edit#gid=121019363

setwd(path)
indata <- read.csv(csv, header = T, encoding = 'latin1')
## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))

##### check #####
# filt %>%
#   filter(grepl('drop camera', SamplingEquipment),
#          FishCouncilRegion == 'North Pacific') %>%
#   pull(DatasetID) %>% unique()
#
# filt$dashlink <- paste('https://www.ncei.noaa.gov/waf/dsc-data/dashboards/',
#                        filt$DatasetID, sep = '')
#
# filt$no_images <- is.na(filt$ImageURL)
#
# filt %>% filter(grepl('drop camera', SamplingEquipment),
#                 FishCouncilRegion == 'North Pacific') %>%
#   group_by(DatasetID,
#            PI,
#            DataContact,
#            Reporter,
#            Vessel,
#            ObservationYear,
#            SamplingEquipment,
#            VehicleName,
#            dashlink,
#            no_images
#            ) %>%
#   summarize(n=n()) %>%
#   write.csv('c:/rworking/deepseatools/reports/20231109-0_summary_of_drop_camera_work_in_AK_RPMcGuinn.csv')
#
#








