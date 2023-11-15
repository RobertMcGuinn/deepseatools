##### header #####
## author: robert.mcguinn@noaa.gov | rpm@alumni.duke.edu
## date_started: 20231109
## purpose: summary query for AK drop camera datasets for roundup for Pam and Arvind

##### packages #####
library(tidyverse)

##### options #####
digits = 121

##### load national database (manual) #####
path <- "C:/rworking/deepseatools/indata"
csv <- 'DSCRTP_NatDB_20230928-0.csv'
# "DSCRTP_NatDB_20230620-0.csv"
# "DSCRTP_NatDB_20230620-0_published.csv"
# "DSCRTP_NatDB_20230428-0_FeatureLayer.csv"

setwd(path)
indata <- read.csv(csv, header = T, encoding = 'latin1')
## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### create summary #####
filt$dashlink <- paste('https://www.ncei.noaa.gov/waf/dsc-data/dashboards/',
filt$DatasetID, sep = '')

filt$no_images <- is.na(filt$ImageURL)

filt %>% filter(grepl('drop camera', SamplingEquipment),
                FishCouncilRegion == 'North Pacific') %>%
  group_by(DatasetID,
           PI,
           DataContact,
           Reporter,
           Vessel,
           ObservationYear,
           SamplingEquipment,
           VehicleName,
           dashlink,
           no_images
  ) %>%
  summarize(n=n()) %>%
  write.csv('c:/rworking/deepseatools/reports/20231109-0_summary_of_drop_camera_work_in_AK_RPMcGuinn.csv')

browseURL('https://github.com/RobertMcGuinn/deepseatools/blob/master/code/20231109-0_summary_of_drop_camera_work_in_AK_RPMcGuinn.R')
