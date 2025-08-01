##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: apply a filter and summarize

##### packages #####
library(tidyverse)

##### source load NDB (creates filt) #####
# source('c:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### filter and summarize #####
filt$DashLink <- paste('https://www.ncei.noaa.gov/waf/dsc-data/dashboards/',
                       filt$DatasetID, sep = '')
tab <- filt %>%
  filter(grepl('Rhode, Michael', PIAffiliation) |
           grepl('Rhode, Michael', DataContact) |
           grepl('Rhode, Michael', Reporter) |
           grepl('Rhode, Michael', IdentifiedBy)) %>%
  group_by(DatasetID,
           Vessel,
           ObservationYear,
           PI,
           PIAffiliation,
           DataContact,
           Reporter,
           IdentifiedBy,
           DashLink) %>%
  summarize(n=n())

View(tab)

##### create output #####
write_csv(tab,'c:/rworking/deepseatools/reports/20250731-0_Mike_Rhode_summary.csv')

##### check #####
x <- filt %>%
  filter(DatasetID == "NOAA_HB-14-02") %>%
  group_by(ObservationYear, ObservationDate) %>%
  summarize(n=n())
View(x)










