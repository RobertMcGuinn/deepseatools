##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: apply a filter and summarize

##### packages #####
library(tidyverse)

##### variables #####
# datasetID <- "KOK 05-11"

##### source load NDB (creates filt) #####
source('c:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### filter and summarize #####
filt$DashLink <- paste('https://www.ncei.noaa.gov/waf/dsc-data/dashboards/',
                       filt$DatasetID, sep = '')
tab <- filt %>%
  filter(ObservationYear == 2013 |
           ObservationYear == 2014|
           ObservationYear == 2015 |
           ObservationYear == 2018,
         grepl('Nizinski', PI) |
           grepl('Packer', DataContact) |
           grepl('Nizinski', DataContact) |
           grepl('Rhode', IdentifiedBy)) %>%
  group_by(DatasetID,
           Vessel,
           ObservationYear,
           PI,
           DataContact,
           Reporter,
           DashLink) %>%
  summarize(n=n())

View(tab)

##### create output #####
write_csv(tab,'c:/rworking/deepseatools/reports/NE_initiative_summary_ObservationYear_2013-2015.csv')

##### check #####
x <- filt %>%
  filter(DatasetID == "NOAA_HB-14-02") %>%
  group_by(ObservationYear, ObservationDate) %>%
  summarize(n=n())
View(x)










