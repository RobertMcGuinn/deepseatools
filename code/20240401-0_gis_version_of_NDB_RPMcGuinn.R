library(tidyverse)

filt %>% pull(VernacularNameCategory) %>%
  table(useNA = 'always')

x<-filt %>% filter(is.na(VernacularNameCategory)==T) %>% pull(CatalogNumber)

write.csv(x, 'C:/rworking/deepseatools/indata/20240328_missing_VernacularNameCategory_RPMcGuinn.csv')

length(x)

##### options #####
digits = 121

##### load national database (manual) #####
path <- "C:/rworking/deepseatools/indata"
csv <- "20230325-0_DSCRTP_GIS_RPMcGuinn.csv" # 'Aretha Franklin'
setwd(path)
gis <- read.csv(csv, header = T, encoding = 'latin1')
## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.


