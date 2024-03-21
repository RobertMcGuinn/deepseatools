##### mod_load_current #####
##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: recovering some flagged records: see Redmine issue
## redmine issue: https://vlab.noaa.gov/redmine/issues/127993

##### packages #####
library(tidyverse)

##### options #####
digits = 121

##### load national database (manual) #####
path <- "C:/rworking/deepseatools/indata"
csv <- 'DSCRTP_NatDB_20240115-0.csv'
# 'DSCRTP_NatDB_20230928-0.csv'
# "DSCRTP_NatDB_20230620-0.csv"
# "DSCRTP_NatDB_20230620-0_published.csv"
# "DSCRTP_NatDB_20230428-0_FeatureLayer.csv"

setwd(path)
indata_new <- read.csv(csv, header = T, encoding = 'latin1')
## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.
new <- indata_new %>%
  filter(Flag == "0", is.na(Phylum) == F)
##### remove james cook dataset from the mix #####
new2 <- new %>% filter(AccessionID != 'National_Oceanography_Centre_James_Cook_JC073_2012_2012_122876')

##### load national database (manual) #####
path <- "C:/rworking/deepseatools/indata"
csv <- 'DSCRTP_NatDB_20230928-0.csv'
# 'DSCRTP_NatDB_20230928-0.csv'
# "DSCRTP_NatDB_20230620-0.csv"
# "DSCRTP_NatDB_20230620-0_published.csv"
# "DSCRTP_NatDB_20230428-0_FeatureLayer.csv"

setwd(path)
indata_old <- read.csv(csv, header = T, encoding = 'latin1')
## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.
old <- indata_old %>%
  filter(Flag == "0", is.na(Phylum) == F)
##### remove james cook dataset from the mix #####
old2 <- old %>% filter(AccessionID != 'National_Oceanography_Centre_James_Cook_JC073_2012_2012_122876')

##### setdiff #####
x <-setdiff(old2$CatalogNumber, new2$CatalogNumber)
write.csv(x, 'c:/rworking/deepseatools/indata/20240321-0_missing_catalognumbers_correction_RPMcGuinn.csv')

y <- indata_new %>% filter(CatalogNumber %in% x) %>%
  group_by(Phylum, Class, Order, Family, Genus, ScientificName, VernacularNameCategory, Flag, FlagReason) %>%
  summarize(n=n())
View(y)

z <- indata_new %>% filter(CatalogNumber %in% x,Order == 'Antipatharia' |
                             Phylum == 'Porifera' |
                             Family == 'Parazoanthidae'|
                             Order == 'Scleralcyonacea' |
                             Order == 'Malacalcyonacea' |
                             Order == 'Octocorallia incertae sedis' |
                             Family == 'Stylasteridae' |
                             Order == 'Scleractinia' |
                             ScientificName == 'Octocorallia') %>%
  pull(CatalogNumber)

##### write out recovery CatalogNumbers #####
z %>% write.csv('c:/rworking/deepseatools/indata/20240321-0_missing_catalognumbers_correction_RPMcGuinn.csv')




