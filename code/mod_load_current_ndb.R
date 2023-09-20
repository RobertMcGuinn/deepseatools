##### mod_load_current #####
##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: load current NDB file and filter out flagged
## output: [filt]

##### packages #####
library(tidyverse)

##### options #####
digits = 12

##### load national database (manual) #####
path <- "C:/rworking/deepseatools/indata"
csv <- 'DSCRTP_NatDB_20230828-0.csv'
# "DSCRTP_NatDB_20230620-0.csv"
# "DSCRTP_NatDB_20230620-0_published.csv"
# "DSCRTP_NatDB_20230428-0_FeatureLayer.csv"

setwd(path)
indata <- read.csv(csv, header = T, encoding = 'utf-8')
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))


