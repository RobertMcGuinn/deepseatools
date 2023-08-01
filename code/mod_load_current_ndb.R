##### mod_load_current #####
##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: load current NDB file and filter out flagged
## output: [filt]

##### packages #####
library(tidyverse)

##### options #####
digits = 12

##### variables #####
path <- "C:/rworking/deepseatools/indata"
csv <- "DSCRTP_NatDB_20230620-0.csv"
# "DSCRTP_NatDB_20230620-0_published.csv"
# "DSCRTP_NatDB_20230428-0_FeatureLayer.csv"

##### manual input: load latest version of NDB #####
setwd(path)
indata <- read.csv(csv, header = T)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

## cleanup
rm(indata)


