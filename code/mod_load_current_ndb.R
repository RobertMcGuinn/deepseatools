##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: load current NDB file and filter

##### packages #####
library(tidyverse)

##### variables #####
path <- "C:/rworking/deepseatools/indata"
csv <- "DSCRTP_NatDB_20230428-0.csv"
##  "DSCRTP_NatDB_20220801-0.csv"
## "DSCRTP_NatDB_20221213-0_ingest"

##### manual input: load latest version of NDB #####
setwd(path)
indata <- read.csv(csv, header = T)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)
## cleanup
rm(indata)


