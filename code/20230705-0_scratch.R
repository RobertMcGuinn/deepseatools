##### mod_load_current #####
##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: scratch

##### packages #####
library(tidyverse)
##### manual input: load latest version of NDB #####
path <- "C:/rworking/deepseatools/indata"
csv <- "DSCRTP_NatDB_20230620-0.csv"
setwd(path)
indata <- read.csv(csv, header = T)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))


