#### packages #####
library(tidyverse)

##### options #####
digits = 121

##### load national database (manual) #####
filename <- 'DSCRTP_NatDB_20250409-0.csv'
path <- paste0("C:/rworking/deepseatools/indata/", filename)
sub <- read.csv(path, header = T, encoding = 'latin1')

filename <- 'DSCRTP_NatDB_20250714-0_clean.csv'
path <- paste0("C:/rworking/deepseatools/indata/", filename)
filt_clean <- read.csv(path, header = T, encoding = 'latin1')

filename <- 'feature_service.csv'
path <- paste0("C:/rworking/deepseatools/indata/", filename)
filt_clean_feature <- read.csv(path, header = T, encoding = 'latin1')

