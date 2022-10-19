##### header #####
## author: Robert McGuinn
## date started: 20221004
## purpose: load the data dictionary / schema

##### packages #####
library(tidyverse)
library(googlesheets4)
library(googledrive)

##### authorizations #####
drive_auth(email = "robert.mcguinn@noaa.gov")
gs4_auth(email = "robert.mcguinn@noaa.gov")
##### load NDB #####
## [manual]
# setwd("C:/rworking/deepseatools/indata")
# filename <- "DSCRTP_NatDB_20220801-0.csv"
# indata <- read_csv(filename,
#                  col_types = cols(.default = "c"),
#                  locale = locale(encoding = 'ISO-8859-1'),
#                  na = c("-999", "NA"))
#
# filt <- indata %>% filter(Flag == "0")

##### download Google Sheet version of schema for use in R #####

## Register and download Google Sheet using googlesheets4::read_sheet
s <- read_sheet('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')

## checking
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(FieldDescription)
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(ValidValues)
# setdiff(names(filt), s$FieldName)
# setdiff(s$FieldName, names(filt))

##### filter to make a public version for inclusion on NOAA GeoPlatform #####
s_public <- s %>%
  # filter(InternalUseOnly == 0) %>%
  dplyr::select(1:4)

##### write the CSV ######
write_csv(s_public, "c:/rworking/deepseatools/indata/20221004-0_NOAA_NDB_corals_sponges_data_dictionary.csv")

##### manual step: load

