##### header #####
## author: Robert McGuinn
## date started: 20221004
## purpose: load the data dictionary / schema

##### packages #####
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(flextable)

##### authorizations #####
# Set authentication token to be stored in a folder called \.secrets``
options(gargle_oauth_cache = ".secrets")
gs4_auth()
list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth()

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

##### set version number #####
s$version <- '20230405-0'

## checking
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(FieldDescription)
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(ValidValues)
# setdiff(names(filt), s$FieldName)
# setdiff(s$FieldName, names(filt))

##### filter to make a public version for inclusion on NOAA GeoPlatform #####
s_public <- s %>%
  filter(InternalUseOnly == 0) %>%
  dplyr::select(version,1,3,4)

##### write the CSV ######
write_csv(s_public,
          "c:/rworking/deepseatools/indata/NOAA_NDB_corals_sponges_data_dictionary.csv")

##### write the html #####
library(flextable)
ft <- s_public %>%
  flextable() %>%
  autofit() %>%
  theme_zebra() %>%
  fontsize(size = 7)

ft <- width(ft, j = 1,
            width = 3,
            unit = "in")

## print the flextable object
setwd('c:/rworking/deepseatools/reports/')
save_as_html(ft,
             path='c:/rworking/deepseatools/reports/NOAA_NDB_corals_sponges_data_dictionary.html',
             title = paste('Data Dictionary version', unique(s$version))
             )




