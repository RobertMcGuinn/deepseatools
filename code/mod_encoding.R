##### header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## date started: 20230920
## purpose: handling special character encoding issues

##### packages #####
library(tidyverse)
library(stringi)

##### load database #####
source('C:/rworking/deepseatools/code/mod_load_current_ndb.R')

## check
# dim(filt)

##### filter NDB #####
d <- filt %>%
  filter(
  #  DatasetID == 'NMNH_IZ' ##NMNH_IZ | NIWA
  )

##### check #####
table(d$ScientificName, useNA = 'always')
tail(table(d$ScientificName, useNA = 'always'))
table(d$DataProvider, useNA = 'always')
table(d$DataContact, useNA = 'always')

##### find problems ####
d$DataContact <- stri_trans_general(d$DataContact, "latin-ascii")
selection <- d %>% filter(grepl('�', DataContact)) %>% pull(CatalogNumber)
filt %>% filter(CatalogNumber %in% selection) %>% pull(DataContact) %>% unique()
d %>% filter(CatalogNumber %in% selection) %>% pull(DataContact) %>% unique()

d$ScientificName <- stri_trans_general(d$ScientificName, "latin-ascii")
selection <- d %>% filter(grepl('�', ScientificName))%>% pull(CatalogNumber)
filt %>% filter(CatalogNumber %in% selection) %>% pull(ScientificName) %>% unique()

d$DataProvider <- stri_trans_general(d$DataProvider, "latin-ascii")
selection <- d %>% filter(grepl('�', DataProvider))%>% pull(CatalogNumber)
filt %>% filter(CatalogNumber %in% selection) %>% pull(DataProvider) %>% unique()
d %>% filter(CatalogNumber %in% selection) %>% pull(DataProvider) %>% unique()

d$ScientificNameAuthorship <- stri_trans_general(d$ScientificNameAuthorship, "latin-ascii")
selection <- d %>% filter(grepl('�', ScientificNameAuthorship))%>% pull(CatalogNumber)
filt %>% filter(CatalogNumber %in% selection) %>% pull(ScientificNameAuthorship) %>% unique()

##### manual corrections #####
d <- d %>% mutate(ScientificName =
                    ifelse(ScientificName == "Muriceides k\xfckenthali",
                           'Muriceides kuekenthali',
                           as.character(ScientificName)))
d <- d %>% mutate(DataProvider =
                    ifelse(DataProvider == "Due\xf1as et al. (2014)",
                           'New Zealand National Institute of Water and Atmospheric Research (NIWA)',
                           as.character(DataProvider)))
d <- d %>% mutate(DataContact =
                    ifelse(DataContact == "Due\xf1as, Luisa (lduenasm@unal.edu.co",
                           'Dueñas, Luisa | duenasm@unal.edu',
                           as.character(DataContact)))





