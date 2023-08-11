##### Header #####
## author: Robert McGuinn: rpm@alumni.duke.edu, robert.mcguinn@noaa.gov
## started on: 20230809
## purpose: troubleshooting taxonomic issues with database

##### packages #####
library(tidyverse)
library(googledrive)
library(googlesheets4)

##### load NDB from local file (manual)#####
setwd("C:/rworking/deepseatools/indata")
filename <- "DSCRTP_NatDB_20230620-0.csv"
indata <- read.csv(filename,
                   encoding = "latin9",
                   header = TRUE,
                   stringsAsFactors = FALSE)
filt <- indata %>%
  filter(Flag == 0)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))

##### inspect taxonomy #####
x <- filt %>%
  # filter(Genus == 'Bebryce') %>%
  filter(ScientificName == 'Plexauridae') %>%
  group_by(DatasetID,
           AccessionID,
           EntryDate,
           EntryUpdate,
           AphiaID,
           VerbatimScientificName,
           TaxonRank,
           ScientificName,
           Genus,
           Species) %>%
  summarize(n=n())

x$EntryUpdate <- as.Date(x$EntryUpdate)
hist(x$EntryUpdate, breaks = 50)

##### inspect uncool taxonomy #####
x <- filt %>%
  # filter(Genus == 'Bebryce') %>%
  filter(
    # ScientificName == 'Plexauridae',
    TaxonRank == 'genus',
    # is.na(Genus) == F,
    is.na(Species) == F) %>%
  group_by(DatasetID,
           AccessionID,
           EntryDate,
           EntryUpdate,
           AphiaID,
           VerbatimScientificName,
           TaxonRank,
           ScientificName,
           Genus,
           Species) %>%
  summarize(n=n())

x$EntryUpdate <- as.Date(x$EntryUpdate)
hist(x$EntryUpdate, breaks = 50)
View(x)
