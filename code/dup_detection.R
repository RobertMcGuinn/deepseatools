##### Header #####
## Author: Robert P. McGuinn
## Date Started: 20210521
## Purpose: find and export dups

##### load packages #####
library(tidyverse)
library(knitr)
library(flextable)
library(sp)
library(rgdal)
library(maps)
library(extrafont)
library(googlesheets4)
library(leaflet)
library(RColorBrewer)
library(sf)
library(openxlsx)

##### load NDB #####
## method using 'read_csv' setting all columns to charactr and using specific encoding
setwd("C:/rworking/deepseatools/indata")
indata<-read_csv("DSCRTP_NatDB_20210414-0.csv",
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = 'ISO-8859-1'),
                 na = c("-999", "NA"))

filt <- indata %>%
  filter(Flag == "0")

## clean
rm(indata)

##### NMNH dup export for Tom Hourigan #####
sub <- filt %>%
  filter(DatasetID == "NMNH_IZ") %>%
  group_by(SampleID) %>%
  filter(n()>1) %>%
  arrange(SampleID)

## View(sub)
write.xlsx(sub, "20210521-0_NMNH_duplicates_RPMcGuinn.xlsx")
