##### Heading #####
# author: Robert P. McGuinn
# date started: 20190305
# purpose: Summary of ScientificNames by family (export to google drive)

##### Installation/Loading of Packages #####
# install.packages('xlsx')
#install.packages('openxlsx')
library(openxlsx)
library(sp)
library(tidyverse)
library(rerddap)
library(leaflet)
# install.packages('extrafont')
library(extrafont)
# install.packages('RColorBrewer')
library(RColorBrewer)
# install.packages('googlesheets')
library(googlesheets)
# install.packages('googledrive')
library(googledrive)
library(rmarkdown)
library(knitr)
#install.packages("maps")
library(maps)
#install.packages("rgdal")
library(rgdal)
#install('raster')
library(raster)
#install.packages("spocc")
library(spocc)
#install.packages('arcgisbinding')
library(arcgisbinding)
arc.check_product()
#install.packages('refinr')
library(refinr)
# install.packages('marmap')
library(marmap)
#install.packages('prettydoc')
library(prettydoc)
#install.packages('robis')
library(robis)

##### _____ Bringing in database #####
setwd("C:/rworking/digs/indata")
indata<-read.csv("DSCRTP_NatDB_20190208-0.csv", header = T) #DSCRTP_NatDB_20181005-0.csv # DSCRTP_NatDB_20181211-2.csv
filt <- indata %>%
  filter(Flag == "0")

##### filtering data and creating an alphabetical List of ScientificNames - Grouped by Family #####
sum_tbl <-
  filt %>%
  arrange(ScientificName) %>%
  filter(
    Phylum == 'Cnidaria',
    FishCouncilRegion == 'Caribbean' |
      FishCouncilRegion == 'Gulf of Mexico' |
      FishCouncilRegion == 'South Atlantic'
    ) %>%
  group_by(Family) %>%
  summarize(
    ScientificName = paste(unique(ScientificName), collapse=" | "),
    Records = n())
# sum_tbl <- kable(sum_tbl, row.names = F, digits = 2)
# sum_tbl

##### exporting the file to a Google Sheet #####
setwd("C:/rworking/digs/indata")
sum_tbl %>% write.csv('20190305-0_SEDCI_taxa_review_DSCRTP_NatDB_20190208-0_RPMcGuinn.csv', row.names = FALSE)
xsheet <- gs_upload('20190305-0_SEDCI_taxa_review_DSCRTP_NatDB_20190208-0_RPMcGuinn.csv')
gs_browse(xsheet, ws = 1)

