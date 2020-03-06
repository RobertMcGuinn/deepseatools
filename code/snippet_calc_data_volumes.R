##### load the packages #####
##Set working directory
setwd("C:/RWorking/deepseatools")

###### open the packages needed ######
library(tidyverse)
library(scales)
library(grid)
library(gtable)
# install.packages('googlesheets4')
library(googlesheets4)
library(googledrive)
#library(breakDown)
library(rgdal)
library(ggmap)
# install.packages('chron')
library(chron)
library(sp)
library(officer)
#library(pdftools)
library(lubridate)
library(zoo)
# library(plyr)

##### Refresh the latest data in the OER FFO Metrics #####
# Read in the latest OER Project Tracking spreadsheet

ffos <- read_sheet("https://docs.google.com/spreadsheets/d/1nU6NnB6QHDRgmTMIgV_2Y3DSJd0CU0tinTyQoTphVi4/edit#gid=584923915")

#
ffos[apply(ffos, 1, function(y) !all(is.na(y))),]

##### select only the fields we need ######
ffos_projs <- select(ffos, 'Fiscal Year', 'Type', 'OER Project Number', 'OER POC', 'Thematic Category', 'Project Title', 'PI', 'PI Affiliation', 'PI Contact Information (and Associates)', 'Award Type', 'Grant Number', 'Original Award End Date (Not Including Extensions)', 'Field Operations Start Date', 'Field Operations End Date', 'Are Data Discoverable and Accessible to the Public?', 'From What Archive(s)/Repository(ies) are Data Discoverable and Accessible?', 'Are Data Exempt?             (i.e., Marine Archaeology)', 'NCEI Has Received Data, But Not Yet Archived', 'Data Type(s)', 'Data Volume                (Number)', 'Data Volume                                  (Units)', 'Direct Link(s) to Data', 'Are Journal Manuscripts Archived in the NOAA Institutional Repository  (Required for FY19 and later FFOs)', 'Comments')

#assign variable names to the column headers
names(ffos_projs) <- c('FiscalYear','Category', 'OERProjNum', 'OERPOC', 'Theme','ProjTitle','PI','PIAffil','PIContact', 'AwardType','GrantNum', 'AwardEnd', 'FieldOpsStart','FieldOpsEnd', 'DataAvail', 'DataRepos','Exempt', 'NCEIReceived', 'DataTypes','DataVolume', 'VolumeUnits', 'DataLinks','Manuscripts', 'Comments')

##select only the fields we need
ffos_projs <- select(ffos, 'Fiscal Year', 'Type', 'OER Project Number', 'OER POC', 'Thematic Category', 'Project Title', 'PI', 'PI Affiliation', 'PI Contact Information (and Associates)', 'Award Type', 'Grant Number', 'Original Award End Date (Not Including Extensions)', 'Field Operations Start Date', 'Field Operations End Date', 'Are Data Discoverable and Accessible to the Public?', 'From What Archive(s)/Repository(ies) are Data Discoverable and Accessible?', 'Are Data Exempt?             (i.e., Marine Archaeology)', 'NCEI Has Received Data, But Not Yet Archived', 'Data Type(s)', 'Data Volume                (Number)', 'Data Volume                                  (Units)', 'Direct Link(s) to Data', 'Are Journal Manuscripts Archived in the NOAA Institutional Repository  (Required for FY19 and later FFOs)', 'Comments')

#assign variable names to the column headers
names(ffos_projs) <- c('FiscalYear','Category', 'OERProjNum', 'OERPOC', 'Theme','ProjTitle','PI','PIAffil','PIContact', 'AwardType','GrantNum', 'AwardEnd', 'FieldOpsStart','FieldOpsEnd', 'DataAvail', 'DataRepos','Exempt', 'NCEIReceived', 'DataTypes','DataVolume', 'VolumeUnits', 'DataLinks','Manuscripts', 'Comments')

##### fill in the values for projects with multiple data types #####
ffos_projs <- zoo::na.locf(ffos_projs, na.rm=FALSE)
ffos_projs$VolumeInGB = 0

# #calculate volume in GB
# for (row in 1:nrow(ffos_projs)) {
#   if (ffos_projs[row, "DataVolume"] != '-') {
#     ffos_projs[row, "VolumeInGB"] = ffos_projs[row,"DataVolume"]
#
#     if (ffos_projs[row, "VolumeUnits"] == "TB") {
#       ffos_projs[row, "VolumeInGB"] = ffos_projs[row, "VolumeInGB"] * 1024
#     }
#
#      else if (ffos_projs[row,"VolumeUnits"] == "MB") {
#       ffos_projs[row, "VolumeInGB"] = ffos_projs[row, "VolumeInGB"] / 1024
#     }
#   }
# }

##### using dplyr to mutate the 'DataVolume' numbers to the proper units #####
options(scipen = 10000)
options(digits = 3)

ffos_projs2 <- ffos_projs %>%
  mutate(DataVolumeNumeric = as.numeric(DataVolume)) %>%
  mutate(VolumeInGB = ifelse(VolumeUnits == "MB", DataVolumeNumeric / 1024,
                             ifelse(VolumeUnits == 'TB', DataVolumeNumeric * 1024,
                                    ifelse(VolumeUnits == 'GB', DataVolumeNumeric, DataVolumeNumeric)))) %>%
  mutate(VolumeInGB_round = round(VolumeInGB, 2))


ffos_projs2 %>%
  dplyr::select(DataVolumeNumeric, VolumeUnits, VolumeInGB, VolumeInGB_round) %>%
  View()

