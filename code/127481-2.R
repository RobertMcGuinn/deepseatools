##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240308
## purpose: dscrtp crosswalk with tator exports
## issuename: 20240308_tator_dscrtp_crosswalk_MTaipan_RPMcGuinn

##### linkage #####
filename <- '127481' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '-2', '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(openxlsx)
library(googlesheets4)
library(googledrive)

##### authorizations #####
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### load database #####
source('C:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### load schema from google drive #####
sheetid <- '1jZa-b18cWxCVwnKsQcREPdaRQXTzLszBrtWEciViDFw'
s <- read_sheet(sheetid)

##### check #####
# s %>%
#   group_by(DSCRTPGroup, FieldName) %>%
#   summarize(n=n()) %>% View()

##### load project data #####
## copy on google drive:
## https://docs.google.com/spreadsheets/d/165lLT5g8r-m9d0sWtG2Y69bSHWL0XKik/edit?usp=drive_link&ouid=109414727136135095326&rtpof=true&sd=true
corals <- read.xlsx('c:/rworking/deepseatools/indata/PC2202L1_FWD_Videos_Annotation_Report.xlsx',
                  sheet = 'corals_inverts')

fish <- read.xlsx('c:/rworking/deepseatools/indata/PC2202L1_FWD_Videos_Annotation_Report.xlsx',
                    sheet = 'fish')

events <- read.xlsx('c:/rworking/deepseatools/indata/PC2202L1_FWD_Videos_Annotation_Report.xlsx',
                  sheet = 'events')

##### check #####
x <- names(corals)  # Assigns the names of 'corals' to 'x'
result <- grepl('Taxon', x)  # Checks for the presence of 'search term' in 'x'
# Subset 'x' to get only the names where 'result' is TRUE
matching_names <- x[result]
matching_names  # Displays the names where search term is present

names(events)



##### check #####
## find names in common
x <- intersect(names(corals), s$FieldName)

##### create export #####
## deal with the variables that need a simple name change
dscrtp_export <- corals %>%
  rename(
    VernacularName = 'CommonName', ##### this will conflict with our WoRMS API Call
    SurveyID = 'CruiseId',
    EventID = 'DiveId',
    MaximumSize = 'MaximumSizeHeight',
    MinimumSize = 'MinimumSizeHeight',
    Cover = 'PercentCover',
    ObservationDate = 'Timestamp'
  )

##### check ####
## find names in common
# x <- intersect(names(dscrtp_export), s$FieldName)
# x
#
# table(dscrtp_export$SurveyID, useNA = 'always')
# table(dscrtp_export$Morphospecies, useNA = 'always')


##### pastw information to OccurrenceComments #####
dscrtp_export$OccurrenceComments <- paste(dscrtp_export$OccurrencComments,
                                          'Heights measured using Tator software', sep = ' | ')

dscrtp_export$OccurrenceComments <- paste(dscrtp_export$OccurrenceComments,
                                          'Proportion of injury: ', dscrtp_export$Proportion.Of.Injury,
                                          'Type of injury: ', dscrtp_export$Type.Of.Injury, sep = ' | ')

##### paste transect to EventID
dscrtp_export$EventID <- paste(dscrtp_export$EventID, dscrtp_export$TransectID, sep = '-')

#### create Sample ID from pre-tranformed ObservationDate
dscrtp_export$SampleID <- dscrtp_export$ObservationDate #pre-transformed, see below for transformation.

##### tranforming date and time #####
split_position = 10
date <- substr(dscrtp_export$ObservationDate, 1, split_position)
time <- substr(dscrtp_export$ObservationDate, split_position + 2, nchar(dscrtp_export$ObservationDate))
dscrtp_export$ObservationDate <- date
dscrtp_export$ObservationTime <- time

## get rid of everything after the '+' in the ObservationTime
dscrtp_export$ObservationTime <- sub("\\+.*", "", dscrtp_export$ObservationTime)

##### get just the DSCRTP fields out #####
dscrtp_fields <- intersect(names(dscrtp_export), s$FieldName)

##### EXPORT TO DSCRTP, whittle down to the only the variables needed and create the export #####
## isolate the DSCRTP fields
dscrtp_export_x <- dscrtp_export[,dscrtp_fields]

##### check #####
dscrtp_export_x %>% pull(ObservationTime) %>% table(useNA = 'always')

## write the file to disk
write.csv(dscrtp_export_x,
          'c:/rworking/deepseatools/indata/20240410-0_TATOR_DSCRTP_export_RPMcGuinn.csv',
          row.names = F)











