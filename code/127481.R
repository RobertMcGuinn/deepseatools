##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240308
## purpose: dscrtp crosswalk with tator exports
## issuename: 20240308_tator_dscrtp_crosswalk_MTaipan_RPMcGuinn

##### linkage #####
filename <- '127481' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
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

##### load data #####
mapping <- read.xlsx('c:/rworking/deepseatools/indata/DSCRTP_MDBC_Tator_Mapping.xlsx')

## The workbook is the tailored / filtered-down version and contains data from multiple
## tables (e.g. fish, corals, marine debris). I believe this is what NCEI is
## using to ingest into their own database
mdbc <- read.xlsx('c:/rworking/deepseatools/indata/NF2206_Forward_Videos_Annotations_dEqqYAtRJW.xlsx',
                  sheet = 'corals_inverts')

events <- read.xlsx('c:/rworking/deepseatools/indata/NF2206_Forward_Videos_Annotations_dEqqYAtRJW.xlsx',
                    sheet = 'event_data')

## The .csv is  a "raw" version of the corresponding corals/inverts database table.
## You'll see "$*", those are built-in attributes/columns that come with every
## annotation. Subsequently there are custom project-specific columns that may
## or may not be filled in.
corals <- read.csv('c:/rworking/deepseatools/indata/nf2206_corals_example.csv')

##### check #####
table(corals$Transect)
table(events$EventType, useNA = 'always')
names(events)
View(corals)
names(corals)
table(corals$RecordType)
table(corals$ScientificName)
head(corals$X.x)
head(corals$X.y)
head(corals$X.x_pixels)
head(corals$X.y_pixels)
s %>% filter(FieldName == "MaximumSize") %>%
  group_by(FieldDescription, ValidValues) %>%
  summarize(n=n()) %>% View()
table(corals$CruiseID, useNA = 'always')
corals %>%
  filter(CruiseID == '') %>%
  pull(CruiseID)

##### find names in common #####
x <- intersect(names(mdbc), s$FieldName)
y <- intersect(names(corals), s$FieldName)
setdiff(y,x)
setdiff(x,y)

##### check #####
# setdiff(x,y)
# setdiff(y,x)
# table(corals$ScientificName, useNA = 'always')

# [1] "AssociatedTaxa"           "CategoricalAbundance"     "Condition"
# [4] "Density"                  "DepthInMeters"            "IndividualCount"
# [7] "Latitude"                 "Longitude"                "Morphospecies"
# [10] "RecordType"               "SampleAreaInSquareMeters" "ScientificName"

table(corals$Color, useNA = 'always')

##### create export #####
dscrtp_export <- corals %>%
  rename(
    VernacularName = 'CommonName',
    SurveyID = 'CruiseID',
    EventID = 'DiveID',
    MaximumSize = 'MaximumSizeHeight',
    MinimumSize = 'MinimumSizeHeight',
    IdentifiedBy = 'Identified.By',
    IdentificationDate = 'Identification.Date',
    IdentificationComments = 'Identification.Comments',
    Cover = 'PercentCover',
    OccurrenceComments = 'Occurrence.Comments',
    TaxonRank = 'Taxon.Rank',
    ObservationDate = 'Timestamp'
  )

##### transformations and paste operations #####
dscrtp_export$OccurrenceComments <- paste(dscrtp_export$OccurrencComments,
                                          'Heights measured using Tator software', sep = ' | ')

dscrtp_export$OccurrenceComments <- paste(dscrtp_export$OccurrenceComments,
                                          'Proportion or injury: ', dscrtp_export$Proportion.Of.Injury,
                                          'Type of injury: ', dscrtp_export$Type.Of.Injury, sep = ' | ')

dscrtp_export$EventID <- paste(dscrtp_export$EventID, dscrtp_export$Transect, sep = '-')

dscrtp_export$SampleID <- dscrtp_export$ObservationDate


## dealing with date and time
split_position = 10
date <- substr(dscrtp_export$ObservationDate, 1, split_position)
time <- substr(dscrtp_export$ObservationDate, split_position + 2, nchar(dscrtp_export$ObservationDate))
dscrtp_export$ObservationDate <- date
dscrtp_export$ObservationTime <- time

##### check #####
# head(dscrtp_export$ObservationDate)
# head(date)
# tail(dscrtp_export$ObservationDate)
# tail(date)
#
# head(dscrtp_export$ObservationTime)
# head(time)
# tail(dscrtp_export$ObservationTime)
# tail(time)

##### field lists #####
##fields that already have the same name ##
same <- intersect(names(corals), s$FieldName)

## fields that needed to be created
create <- c('VernacularName',
            'SurveyID',
            'EventID',
            'MaximumSize',
            'MinimumSize',
            'IdentifiedBy',
            'IdentificationDate',
            'IdentificationComments',
            'Cover',
            'OccurrenceComments',
            'TaxonRank',
            'ObservationDate',
            'ObservationTime',
            'SampleID')

## fields that needed a name transformation
needed <- c('CommonName',
            'CruiseID',
            'DiveID',
            'MaximumSizeHeight',
            'MinimumSizeHeight',
            'Identified.By',
            'Identification.Date',
            'Identification.Comments',
            'PercentCover',
            'OccurrenceComments',
            'Taxon.Rank',
            'Timestamp',
            'Proportion.Of.Injury',
            'Type.Of.Injury',
            'Transect')

dscrtp_fields <- union(same, create)

mdbc_fields <- union(same, needed)

##### whittle down to the only the variables needed and create the export #####
dscrtp_export_x <- dscrtp_export[,dscrtp_fields]

##### check #####
# setdiff(names(dscrtp_export), dscrtp_fields)
# setdiff(dscrtp_fields, names(dscrtp_export))

##### notes #####

# export of images from video?  stills from time stamp?
# what about image name associations?
# [8] "X.media_name"
# [9] "X.media_id"
# [21] "X.x"
# [22] "X.x_pixels"
# [23] "X.y"
# [24] "X.y_pixels"

# LocationAccuracy?

# what about completeness of ID? "Needs.Review" We only want things that have been reviewed.
# Needs.Review.Comments
# ReviewedBy, should this name go into IdentifiedBy?

# AssociatedNavIdtator (we would like to get smoothed tracklines) Are these at the dive level?

## Habitat questions.
# AssociatedGeologyId
# AttachmentSubstrate
# DominantSubstrate
# Bedrock
# Moderately.Coarse.Unconsolidated
# Fine.Unconsolidated
# Shell.Substrate
# Very.Coarse.Unconsolidated

# Coral.Presence
# Coral.Substrate
# Sponge.Presence

# AssociatedTransectId?

# ID.Analyst vs Identified.By?

# Color?

# We want fish too!

# Are you capturing 'Locality" or named places?










