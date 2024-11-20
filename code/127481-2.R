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
library(fuzzyjoin)
library(lubridate)

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

##### load export from tator #####
## copy on google drive:
## https://docs.google.com/spreadsheets/d/165lLT5g8r-m9d0sWtG2Y69bSHWL0XKik/edit?usp=drive_link&ouid=109414727136135095326&rtpof=true&sd=true

corals <- read.xlsx('c:/rworking/deepseatools/indata/PC2202L1_FWD_Videos_Annotation_Report.xlsx',
                  sheet = 'corals_inverts')

fish <- read.xlsx('c:/rworking/deepseatools/indata/PC2202L1_FWD_Videos_Annotation_Report.xlsx',
                    sheet = 'fish')

events <- read.xlsx('c:/rworking/deepseatools/indata/PC2202L1_FWD_Videos_Annotation_Report.xlsx',
                  sheet = 'events')

geology <- read.xlsx('c:/rworking/deepseatools/indata/PC2202L1_FWD_Videos_Annotation_Report.xlsx',
                               sheet = 'geology')

##### check #####
intersect(names(corals), names(fish))
setdiff(names(corals), names(fish))
setdiff(names(fish), names(corals))

x <- intersect(names(corals), names(events))
summary(corals[,x])

table(events$EventType, useNA = 'always')
names(events)
corals$MediaId

corals %>% select(MediaName, MediaId) %>% View()

corals %>% group_by(MediaName, MediaId) %>% summarize(n=n()) %>% View()

##### create a single fish and coral data frame #####

## fix the mis-matching classes between vectors so I can use bind_rows function
corals$OccurrenceComments <- as.character(corals$OccurrenceComments)
fish$IdentificationComments <- as.character(fish$IdentificationComments)
fish$IdentifiedBy <-  as.character(fish$IdentifiedBy)
fish$IdentificationDate <- as.character(fish$IdentificationDate)

## bind rows
coralsfish <- bind_rows(corals, fish)

##### take out the unknown and null within ScientificName #####
coralsfish_cl <- coralsfish %>% filter(is.na(ScientificName) == F)
coralsfish_cl <- coralsfish %>% filter(!(grepl('Unknown', ScientificName) | grepl('unknown', ScientificName)))

##### check #####
table(coralsfish_cl$ScientificName, useNA = 'always')
coralsfish %>% filter(is.na(ScientificName) == T) %>% pull(ScientificName) %>% length()
coralsfish %>% filter(grepl('Unknown', ScientificName) |
                      grepl('unknown', ScientificName)) %>%
  pull(ScientificName) %>% table()


##### join the geology information by timestamp #####
## get the timestamps in a joinable format
coralsfish_cl$Timestamp_pos <- ymd_hms(coralsfish_cl$Timestamp)
geology$Timestamp_pos <- ymd_hms(geology$Timestamp)

## Specify the tolerance window in seconds (e.g., 600 seconds for 10 minutes)
tolerance_window <- 20

## Perform the fuzzy left join
result <- difference_left_join(coralsfish_cl, geology, by = "Timestamp_pos", max_dist = tolerance_window, distance_col = "time_diff") %>%
  group_by(Timestamp_pos.x) %>%
  slice_min(order_by = time_diff, n = 1, with_ties = FALSE) %>% # `with_ties = FALSE` ensures only one row per Timestamp_pos.x
  ungroup()

result <-
  left_join(coralsfish_cl, result, by = c("Timestamp_pos" = "Timestamp_pos.x"))

##### check #####
result %>% group_by(CMECSGeoform1,
                    CMECSGeoform2,
                    DominantSubstrate,
                    BedrockMegaclast,
                    BoulderCobble,
                    PebbleGranule,
                    SandMud,
                    AlgalSubstrate,
                    CoralSubstrate,
                    ShellSubstrate) %>%
  summarize(n=n()) %>% View()


names(result)

result %>%
  pull(CMECSGeoform1) %>%
  table(useNA = 'always')

result %>%
  pull(CMECSGeoform2) %>%
  table(useNA = 'always')

result %>%
  pull(DominantSubstrate) %>%
  table(useNA = 'always')

result %>%
  pull(DominantSubstrate.x) %>%
  table(useNA = 'always')

result %>%
  filter(DominantSubstrate == 'Bedrock') %>% pull(DominantSubstrate) %>%
  table(useNA = 'always')

result %>% select(Timestamp_pos, Timestamp_pos.y) %>% View()

result %>% filter(is.na(Timestamp_pos.y) == T) %>% pull(Timestamp_pos.y) %>% length()
result %>% filter(is.na(Timestamp_pos.y) == F) %>% pull(Timestamp_pos.y) %>% length()

table(events$Camera, useNA = 'always')

names(coralsfish)
names(result)

result %>% group_by(IdentificationComments, IdentificationComments.x, IdentificationComments.y) %>%
  summarize(n=n()) %>% View()

result %>% group_by(IdentifiedBy, IdentifiedBy.x, IdentifiedBy.y, IdAnalyst, IdAnalyst.x, IdAnalyst.y) %>%
  summarize(n=n())

##### create export #####
## deal with the variables that need a simple name change
dscrtp_export <- coralsfish %>%
  rename(
    VernacularName = 'CommonName', ##### this will conflict with our WoRMS API Call
    SurveyID = 'CruiseId',
    EventID = 'DiveId',
    MaximumSize = 'MaximumSizeHeight',
    MinimumSize = 'MinimumSizeHeight',
    Cover = 'PercentCover',
    ObservationDate = 'Timestamp',
    DepthInMeters = 'NavDepth'
  )

##### check ####
## find names in common
# x <- intersect(names(dscrtp_export), s$FieldName)
# x
#
# table(dscrtp_export$SurveyID, useNA = 'always')
# table(dscrtp_export$Morphospecies, useNA = 'always')


##### paste information to OccurrenceComments #####
dscrtp_export$OccurrenceComments <- paste(dscrtp_export$OccurrencComments,
                                          'Heights measured using Tator software', sep = ' | ')

dscrtp_export$OccurrenceComments <- paste(dscrtp_export$OccurrenceComments,
                                          'Proportion of injury: ', dscrtp_export$Proportion.Of.Injury,
                                          'Type of injury: ', dscrtp_export$Type.Of.Injury, sep = ' | ')

##### paste transect information into to EventID
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











