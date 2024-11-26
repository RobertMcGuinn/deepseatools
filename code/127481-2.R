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
# intersect(names(corals), names(fish))
# setdiff(names(corals), names(fish))
# setdiff(names(fish), names(corals))
#
# x <- intersect(names(corals), names(events))
# summary(corals[,x])
#
# table(events$EventType, useNA = 'always')
# names(events)
# corals$MediaId
#
# corals %>% select(MediaName, MediaId) %>% View()
#
# corals %>% group_by(MediaName, MediaId) %>% summarize(n=n()) %>% View()

##### ***** #####
##### create a single fish and coral data frame #####
## fix the mis-matching classes between vectors so I can use bind_rows function
corals$OccurrenceComments <- as.character(corals$OccurrenceComments)
fish$IdentificationComments <- as.character(fish$IdentificationComments)
fish$IdentifiedBy <-  as.character(fish$IdentifiedBy)
fish$IdentificationDate <- as.character(fish$IdentificationDate)

## bind rows
coralsfish <- bind_rows(corals, fish)

##### filter: take out the unknown and null within ScientificName #####
coralsfish_cl <- coralsfish %>% filter(is.na(ScientificName) == F)
coralsfish_cl <- coralsfish %>% filter(!(grepl('Unknown', ScientificName) | grepl('unknown', ScientificName)))

##### check #####
# table(coralsfish_cl$ScientificName, useNA = 'always')
# coralsfish %>% filter(is.na(ScientificName) == T) %>% pull(ScientificName) %>% length()
# coralsfish %>% filter(grepl('Unknown', ScientificName) |
#                       grepl('unknown', ScientificName)) %>%
#   pull(ScientificName) %>% table()

# names(coralsfish_cl)

##### join the geology information by timestamp #####
## get the timestamps in a joinable format
coralsfish_cl$Timestamp_pos <- ymd_hms(coralsfish_cl$Timestamp)
geology$Timestamp_pos <- ymd_hms(geology$Timestamp)

## Specify the tolerance window in seconds (e.g., 600 seconds for 10 minutes)
tolerance_window <- 5

## Perform the fuzzy left join
result <- difference_left_join(coralsfish_cl, geology,
                               by = "Timestamp_pos",
                               max_dist = tolerance_window,
                               distance_col = "time_diff") %>%
  group_by(Timestamp_pos.x) %>%
  slice_min(order_by = time_diff, n = 1, with_ties = FALSE) %>% # `with_ties = FALSE` ensures only one row per Timestamp_pos.x
  ungroup()

coralsfish_cl$joiner_Timestamp_pos <- paste(coralsfish_cl$ScientificName, coralsfish_cl$Timestamp, sep = '_')
result$joiner_Timestamp_pos.x <- paste(result$ScientificName, result$Timestamp.x, sep = '_' )

result <-
  left_join(coralsfish_cl, result, by = c("joiner_Timestamp_pos" = "joiner_Timestamp_pos.x"))

##### ***** #####
##### create export: main crosswalk and transformations #####
dscrtp_export <- result %>%
  mutate(
    ImageFilePath = paste(MediaId, MediaName, Frame, sep = ' | '), ## this will need to be adapted when we get the media in house
    SampleAreaInSquareMeters = SampleAreaInSquareMeters.x,
    ScientificName = ScientificName.x,
    Morphospecies = paste(Morphospecies.x, Color.x, sep = ' | '),
    IdentifiedBy = paste(IdentifiedBy, ReviewedBy, set = ' | '),
    IdentificationDate = IdentificationDate,
    IdentificationComments = paste(IdentificationComments, AnnotatorResponse, OccurrenceComments.x, sep = ' | '),
    OccurrenceComments = paste(OccurrenceComments.x, TypeOfInjury.x, sep = ' | '),
    Condition = Condition.x,
    Longitude = Longitude,
    Latitude = Latitude,
    ObservationDate = Timestamp,
    MinimumDepthInMeters = NavDepth,
    MaximumDepthInMeters = NavDepth,
    CategoricalAbundance = coalesce(CategoricalAbundance.x, FishCategoricalAbundance.x),
    IndividualCount = IndividualCount.x,
    Cover = PercentCover.x,
    MaximumSize = MaximumSizeHeight.x,
    MinimumSize = MinimumSizeHeight.x,
    CMECSGeoForm = paste(CMECSGeoform1, CMECSGeoform2, sep = ' | '),
    Habitat = paste(CMECSGeoform1, CMECSGeoform2, DominantSubstrate, sep = ' | '),
    Salinity = Salinity.x,
    Temperature = Temperature.x,
    SurveyID = CruiseId,
    EventID = paste(DiveId,TransectId.x, sep = '-'),
    BboxHeight = BboxHeight.x,
    BboxWidth = BboxWidth.x,
    BboxX = BboxX.x,
    BboxY = BboxY.x)

## paste information to OccurrenceComments
dscrtp_export$OccurrenceComments <- paste(dscrtp_export$OccurrencComments,
                                          'Heights measured using Tator software', sep = ' | ')

dscrtp_export$OccurrenceComments <- paste(dscrtp_export$OccurrenceComments,
                                          'Proportion of injury: ', dscrtp_export$Proportion.Of.Injury,
                                          'Type of injury: ', dscrtp_export$Type.Of.Injury, sep = ' | ')

## create Sample ID from pre-tranformed ObservationDate
dscrtp_export$SampleID <- paste(dscrtp_export$ObservationDate, dscrtp_export$TatorId, sep = '_') #pre-transformed, see below for transformation.

## transforming date and time
split_position = 10
date <- substr(dscrtp_export$ObservationDate, 1, split_position)
time <- substr(dscrtp_export$ObservationDate, split_position + 2, nchar(dscrtp_export$ObservationDate))
dscrtp_export$ObservationDate <- date
dscrtp_export$ObservationTime <- time

## get rid of everything after the '+' in the ObservationTime
dscrtp_export$ObservationTime <- sub("\\+.*", "", dscrtp_export$ObservationTime)

## get just the DSCRTP fields out
dscrtp_fields <- c(intersect(names(dscrtp_export), s$FieldName),
                   'BboxHeight',
                   'BboxWidth',
                   'BboxX',
                   'BboxY')


## isolate just the DSCRTP fields
dscrtp_export <- dscrtp_export[,dscrtp_fields]

## transform the 'Condition' variable
dscrtp_export <- dscrtp_export %>%
  mutate(
    Condition = as.character(Condition), # Convert to character
    Condition = case_when(
      Condition %in% c('Affected') ~ 'Damaged',
      Condition %in% c('Healthy') ~ 'Live',
      Condition %in% c('Not Set', 'Unknown') ~ '',
      Condition %in% c('Dead') ~ 'Dead',
      TRUE ~ ''
    ),
    Condition = na_if(Condition, '') # Replace blanks with NA
  )

## transform the 'CategoricalAbundance' Value
dscrtp_export <- dscrtp_export %>%
  mutate(
    CategoricalAbundance = as.character(CategoricalAbundance), # Convert to character
    CategoricalAbundance = case_when(
      CategoricalAbundance %in% c('>500') ~ '>100',
      CategoricalAbundance %in% c('Not Set') ~ '',
      TRUE ~ CategoricalAbundance
    ),
    CategoricalAbundance = na_if(CategoricalAbundance, '') # Replace blanks with NA
  )

## remove records with ScientificName issues
dscrtp_export <- dscrtp_export %>%
  filter(
    !is.na(ScientificName),                      # Ensure ScientificName is not NA
    ScientificName != 'Human Debris-Line',       # Exclude 'Human Debris-Line'
    ScientificName != 'Fishing Line'             # Exclude 'Fishing Line'
  )


##### check #####
dscrtp_export %>% pull(MinimumSize) %>% table(useNA = 'always')
dscrtp_export %>% pull(MaximumSize) %>% table(useNA = 'always')
dscrtp_export %>% pull(Condition) %>% unique()
dscrtp_export %>% pull(Condition) %>% table(useNA = 'always')
dscrtp_export %>% pull(CategoricalAbundance) %>% table(useNA = 'always')
dscrtp_export %>% pull(IdentifiedBy) %>% table(useNA = 'always')
dscrtp_export %>% pull(Latitude) %>% table(useNA = 'always')
dscrtp_export %>% pull(Longitude) %>% table(useNA = 'always')
dscrtp_export %>% pull(ScientificName) %>% table(useNA = 'always')

##### write the file to disk #####
write.csv(dscrtp_export,
          'c:/rworking/deepseatools/indata/20241125-0_Example_TATOR_DSCRTP_export_RPMcGuinn.csv',
          row.names = F)

##### NOTES #####
## Questions for Mark Taipan:
## What is the 'Polygon' and 'PolygonDtype' going to be used for?
## Should I only take the ones where 'NeedsReview' is false?
## Salinity and Temperature are empty. Will they be populated for the corals and fish sheet?
## IdentificationDate is mostly empty, would we be expecting this to be populated in the future?
## Go over our structure for the ImageFilePath variable.
## What is VersionName and VersionID?
## What should we use as our SampleID and TrackingID identifiers? (possibly paste(MediaId, MediaName, Frame, sep = ' | ') or 'TatorID')










