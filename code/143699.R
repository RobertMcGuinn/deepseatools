##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240308
## purpose: dscrtp crosswalk with tator exports
## issuename: 20250306-0_NOAA_PC2202L1_MDBC_143699

##### linkage #####
filename <- '143699' ## manual: for this code file name, match to redmine
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

##### load exports from tator #####
tatorexport <- 'PC2202L1_Forward Videos_2025-02-18_report_XJDopbtRvb.xlsx'

corals <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                  sheet = 'corals_inverts')

fish <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                    sheet = 'fish')

events <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                  sheet = 'events')

geology <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                               sheet = 'geology')

##### load data from SQL files #####
filename <- 'SAMPLEMETADATA.xlsx'
samplemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                     sheet = 'BiologicalSamples')

filename <- 'DIVEVEHICLEMETADATA.xlsx'
divevehiclemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                            sheet = 'DiveMetadata')

##### check #####
names(events)
# names(corals)
# names(geology)
# unique(divevehiclemetadata$CruiseID)
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
# names(corals)
# names(fish)
# names(events)
# table(events$EventType)

##### get VehicleName from divevehiclemetadata table #####
vehiclename <- divevehiclemetadata %>% filter(CruiseID == unique(corals$CruiseId)) %>% pull(VehicleName) %>% unique()
vessel <- divevehiclemetadata %>% filter(CruiseID == unique(corals$CruiseId)) %>% pull(CruiseName) %>% unique()

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
##### join the General Locality information #####
locality_table <- divevehiclemetadata %>% filter(CruiseID == unique(corals$CruiseId)) %>% select(DiveID, GeneralLocation)
result <- merge(result, locality_table, by.x= 'DiveId', by.y = 'DiveID')

##### ***** #####
##### main crosswalk and transformations to create export #####
dscrtp_export <- result %>%
  mutate(
    ImageFilePath = paste(MediaId, Frame, TatorId, sep = '_'), ## this will need to be adapted when we get the media in house
    SampleAreaInSquareMeters = SampleAreaInSquareMeters.x,
    ScientificName = ScientificName.x,
    Morphospecies = paste(Morphospecies.x, Color.x, sep = ' | '),
    IdentifiedBy = paste(IdentifiedBy, ReviewedBy, sep = ' | '),
    IdentificationDate = IdentificationDate,
    IdentificationComments = paste(IdentificationComments, AnnotatorResponse, OccurrenceComments.x, sep = ' | '),
    OccurrenceComments = paste(OccurrenceComments.x, sep = ' | '),
    Condition = Condition.x,
    Longitude = Longitude,
    Latitude = Latitude,
    ObservationDate = Timestamp,
    Locality = GeneralLocation,
    MinimumDepthInMeters = NavDepth,
    MaximumDepthInMeters = NavDepth,
    DepthInMeters = NavDepth,
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
# dscrtp_export %>% pull(MinimumSize) %>% table(useNA = 'always')
# dscrtp_export %>% pull(MaximumSize) %>% table(useNA = 'always')
# dscrtp_export %>% pull(Condition) %>% unique()
# dscrtp_export %>% pull(Condition) %>% table(useNA = 'always')
# dscrtp_export %>% pull(CategoricalAbundance) %>% table(useNA = 'always')
# dscrtp_export %>% pull(IdentifiedBy) %>% table(useNA = 'always')
# dscrtp_export %>% pull(Latitude) %>% table(useNA = 'always')
# dscrtp_export %>% pull(Longitude) %>% table(useNA = 'always')
# dscrtp_export %>% pull(ScientificName) %>% table(useNA = 'always')
# str(dscrtp_export)
#
#
# dscrtp_export %>% pull(IdentificationComments) %>% table()
# dscrtp_export$IdentificationComments2 <- sub(
#   " \\| NA \\| NA$", "", dscrtp_export$IdentificationComments
#   )
# dscrtp_export$IdentificationComments3 <- sub(
#   " \\| NA$", "", dscrtp_export$IdentificationComments2
# )
#
# dscrtp_export$IdentificationComments4 <- sub(
#   "NA \\| ", "", dscrtp_export$IdentificationComments3
# )
#
#
#
# dscrtp_export %>%
#   pull(IdentificationComments4) %>%
#   unique()
#
# dscrtp_export %>%
#   pull(IdentifiedBy) %>%
#   unique()
#
# corals %>% pull(IdentifiedBy) %>% unique()

##### correct NA values in IdentificationComments #####
dscrtp_export$IdentificationComments2 <- sub(
  " \\| NA \\| NA$", "", dscrtp_export$IdentificationComments
)
dscrtp_export$IdentificationComments3 <- sub(
  " \\| NA$", "", dscrtp_export$IdentificationComments2
)

dscrtp_export$IdentificationComments4 <- sub(
  "NA \\| ", "", dscrtp_export$IdentificationComments3
)

dscrtp_export$IdentificationComments <- dscrtp_export$IdentificationComments4

## cleanup
dscrtp_export <- dscrtp_export %>% select(c(-IdentificationComments2, -IdentificationComments3, -IdentificationComments4))

##### correct NA values in IdentifiedBy #####
dscrtp_export$IdentifiedBy2 <- sub(
  " \\| NA \\| NA$", "", dscrtp_export$IdentifiedBy
)
dscrtp_export$IdentifiedBy3 <- sub(
  " \\| NA$", "", dscrtp_export$IdentifiedBy2
)

dscrtp_export$IdentifiedBy4 <- sub(
  "NA \\| ", "", dscrtp_export$IdentifiedBy3
)

dscrtp_export$IdentifiedBy <- dscrtp_export$IdentifiedBy4

## cleanup
dscrtp_export <- dscrtp_export %>% select(c(-IdentifiedBy2, -IdentifiedBy3, -IdentifiedBy4))

##### correct NA values in Morphospecies #####
dscrtp_export$Morphospecies2 <- sub(
  " \\| NA \\| NA$", "", dscrtp_export$Morphospecies
)
dscrtp_export$Morphospecies3 <- sub(
  " \\| NA$", "", dscrtp_export$Morphospecies2
)

dscrtp_export$Morphospecies4 <- sub(
  "NA \\| ", "", dscrtp_export$Morphospecies3
)

dscrtp_export$Morphospecies <- dscrtp_export$Morphospecies4

## cleanup
dscrtp_export <- dscrtp_export %>% select(c(-Morphospecies2, -Morphospecies3, -Morphospecies4))


##### correct NA values in CMECSGeoForm #####
dscrtp_export$CMECSGeoForm2 <- sub(
  " \\| NA \\| NA$", "", dscrtp_export$CMECSGeoForm
)
dscrtp_export$CMECSGeoForm3 <- sub(
  " \\| NA$", "", dscrtp_export$CMECSGeoForm2
)

dscrtp_export$CMECSGeoForm4 <- sub(
  "NA \\| ", "", dscrtp_export$CMECSGeoForm3
)


dscrtp_export$CMECSGeoForm <- dscrtp_export$CMECSGeoForm4

## cleanup
dscrtp_export <- dscrtp_export %>% select(c(-CMECSGeoForm2, -CMECSGeoForm3, -CMECSGeoForm4))


##### add some metadata (Manual) ######
dscrtp_export$DataProvider <- "NOAA, Mesophotic Deep Benthic Communities Restoration Project"
dscrtp_export$Vessel <- 'Pisces R/V'
dscrtp_export$VehicleName <- vehiclename
dscrtp_export$Locality <- 'Gulf of America (formerly Gulf of Mexico)'
dscrtp_export$RecordType <- 'video observation'
dscrtp_export$Modified <- '2025-03-05'
dscrtp_export$DataContact <- 'Bassett, Rachel | rachel.bassett@noaa.gov'
dscrtp_export$Reporter <- 'Bassett, Rachel | rachel.bassett@noaa.gov'
dscrtp_export$DatasetID <- paste('NOAA_', unique(dscrtp_export$SurveyID),'_MDBC', sep = '')
dscrtp_export$PI <- 'Bassett, Rachel | rachel.bassett@noaa.gov'
dscrtp_export$DepthMethod <- 'reported'

##### make Citation #####
dscrtp_export$Citation <- paste(dscrtp_export$DataProvider,'. ','Observation date range: ',
                           min(dscrtp_export$ObservationDate[dscrtp_export$ObservationDate != "-999"]),' to ',
                           max(dscrtp_export$ObservationDate[dscrtp_export$ObservationDate != "-999"]),'. ',
                           'Coral or sponge occurrence observations submitted to the NOAA National Database for Deep Sea Corals and Sponges (www.deepseacoraldata.noaa.gov)', '. ',
                           #'DSCRTP Accession ID: ',dscrtp_export$AccessionID, '. ',
                           #'Record type: ', dscrtp_export$RecordType, '. ',
                           'Vessel(s): ', dscrtp_export$Vessel,'. ',
                           'Sampling vehicle: ', dscrtp_export$VehicleName,'. ',
                           'Survey ID: ', dscrtp_export$SurveyID,'. ',
                           'DSCRTP Dataset ID: ', dscrtp_export$DatasetID, '. ',
                           'Principal investigator: ', dscrtp_export$PI,'. ',
                           'Reporter: ', dscrtp_export$Reporter, '. ',
                           'Database version: ', unique(filt$DatabaseVersion), '. ',
                           #'Data contact: ', dscrtp_exportDataContact,'. ',
                           #'Reporter: ', dscrtp_exportReporter,'. ',
                           #'Repository: ', dscrtp_exportRepository,'. ',
                           # 'Web site [last accessed on YYYY-MM-DD]: ', dscrtp_exportWebSite,'.',
                           sep = '')

##### check #####

# unique(dscrtp_export$CitationMaker)
# filt %>% filter(grepl("PC", SurveyID)) %>% pull(Vessel) %>% table()
# st#r(dscrtp_export)
# dscrtp_export %>% pull(MinimumDepthInMeters) %>% is.na() %>% table()
# dscrtp_export %>% pull(MaximumDepthInMeters) %>% is.na() %>% table()
# dscrtp_export %>% pull(SurveyID) %>% is.na() %>% table()
dscrtp_export %>% pull(IdentifiedBy) %>% table()
dscrtp_export %>% pull(OccurrenceComments) %>% table()

# dscrtp_export %>% pull(IdentificationDate) %>% is.na() %>% table()
# dscrtp_export %>% pull(Condition) %>% table(useNA = 'always')
# dscrtp_export %>% pull(Habitat) %>% table(useNA = 'always')  %>% sort()
# dim(dscrtp_export)

#
#
# x <- s %>% filter(PointNew == "R") %>% pull(FieldName)
# setdiff(names(dscrtp_export), x)
# setdiff(x,names(dscrtp_export))

# dscrtp_export %>% pull(CategoricalAbundance) %>% table() %>% sort()
# dscrtp_export %>% pull(IndividualCount) %>% table() %>% sort()

##### ***** #####
##### write the file to disk #####
## get rid of .xlsx at the end of the input file name
x <- str_remove(tatorexport, "\\.xlsx$")

## write
write.csv(dscrtp_export,
          paste('c:/rworking/deepseatools/indata/',
                '20250306-0_',
                unique(dscrtp_export$DatasetID),
                '.csv',
                sep = ''),
          row.names = F)



