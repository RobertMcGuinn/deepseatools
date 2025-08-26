##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240308
## purpose: dscrtp crosswalk with tator exports
## issuename: 20250306-0_NOAA_PC2202L1_MDBC_143699

##### linkage #####
filename <- '143699' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(openxlsx)
library(googlesheets4)
library(googledrive)
library(fuzzyjoin)
library(lubridate)
library(worrms)
library(googlesheets4)
library(taxize)

##### authorizations #####
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### load database #####
source('C:/rworking/deepseatools/code/mod_load_current_ndb.R')


##### ***** ORIGINAL ***** #####
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
## version 20250516

filename <- 'SAMPLEMETADATA.xlsx'
samplemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                     sheet = 'BiologicalSamples')

filename <- 'DIVEVEHICLEMETADATA.xlsx'
divevehiclemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                            sheet = 'DiveMetadata')

##### check #####
# names(events)
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
dscrtp_export %>% pull(IdentifiedBy) %>% table()
dscrtp_export %>% pull(OccurrenceComments) %>% table()

dscrtp_export %>% pull(Habitat) %>% table(useNA = 'always')  %>% sort()
dim(dscrtp_export)

dscrtp_export %>% select(Habitat, CMECSGeoForm) %>% distinct() %>%  View()

# x <- s %>% filter(PointNew == "R") %>% pull(FieldName)
# setdiff(names(dscrtp_export), x)
# setdiff(x,names(dscrtp_export))

# dscrtp_export %>% pull(CategoricalAbundance) %>% table() %>% sort()
# dscrtp_export %>% pull(IndividualCount) %>% table() %>% sort()

##### ***** #####
##### write the file to disk #####
## get rid of .xlsx at the end of the input file name
# x <- str_remove(tatorexport, "\\.xlsx$")
#
# ## write
# write.csv(dscrtp_export,
#           paste('c:/rworking/deepseatools/indata/',
#                 '20250306-0_',
#                 unique(dscrtp_export$DatasetID),
#                 '.csv',
#                 sep = ''),
#           row.names = F)








##### ***** NEW VERSION ***** #####
##### load dataset from CSV #####
setwd('c:/rworking/deepseatools/indata')
filename <- ''
sub <- read.csv(paste(filename, '.csv', sep=''))
# View(sub)

##### explore #####
# length(sub$SampleID)
# length(unique(sub$SampleID))
# dim(sub)
# summary(sub)
# names(sub)
# table(sub$ImageFilePath, useNA = 'always')
# table(sub$ObservationDate, useNA = 'always')
# table(sub$Modified, useNA = 'always')
# table(sub$DataProvider, useNA = 'always')
# table(sub$SurveyID, useNA = 'always')
# table(sub$Vessel, useNA = 'always')
# table(sub$EventID, useNA = 'always')
# table(sub$NavType, useNA = 'always')
# table(sub$LocationAccuracy, useNA = 'always')
# table(sub$EndLatitude, useNA = 'always')
# table(sub$StartLatitude, useNA = 'always')
# table(sub$EndLongitude, useNA = 'always')
# table(sub$StartLongitude, useNA = 'always')
# table(sub$Longitude, useNA = 'always')
# table(sub$Latitude, useNA = 'always')
# table(sub$Locality, useNA = 'always')
# table(sub$DepthInMeters, useNA = 'always')
# table(sub$MinimumDepthInMeters, useNA = 'always')
# table(sub$MaximumDepthInMeters, useNA = 'always')
# table(sub$DepthMethod, useNA = "always")
# table(sub$ScientificName, useNA = "always")
# table(sub$AphiaID, useNA = "always")
# table(sub$Class)


# table(sub$ScientificName, useNA = 'always')
# table(sub$AphiaID, useNA = 'always')
# table(sub$RecordType, useNA = 'always')
# table(sub$VerbatimSize, useNA = 'always')


# table(is.na(sub$Latitude))
# table(is.na(sub$Longitude))
# table(is.na(sub$SampleID))

# head(sub$SampleID)
# head(sub$TrackingID)
#
# table(is.na(sub$TrackingID))
# table(is.na(sub$Condition))

# unique(grep("OET", filt$DatasetID, value = TRUE))
# table(unique(sub$ObservationDate))
# filt %>% filter(grepl("Fulmar", Vessel)) %>% select(DatasetID, Vessel, SurveyID) %>% distinct()
# filt %>% filter(grepl("CBNMS", DatasetID)) %>% select(DatasetID, Vessel, SurveyID) %>% distinct()

##### create vector from incoming AphiaIDs #####
my_vector <- unique(sub$AphiaID)
# remove any missing value.
my_vector <- my_vector[complete.cases(my_vector)]

##### check #####
#length(my_vector)

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list_original <- df

##### check #####
# dim(species_list_original)
# View(species_list_original)
#
# table(species_list_original$AphiaID, useNA = 'always')
#
# table(sub$AphiaID, useNA = 'always')
#
# sub %>% filter(AphiaID == -999) %>% pull(ScientificName)
#
# species_list_original %>% filter(is.na(AphiaID) == T) %>%
#   pull(scientificname)
# table(sub$AphiaID, useNA = 'always')
# setdiff(species_list_original$AphiaID, sub$AphiaID)
# setdiff(sub$AphiaID, species_list_original$AphiaID)
# View(species_list_original)
# table(species_list_original$status, useNA = 'always')
# species_list_original %>% filter(status != 'accepted') %>% View()

##### create a complete valid AphiaID list #####
species_list_original <- species_list_original %>%
  mutate(valid_AphiaID_complete = ifelse(is.na(valid_AphiaID) == T,
                                         AphiaID,
                                         valid_AphiaID))

##### check #####
species_list_original %>% filter(status != 'accepted') %>%
  group_by(AphiaID, valid_AphiaID, valid_AphiaID_complete) %>%
  summarize(n=n()) %>% View()

##### create vector from valid AphiaIDs #####
my_vector <- unique(species_list_original$valid_AphiaID_complete)

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by the valid AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list <- df

##### check #####
View(species_list)
table(is.na(species_list$AphiaID))
table(species_list$status)
dim(species_list)

##### loop to get classification #####
df <- data.frame(
  Domain = character(),
  Kingdom = character(),
  Subkingdom = character(),
  Phylum = character(),
  Subphylum = character(),
  Superclass = character(),
  Class = character(),
  Subclass = character(),
  Infraclass = character(),
  Superorder = character(),
  Order = character(),
  Suborder = character(),
  Infraorder = character(),
  Superfamily = character(),
  Family = character(),
  Subfamily = character(),
  Tribe = character(),
  Subtribe = character(),
  Genus = character(),
  Subgenus = character(),
  Species = character(),
  Subspecies = character(),
  Variety = character(),
  stringsAsFactors=FALSE)

## loop to get full classification
for (i in my_vector){
  try(classification <- wm_classification(i))
  classification_wide <- classification %>%
    select(rank,scientificname) %>%
    pivot_wider(
      names_from = rank,
      values_from = scientificname
    )
  classification_wide$AphiaID <- i
  df <- bind_rows(df, classification_wide)
}

classification <- df

##### loop to get vernacular name #####
## initialize an empty list to store successful results
result_list <- list()

## loop through my_vector
for (i in my_vector) {
  ## use try() to handle errors
  vernaculars <- try({
    wm_result <- wm_common_id(i)
    wm_result %>% filter(language == 'English')
  }, silent = TRUE) ## Use silent = TRUE to suppress error messages

  ## check if there was an error in wm_common_id()
  if (!inherits(vernaculars, "try-error")) {
    ## If no error, proceed to append the data to the result_list
    vernaculars_list <- paste(vernaculars$vernacular, collapse = " | ")
    AphiaID <- i
    vernaculars_wide <- data.frame(AphiaID, vernaculars_list)
    result_list[[length(result_list) + 1]] <- vernaculars_wide
  }
}

## Combine the successful results into the final data frame df
df <- do.call(rbind, result_list)
vernaculars <- df

##### check #####
# wm_common_id(1567760)

##### loop to get synonyms #####
## initialize a results list
result_list <- list()

## loop through my_vector
for (i in my_vector) {
  ## use try() to handle errors
  synonyms <- try({
    wm_result <- wm_synonyms(i) # i = 423632
  }, silent = TRUE) # Use silent = TRUE to suppress error messages

  ## check if there was an error in wm_synonyms()
  if (!inherits(synonyms, "try-error")) {
    ## if no error, proceed to append the data to the result_list
    synonyms_list <- paste(synonyms$scientificname, collapse = " | ")
    AphiaID <- i
    synonyms_wide <- data.frame(AphiaID, synonyms_list)
    result_list[[length(result_list) + 1]] <- synonyms_wide
  }
}

## Combine the successful results into the final data frame df
df <- do.call(rbind, result_list)
synonyms <- df

##### check #####
# View(classification)
# View(vernaculars)
# View(classification)
# View(synonyms)
#
# dim(species_list)
# dim(classification)
# dim(vernaculars)
# dim(synonyms)
#
# names(species_list)
# names(classification)
# names(vernaculars)
# names(synonyms)
#
# head(species_list$AphiaID)
# head(classification$AphiaID)
# head(vernaculars$AphiaID)
# head(synonyms$AphiaID)
#
# tail(tax_tom_enhanced$AphiaID)
# tail(classification$AphiaID)
# tail(vernaculars$AphiaID)
# tail(synonyms$AphiaID)
#
# class(tax_tom_enhanced$AphiaID)
# class(classification$AphiaID)
# class(vernaculars$AphiaID)
# class(synonyms$AphiaID)

##### left join the summary from above with all of the other API tables #####
by <- join_by(valid_AphiaID == AphiaID)
joined2 <- left_join(species_list, classification, by)

by <- join_by(valid_AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

by <- join_by(valid_AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

##### check #####
# names(joined4)
# setdiff(joined4$AphiaID, species_list_original$valid_AphiaID_complete)
# setdiff(species_list_original$valid_AphiaID_complete, joined4$AphiaID)

##### join original table with the new table #####
joined4$AphiaID2 <- joined4$AphiaID
by <- join_by(valid_AphiaID_complete == AphiaID2)
taxonomy_table <- left_join(species_list_original, joined4, by)
# View(taxonomy_table)
# names(taxonomy_table)

##### join taxonomy to sub #####
by <- join_by(AphiaID == AphiaID.x)
sub_enhanced <- left_join(sub, taxonomy_table, by)

##### check #####
# sub_enhanced %>% filter(is.na(phylum.y) == T) %>%
#   pull(ScientificName) %>%
#   unique()
#
# dim(sub)
# dim(sub_enhanced)

##### gather information into proper variables #####
sub_enhanced$VerbatimScientificName <- sub$ScientificName
sub_enhanced$ScientificName <- sub_enhanced$scientificname.y
sub_enhanced$VernacularName <- sub_enhanced$vernaculars_list
sub_enhanced$TaxonRank <- sub_enhanced$rank.y
sub_enhanced$AphiaID <- sub_enhanced$valid_AphiaID_complete
sub_enhanced$Phylum <- sub_enhanced$phylum.y
sub_enhanced$Class <- sub_enhanced$Class.y
sub_enhanced$Subclass <- sub_enhanced$Subclass.y
sub_enhanced$Order <- sub_enhanced$Order.y
sub_enhanced$Suborder <- sub_enhanced$Suborder.y
sub_enhanced$Family <- sub_enhanced$Family.y
sub_enhanced$Subfamily <- sub_enhanced$Subfamily.y
sub_enhanced$Genus <- sub_enhanced$Genus.y
sub_enhanced$Subgenus <- sub_enhanced$Subgenus.y
sub_enhanced$Species <- word(sub_enhanced$Species.y, -1)
sub_enhanced$Subspecies <- sub_enhanced$Subspecies.y
sub_enhanced$ScientificNameAuthorship <- sub_enhanced$authority.y
sub_enhanced$Synonyms <- sub_enhanced$synonyms_list

##### add a variable #####
sub_enhanced$IdentificationComments <- sub_enhanced$VernacularNameCategory

##### check #####
# table(sub_enhanced$Phylum, useNA = 'always')
# table(x$Phylum, useNA = 'always')
# table(sub_enhanced_filter$Class, useNA = 'always')
# sub_enhanced_filter %>% filter(Class == 'Hydrozoa') %>%
#   group_by(Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()

##### apply taxonomic filter #####
sub_enhanced_filter <- sub_enhanced %>%
  filter(Subphylum == 'Vertebrata' |
           Phylum == 'Cnidaria' |
           Phylum == 'Porifera')

`%notin%` <- Negate(`%in%`)
sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Class %notin% c('Scyphozoa', 'Thalicacea', 'Ascidiacea'))

sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Order == 'Scleractinia' |
           Order == 'Antipatharia' |
           Genus == 'Savalia' |
           Genus == 'Kulamanamana' |
           Genus == 'Gerardia' |
           Family == 'Stylasteridae' |
           Order  == 'Alcyonacea' |
           Order ==  'Gorgonacea' |
           Order ==  'Helioporacea' |
           Order == 'Pennatulacea' |
           Order == 'Scleralcyonacea' |
           Family == 'Stylasteridae' |
           Genus == 'Solanderia' |
           Genus == 'Janaria' |
           Genus == 'Hydrocorella' |
           Genus == 'Hydrodendron' |
           Phylum == 'Chordata' |
           Phylum == 'Porifera' |
           Order == 'Malacalcyonacea'
  )


##### check #####
# sub_enhanced %>% filter(ScientificName == "Anthozoa") %>% pull(ScientificName)
# sub_enhanced_filter %>% filter(ScientificName == "Anthozoa") %>% pull(ScientificName)
# filt %>% filter(ScientificName == "Anthozoa") %>% pull(ScientificName)
#
# table(sub_enhanced_filter$Phylum, useNA = 'always')
# table(sub_enhanced_filter$Subphylum, useNA = 'always')
#
# sub_enhanced_filter %>%
#   group_by(AphiaID, Phylum, Subphylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()

##### assign VernacularNameCategory #####
## define not in
`%notin%` <- Negate(`%in%`)

gorgfamilies <- c("Chrysogorgiidae","Dendrobrachiidae",
                  "Ellisellidae", "Isididae",
                  "Pleurogorgiidae", "Primnoidae",
                  "Acanthogorgiidae", "Gorgoniidae","Keroeididae",
                  "Plexauridae", "Anthothelidae",
                  "Coralliidae", "Melithaeidae",
                  "Paragorgiidae", "Parisididae","Spongiodermidae", "Subergorgiidae",
                  "Victorgorgiidae", "Keratoisididae", "Malacalcyonacea incertae sedis")

softfamilies <- c("Alcyoniidae","Aquaumbridae", "Ifalukellidae",
                  "Nephtheidae","Nidaliidae", "Paralcyoniidae",
                  "Xeniidae", "Taiaroidae")

othercorallikehydrozoanfamilies <- c("Solanderiidae", "Haleciidae")

stonycoralbranching <- tax %>%
  filter(VernacularNameCategory == 'stony coral (branching)') %>%
  pull(ScientificName)

stonycoralcupcoral <- tax %>%
  filter(VernacularNameCategory == 'stony coral (cup coral)') %>%
  pull(ScientificName)

sub_enhanced2 <- sub_enhanced_filter %>%
  mutate(VernacularNameCategory = case_when(
    Phylum %in% c('Chordata') ~ 'fish',
    TaxonRank %in% c('Order') &
      Order %in% c('Alcyonacea') ~ 'alcyonacean (unspecified)',
    Order %in% c('Antipatharia') ~ 'black coral',
    Class %in% c('Calcarea')~ 'calcareous sponge',
    Class %in% c('Demospongiae') ~ 'demosponge',
    Class %in% c('Hexactinellida') ~ 'glass sponge',
    Class %in% c('Homoscleromorpha') ~ 'homoscleromorph sponge',
    Family %in% c('Parazoanthidae') ~ 'gold coral',
    Family %in% gorgfamilies ~ 'gorgonian coral',
    Family %in% softfamilies ~ 'soft coral',
    Order %in% c('Anthoathecata') &
      Family %notin%  c('Solanderiidae') ~ 'lace coral',
    Family %in% c('Lithotelestidae') ~ 'lithotelestid coral',
    Family %in% othercorallikehydrozoanfamilies ~ 'other coral-like hydrozoan',
    Superfamily %in% c('Pennatuloidea') ~ 'sea pen',
    ScientificName %in% c('Porifera') ~ 'sponge',
    Suborder %in% c('Stolonifera') ~ 'stoloniferan coral',
    Genus %in% c('Clavularia') ~ 'stoloniferan coral',
    Order %in% c('Scleractinia') &
      TaxonRank %in% c('Order')  ~ 'stony coral (unspecified)',
    ScientificName %in% stonycoralbranching ~ 'stony coral (branching)',
    ScientificName %in% stonycoralcupcoral ~ 'stony coral (cup)',
    Genus %in% c('Acanthogorgia') ~ 'gorgonian coral',
    Genus %in% c('Hydrodendron') ~ 'other coral-like hydrozoan',
    Genus %in% c('Caryophyllia') ~ 'stony coral (cup coral)',
    TRUE ~ ''))

##### check #####
# table(sub_enhanced2$VernacularNameCategory, useNA = 'always')
# filt %>% filter(Order == 'Malacalcyonacea') %>% pull(VernacularNameCategory) %>%
#   table(useNA = 'always')
#
# filt %>% filter(Genus == 'Clavularia') %>% pull(VernacularNameCategory) %>%
#   table(useNA = 'always')
#
# sub_enhanced2 %>%
#   filter(VernacularNameCategory == '') %>%
#   pull(ScientificName) %>% unique()
#
# sub_enhanced2 %>%
#   filter(VernacularNameCategory == 'stony coral (cup)') %>%
#   pull(VernacularNameCategory) %>% unique()

##### get rid of unneeded column names #####
names_list <- names(sub)
sub_enhanced2 <- sub_enhanced2 %>%
  dplyr::select(all_of(names_list))

##### select just the taxonomic variables #####
sub_enhanced3<- sub_enhanced2 %>%
  select(CatalogNumber,
         VerbatimScientificName,
         ScientificName,
         VernacularName,
         VernacularNameCategory,
         TaxonRank,
         AphiaID,
         Phylum,
         Class,
         Subclass,
         Order,
         Suborder,
         Family,
         Subfamily,
         Genus,
         Subgenus,
         Species,
         Subspecies,
         ScientificNameAuthorship,
         Synonyms,
         IdentificationComments)

##### check #####
sub_enhanced3$IdentificationComments
# sub_enhanced3$VernacularNameCategory
# table(sub_enhanced3$VernacularNameCategory, useNA = 'always')
# View(sub_enhanced3)
# dim(sub_enhanced3)
# dim(sub)
# length(sub$CatalogNumber) - length(sub_enhanced3$CatalogNumber)
#
x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
sub %>% filter(CatalogNumber %in% x) %>%
  group_by(CatalogNumber,
           VerbatimScientificName,
           ScientificName,
           VernacularNameCategory) %>%
  summarize(n=n()) %>% View()

filt %>% filter(ScientificName == 'Callistephanus') %>%
  pull(VernacularNameCategory) %>% table()

x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
sub_enhanced %>% filter(CatalogNumber %in% x) %>%
  group_by(AphiaID, Phylum, Class, Order, Suborder, Family, Genus, Species) %>%
  summarize(n=n()) %>% View()
#
#
# table(is.na(sub$CatalogNumber))
# table(is.na(sub_enhanced3$CatalogNumber))
# sub %>% filter(ScientificName == 'Dichotella gemmacea') %>% pull(AphiaID)
# 'Dichotella gemmacea'
#
# x <- setdiff(sub_enhanced3$VerbatimScientificName, sub_enhanced3$ScientificName)
# sub_enhanced3 %>% filter(VerbatimScientificName %in% x) %>%
#   group_by(VerbatimScientificName, ScientificName, VernacularNameCategory) %>%
#   summarize(n=n()) %>% View()
#
# x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
# sub %>% filter(CatalogNumber %in% x) %>% pull(AphiaID)
#
# table(sub_enhanced3$VernacularNameCategory, useNA = 'always')
#
# sub_enhanced3 %>% filter(VernacularNameCategory == '') %>% pull(Order) %>% unique()
#
# sub_enhanced3 %>% filter(VernacularNameCategory == '') %>%
#   group_by(AphiaID, Phylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()
#
# sub_enhanced3 %>%
#   group_by(AphiaID, Phylum, Class, Order, Family, Genus, Species, ScientificNameAuthorship) %>%
#   summarize(n=n()) %>% View()

View(sub_enhanced3)

##### write result to csv (export to CSV) #####
filename_patch <- paste(filename, '_taxonomy_patch', '.csv',sep = '')
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename_patch, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))


##### ***** NEW VERSION  *****  #####
##### load data #####
setwd('c:/rworking/deepseatools/indata')
filename <- ''
sub <- read.csv(paste(filename, '.csv', sep = ''))

##### check #####
table(sub$Flag)
sub %>% filter(Flag == 0) %>% group_by(VernacularNameCategory, Phylum, Class, Order, Family, ScientificName) %>%
  summarize(n=n()) %>% View()
table(sub$IndividualCount, useNA = 'always')
filt %>% filter(grepl('Eiwa', VehicleName)) %>% pull(VehicleName) %>% table()
sub %>% filter(grepl('Eiwa', VehicleName)) %>% pull(VehicleName) %>% table()
filt %>% filter(grepl('NOAA_SH-22-09', DatasetID)) %>% pull(VehicleName) %>% table()
filt %>% filter(grepl('SH', SurveyID)) %>% pull(SurveyID) %>% table()


##### run QA report #####
## manual change version of dashboard version number is required
rmarkdown::render("C:/rworking/deepseatools/code/20240320-0_rmd_accession_qa_dashboard.Rmd",
                  output_file =  paste(filename,".docx", sep=''),
                  output_dir = 'C:/rworking/deepseatools/reports')

## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1cGC8rQoRdS_xsmYfx7hGGnx23q3VCX3Z"
setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)




























##### ***** NEW ORIGINAL ***** #####
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
tatorexport <- 'PC2202L1_Forward Videos_2025-06-15_report_tjeXMcPuyr.xlsx'

corals <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                    sheet = 'corals_inverts')

fish <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                  sheet = 'fish')

events <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                    sheet = 'events')

geology <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                     sheet = 'geology')

##### load data from SQL files #####
## version 20250516
## see Google Drive: https://drive.google.com/drive/folders/1P8sRAMh2gTHtUkmIBXclMwz_4QUycvbJ

filename <- 'SAMPLEMETADATA.xlsx'
samplemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                            sheet = 'BiologicalSamples')

filename <- 'DIVEVEHICLEMETADATA.xlsx'
divevehiclemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                                 sheet = 'DiveMetadata')

##### check #####
# names(events)
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
dscrtp_export %>% pull(Temperature) %>% table()
dscrtp_export %>% pull(Salinity) %>% table()
dscrtp_export %>% pull(DepthInMeters) %>% table(useNA = 'always')
dscrtp_export %>% pull(IdentifiedBy) %>% table()
dscrtp_export %>% pull(OccurrenceComments) %>% table()
dscrtp_export %>% pull(Habitat) %>% table(useNA = 'always')  %>% sort()
dim(dscrtp_export)

dscrtp_export %>% select(Habitat, CMECSGeoForm) %>% distinct() %>%  View()

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
                '20250617-0_',
                unique(dscrtp_export$DatasetID),
                '.csv',
                sep = ''),
          row.names = F)



##### ***** NEW VERSION 20250618-2***** #####
##### load dataset from CSV #####
setwd('c:/rworking/deepseatools/indata/')
filename <- '20250618-2_NOAA_PC2202L1_MDBC_143699'
sub <- read.csv(paste(filename, '.csv', sep=''))
# View(sub)

##### load the most current taxonomy from Google Sheets #####
# https://drive.google.com/open?id=0B9c2c_XdhpFBT29NQmxIeUQ4Tlk
## manual: make sure the IDs below are pointing at the correct sheets
tax <- read_sheet('1v3yZO7ATMtV-wp9lePl2pV9-ycxFo3VGVrR_SIunbdQ')
taxfl <- read_sheet('1ZfR4wiBQbDsFGpYXXDjHrsF1QJyoCMqfocmxbpBPo9M')
taxch <- read_sheet('11FgDuNmIZRSf2W4MeFqn2h8pOekvQEP2nG4vcy46pY8')

##### make taxonomic changes to incoming (manual: specific to each new dataset) #####
## filter out these names
sub1 <- sub %>% filter(ScientificName != 'Not Set')

## change these taxa
sub2 <- sub1  %>%
  mutate(ScientificName = str_replace(ScientificName, "Stichopathes sp", "Stichopathes")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Nicella Sp.", "Nicella")) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Agelas cf. flabelliformis', 'Agelas')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Davidastar discoideus', 'Davidaster discoideus')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Ircinidae', 'Irciniidae')) %>%
  mutate(ScientificName = str_replace(ScientificName, '\\bOctocoral\\b', 'Octocorallia')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Crustose Coralline Algae', 'Rhodophyta')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Cladocora caespitosa', 'Scleractinia')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Eel', 'Anguilliformes'))

##### check #####
# sub %>% filter(ScientificName == 'Octocoral') %>% dim()

filt %>% filter(ScientificName == 'Scleralcyonacea') %>% pull(VernacularNameCategory)

##### create vector of names #####
my_vector <- unique(sub2$ScientificName)
my_vector <- my_vector[complete.cases(my_vector)]

##### parse the list using taxize function 'gbif_parse' #####
parsed_list <- gbif_parse(my_vector)

## get only unique parsed names
parsed_list <- distinct(parsed_list)

## View(parsed_list)
my_vector_parsed <- parsed_list$canonicalname

## get only unique names
my_vector_parsed <- unique(my_vector_parsed)

##### check #####
my_vector_parsed
sort(my_vector_parsed)
sort(my_vector)

##### make groups of 50 (because the API limit is 50) #####
my_groups <- split(my_vector_parsed, ceiling(seq_along(my_vector)/50))

##### loop to get records by names list #####
## run this just once to get the proper data structure for an empty dataframe
species_list <- wm_records_name("Octocorallia", fuzzy = F)

## initiate the empty data frame
df <- species_list[0,]

## loop to get WoRMS records from names (b)
for (i in seq_along(my_groups)){
  species_list <- wm_records_names(name = my_groups[[i]],
                                   fuzzy = F,
                                   marine_only = T,

  )
  species_list <- do.call("rbind", species_list)
  df <- rbind(df, species_list)
}
species_list <- df

## get rid of any extinct matches
species_list <- species_list %>%
  filter(isExtinct == 0 |
           is.na(isExtinct) == T)

## get just the data that are distinct
species_list <- distinct(species_list)

##### check #####
# dim(species_list)
# View(species_list)

##### left join the parsed list #####
by <- join_by(canonicalname == scientificname)
joined <- left_join(parsed_list, species_list, by)

##### create a summary joined file #####
summary <- joined %>%
  group_by(status,
           phylum,
           scientificname,
           canonicalname,
           valid_name,
           valid_AphiaID,
           rank,
           authority) %>%
  summarize(n=n())

##### check: test for difficult taxa #####
summary$sametest <- ifelse(summary$canonicalname == summary$valid_name,"Yes","No")
changes <- summary %>% filter(sametest == "No") %>% pull(scientificname)
nomatch <- summary %>% filter(is.na(sametest) == T) %>% pull(scientificname)

changes
nomatch

##### check for duplicates #####
# names(joined4)
# Find duplicated values
duplicates <- summary$valid_AphiaID[duplicated(summary$valid_AphiaID)]
unique_duplicates <- unique(duplicates)
print(unique_duplicates)

##### get rid of NA #####
summary <- summary %>% filter(is.na(valid_AphiaID) == F)

##### create vector from valid AphiaIDs #####
my_vector <- unique(summary$valid_AphiaID)

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list <- df

##### loop to get classification #####
df <- data.frame(
  Domain = character(),
  Kingdom = character(),
  Subkingdom = character(),
  Phylum = character(),
  Subphylum = character(),
  Superclass = character(),
  Class = character(),
  Subclass = character(),
  Infraclass = character(),
  Superorder = character(),
  Order = character(),
  Suborder = character(),
  Infraorder = character(),
  Superfamily = character(),
  Family = character(),
  Subfamily = character(),
  Tribe = character(),
  Subtribe = character(),
  Genus = character(),
  Subgenus = character(),
  Species = character(),
  Subspecies = character(),
  Variety = character(),
  stringsAsFactors=FALSE)

## loop to get full classification
for (i in my_vector){
  try(classification <- wm_classification(i))
  classification_wide <- classification %>%
    select(rank,scientificname) %>%
    pivot_wider(
      names_from = rank,
      values_from = scientificname
    )
  classification_wide$AphiaID <- i
  df <- bind_rows(df, classification_wide)
}

classification <- df

##### loop to get vernacular name #####
## initialize an empty list to store successful results
result_list <- list()

## loop through my_vector
for (i in my_vector) {
  ## use try() to handle errors
  vernaculars <- try({
    wm_result <- wm_common_id(i)
    wm_result %>% filter(language == 'English')
  }, silent = TRUE) ## Use silent = TRUE to suppress error messages

  ## check if there was an error in wm_common_id()
  if (!inherits(vernaculars, "try-error")) {
    ## If no error, proceed to append the data to the result_list
    vernaculars_list <- paste(vernaculars$vernacular, collapse = " | ")
    AphiaID <- i
    vernaculars_wide <- data.frame(AphiaID, vernaculars_list)
    result_list[[length(result_list) + 1]] <- vernaculars_wide
  }
}

## Combine the successful results into the final data frame df
df <- do.call(rbind, result_list)
vernaculars <- df

##### check #####
# wm_common_id(1567760)

##### loop to get synonyms #####
## initialize a results list
result_list <- list()

## loop through my_vector
for (i in my_vector) {
  ## use try() to handle errors
  synonyms <- try({
    wm_result <- wm_synonyms(i) # i = 423632
  }, silent = TRUE) # Use silent = TRUE to suppress error messages

  ## check if there was an error in wm_synonyms()
  if (!inherits(synonyms, "try-error")) {
    ## if no error, proceed to append the data to the result_list
    synonyms_list <- paste(synonyms$scientificname, collapse = " | ")
    AphiaID <- i
    synonyms_wide <- data.frame(AphiaID, synonyms_list)
    result_list[[length(result_list) + 1]] <- synonyms_wide
  }
}

## Combine the successful results into the final data frame df
df <- do.call(rbind, result_list)
synonyms <- df

##### check #####
# View(classification)
# View(vernaculars)
# View(classification)
# View(synonyms)
#
# dim(species_list)
# dim(classification)
# dim(vernaculars)
# dim(synonyms)
#
# names(species_list)
# names(classification)
# names(vernaculars)
# names(synonyms)
#
# head(species_list$AphiaID)
# head(classification$AphiaID)
# head(vernaculars$AphiaID)
# head(synonyms$AphiaID)
#
# tail(tax_tom_enhanced$AphiaID)
# tail(classification$AphiaID)
# tail(vernaculars$AphiaID)
# tail(synonyms$AphiaID)
#
# class(tax_tom_enhanced$AphiaID)
# class(classification$AphiaID)
# class(vernaculars$AphiaID)
# class(synonyms$AphiaID)


##### get rid of duplicates in each table #####
classification <- classification %>%
  mutate(across(everything(), as.character)) %>%
  distinct()
vernaculars <- vernaculars %>%
  mutate(across(everything(), as.character)) %>%
  distinct()
synonyms <- synonyms %>%
  mutate(across(everything(), as.character)) %>%
  distinct()
summary <- summary %>%
  mutate(across(everything(), as.character)) %>%
  distinct()

##### check #####
# duplicates <- classification_clean %>%
#   group_by(AphiaID) %>%
#   filter(n() > 1) %>%
#   ungroup()
# View(duplicates)
#
# duplicates <- summary %>%
#   group_by(valid_AphiaID) %>%
#   filter(n() > 1) %>%
#   ungroup()
#
# View(duplicates)

##### left join the summary from above with all of the other API tables #####
summary$valid_AphiaID <- as.integer(summary$valid_AphiaID)
classification$AphiaID <- as.integer(classification$AphiaID)

by <- join_by(valid_AphiaID == AphiaID)
joined2 <- left_join(summary, classification, by)
joined2 <- distinct(joined2) # get distinct records

vernaculars$AphiaID <- as.integer(vernaculars$AphiaID)
by <- join_by(valid_AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

synonyms$AphiaID <- as.integer(synonyms$AphiaID)
by <- join_by(valid_AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

##### clean up joined4 #####
joined4 <- joined4 %>%
  mutate(across(everything(), as.character)) %>%
  distinct()

joined4 <- joined4 %>% filter(phylum %in% c('Porifera', 'Cnidaria', 'Chordata'))
joined4 <- joined4 %>% filter(!(scientificname == 'Zoantharia' & valid_AphiaID == 1340))
joined4 <- joined4 %>% filter(!(scientificname == 'Chromis' & valid_AphiaID == 271096))
joined4 <- joined4 %>% filter(!(scientificname == 'Aplysina cauliformis' & valid_AphiaID == 166230))
joined4 <- joined4 %>% filter(!(scientificname == 'Seriola' & valid_AphiaID == 131995))

##### check #####
# sub2[11388,c('ScientificName')]
# sort(joined4$scientificname)
# joined4 %>% filter(scientificname == 'Seriola') %>% View()

##### ***** add taxonomy to sub ***** #####
by <- join_by(ScientificName == scientificname)
sub_enhanced <- left_join(sub2, joined4, by)

##### check #####
# sub_enhanced %>% filter(is.na(phylum) == T) %>%
#   pull(ScientificName) %>%
#   unique()

##### gather information into proper variables #####
sub_enhanced$VerbatimScientificName <- sub1$ScientificName
sub_enhanced$ScientificName <- sub_enhanced$valid_name
sub_enhanced$VernacularName <- sub_enhanced$vernaculars_list
sub_enhanced$TaxonRank <- sub_enhanced$rank
sub_enhanced$AphiaID <- sub_enhanced$valid_AphiaID
sub_enhanced$Phylum <- sub_enhanced$phylum
sub_enhanced$Class <- sub_enhanced$Class.y
sub_enhanced$Subclass <- sub_enhanced$Subclass.y
sub_enhanced$Order <- sub_enhanced$Order.y
sub_enhanced$Suborder <- sub_enhanced$Suborder.y
sub_enhanced$Family <- sub_enhanced$Family.y
sub_enhanced$Subfamily <- sub_enhanced$Subfamily.y
sub_enhanced$Genus <- sub_enhanced$Genus.y
sub_enhanced$Subgenus <- sub_enhanced$Subgenus.y
sub_enhanced$Species <- word(sub_enhanced$Species.y, -1)
sub_enhanced$Subspecies <- sub_enhanced$Subspecies.y
sub_enhanced$ScientificNameAuthorship <- sub_enhanced$authority
sub_enhanced$Synonyms <- sub_enhanced$synonyms_list

##### get rid of any flagged taxa #####
sub_enhanced <- sub_enhanced %>% filter(!(ScientificName %in% intersect(sub_enhanced2$ScientificName, taxfl$ScientificName)))

##### apply taxonomic filter #####
sub_enhanced_filter <- sub_enhanced %>%
  filter(Subphylum == 'Vertebrata' |
           Phylum == 'Cnidaria' |
           Phylum == 'Porifera')

`%notin%` <- Negate(`%in%`)
sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Class %notin% c('Scyphozoa', 'Thalicacea', 'Ascidiacea'))

sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Order == 'Scleractinia' |
           Order == 'Antipatharia' |
           Genus == 'Savalia' |
           Genus == 'Kulamanamana' |
           Genus == 'Gerardia' |
           Family == 'Stylasteridae' |
           Order  == 'Alcyonacea' |
           Order ==  'Gorgonacea' |
           Order ==  'Helioporacea' |
           Order == 'Pennatulacea' |
           Order == 'Scleralcyonacea' |
           Family == 'Stylasteridae' |
           Genus == 'Solanderia' |
           Genus == 'Janaria' |
           Genus == 'Hydrocorella' |
           Genus == 'Hydrodendron' |
           Phylum == 'Chordata' |
           Phylum == 'Porifera' |
           Order == 'Malacalcyonacea'
  )


##### check #####
# sub_enhanced %>% filter(ScientificName == "Anthozoa") %>% pull(ScientificName)
# sub_enhanced_filter %>% filter(ScientificName == "Anthozoa") %>% pull(ScientificName)
# filt %>% filter(ScientificName == "Anthozoa") %>% pull(ScientificName)
#
# table(sub_enhanced_filter$Phylum, useNA = 'always')
# table(sub_enhanced_filter$Subphylum, useNA = 'always')
#
# sub_enhanced_filter %>%
#   group_by(AphiaID, Phylum, Subphylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()

##### assign VernacularNameCategory #####
## define not in
## define not in
`%notin%` <- Negate(`%in%`)

gorgfamilies <- c("Paramuriceidae","Chrysogorgiidae","Dendrobrachiidae",
                  "Ellisellidae", "Isididae",
                  "Pleurogorgiidae", "Primnoidae",
                  "Acanthogorgiidae", "Gorgoniidae","Keroeididae",
                  "Plexauridae", "Anthothelidae",
                  "Coralliidae", "Melithaeidae",
                  "Paragorgiidae", "Parisididae","Spongiodermidae", "Subergorgiidae",
                  "Victorgorgiidae", "Keratoisididae", "Malacalcyonacea incertae sedis")

softfamilies <- c("Alcyoniidae","Aquaumbridae", "Ifalukellidae",
                  "Nephtheidae","Nidaliidae", "Paralcyoniidae",
                  "Xeniidae", "Taiaroidae")

othercorallikehydrozoanfamilies <- c("Solanderiidae", "Haleciidae")

stonycoralbranching <- tax %>%
  filter(VernacularNameCategory == 'stony coral (branching)') %>%
  pull(ScientificName)

stonycoralcupcoral <- tax %>%
  filter(VernacularNameCategory == 'stony coral (cup coral)') %>%
  pull(ScientificName)

sub_enhanced2 <- sub_enhanced_filter %>%
  mutate(VernacularNameCategory = case_when(
    Phylum %in% c('Chordata') ~ 'fish',
    TaxonRank %in% c('Order') &
      Order %in% c('Alcyonacea') ~ 'alcyonacean (unspecified)',
    Order %in% c('Antipatharia') ~ 'black coral',
    Class %in% c('Calcarea')~ 'calcareous sponge',
    Class %in% c('Demospongiae') ~ 'demosponge',
    Class %in% c('Hexactinellida') ~ 'glass sponge',
    Class %in% c('Homoscleromorpha') ~ 'homoscleromorph sponge',
    Family %in% c('Parazoanthidae') ~ 'gold coral',
    Family %in% gorgfamilies ~ 'gorgonian coral',
    Family %in% softfamilies ~ 'soft coral',
    Order %in% c('Malacalcyonacea') ~ 'soft coral',
    Order %in% c('Anthoathecata') &
      Family %notin%  c('Solanderiidae') ~ 'lace coral',
    Family %in% c('Lithotelestidae') ~ 'lithotelestid coral',
    Family %in% othercorallikehydrozoanfamilies ~ 'other coral-like hydrozoan',
    Superfamily %in% c('Pennatuloidea') ~ 'sea pen',
    ScientificName %in% c('Porifera') ~ 'sponge',
    Suborder %in% c('Stolonifera') ~ 'stoloniferan coral',
    Family %in% c('Clavulariidae') ~ 'stoloniferan coral',
    Genus %in% c('Clavularia') ~ 'stoloniferan coral',
    Order %in% c('Scleractinia') ~ 'stony coral (unspecified)',
    Order %in% c('Scleralcyonacea') ~  'scleralcyonacea (unspecified)',
    ScientificName %in% stonycoralbranching ~ 'stony coral (branching)',
    ScientificName %in% stonycoralcupcoral ~ 'stony coral (cup coral)',
    Genus %in% c('Acanthogorgia') ~ 'gorgonian coral',
    Genus %in% c('Hydrodendron') ~ 'other coral-like hydrozoan',
    Genus %in% c('Caryophyllia') ~ 'stony coral (cup coral)',
    Genus %in% c('Cladocora') ~ 'stony coral (cup coral)',
    TRUE ~ ''))

##### check #####
# table(sub_enhanced2$VernacularNameCategory, useNA = 'always')
# sub_enhanced2 %>% filter(VernacularNameCategory == '') %>% pull(ScientificName) %>% unique()
# intersect(sub_enhanced2$ScientificName, taxfl$ScientificName)
# sub_enhanced2 %>% filter(ScientificName == 'Vacatina') %>% pull(TaxonRank)
# filt %>% filter(ScientificName == 'Scleralcyonacea') %>% pull(Order) %>% table()
# filt %>% filter(ScientificName == 'Scleractinia') %>% pull(VernacularNameCategory) %>% table()
# filt %>% filter(ScientificName == 'Vacatina') %>% pull(Order) %>% table()
# filt %>% filter(ScientificName == 'Cladocora') %>% pull(VernacularNameCategory) %>% table()


##### get rid of unneeded column names #####
names_list <- names(sub)
sub_enhanced2 <- sub_enhanced2 %>%
  dplyr::select(all_of(names_list))

##### select just the taxonomic variables + CatalogNumber to create patch #####
sub_enhanced3<- sub_enhanced2 %>%
  select(CatalogNumber,
         VerbatimScientificName,
         ScientificName,
         VernacularName,
         VernacularNameCategory,
         TaxonRank,
         AphiaID,
         Phylum,
         Class,
         Subclass,
         Order,
         Suborder,
         Family,
         Subfamily,
         Genus,
         Subgenus,
         Species,
         Subspecies,
         ScientificNameAuthorship,
         Synonyms)

##### check #####
# View(sub_enhanced3)
# sub_enhanced3 %>% pull(VerbatimScientificName) %>% table(useNA = 'always')
# sub_enhanced3 %>% filter(VerbatimScientificName == 'Agelas cf. flabelliformis') %>%
#   pull(ScientificName) %>% table(useNA = 'always')
cats <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
length(cats)
dim(sub)

sub %>%
  filter(CatalogNumber %in% cats) %>%
  pull(ScientificName) %>% unique()

sub_enhanced3 %>%
  filter(VerbatimScientificName == 'Eel') %>%
  pull(ScientificName)

sub_enhanced3 %>%
  filter(VerbatimScientificName == 'Eel') %>%
  pull(Class) %>% table()

##### export result to csv (export to CSV) #####
filename <- "20250623-0_NOAA_PC2202L1_MDBC_143699_taxonomy_patch.csv"
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)






##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))



##### ***** CTD PATCH Creation 2025xxxx-x (CREATING CTD patch) ***** #####
##### load dataset from CSV #####
setwd('c:/rworking/deepseatools/indata/')
filename <- '20250618-2_NOAA_PC2202L1_MDBC_143699'
sub <- read.csv(paste(filename, '.csv', sep=''))
# View(sub)

##### check #####
## true if all values are unique, false if not.
# length(unique(sub$SampleID)) == length(sub$SampleID)E
##### load exports from tator #####
tatorexport <- 'PC2202L1_Forward Videos_2025-06-22_report_KZOWtUnqfT.xlsx'

corals_test <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                    sheet = 'corals_inverts')

fish <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                  sheet = 'fish')

events <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                    sheet = 'events')

geology <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                     sheet = 'geology')

##### check #####
names(corals)

intersect(corals$TatorId, corals_test$TatorId)
##### load data from SQL files #####
## version 20250516

filename <- 'SAMPLEMETADATA.xlsx'
samplemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                            sheet = 'BiologicalSamples')

filename <- 'DIVEVEHICLEMETADATA.xlsx'
divevehiclemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                                 sheet = 'DiveMetadata')

##### check #####
# names(events)
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
    SampleID = TatorElementalId,
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
# dscrtp_export$SampleID <- paste(dscrtp_export$ObservationDate, dscrtp_export$TatorId, sep = '_') #pre-transformed, see below for transformation.

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
dscrtp_export %>% pull(IdentifiedBy) %>% table()
dscrtp_export %>% pull(OccurrenceComments) %>% table()

dscrtp_export %>% pull(Habitat) %>% table(useNA = 'always')  %>% sort()
dim(dscrtp_export)

dscrtp_export %>% select(Habitat, CMECSGeoForm) %>% distinct() %>%  View()

# x <- s %>% filter(PointNew == "R") %>% pull(FieldName)
# setdiff(names(dscrtp_export), x)
# setdiff(x,names(dscrtp_export))

# dscrtp_export %>% pull(CategoricalAbundance) %>% table() %>% sort()
# dscrtp_export %>% pull(IndividualCount) %>% table() %>% sort()

##### create the Temperature and Salinity patch #####
yo <- dscrtp_export %>% dplyr::select(SampleID, Temperature, Salinity)

##### ***** #####
##### write the file to disk #####
## get rid of .xlsx at the end of the input file name
x <- str_remove(tatorexport, "\\.xlsx$")

## write
write.csv(yo,
          paste('c:/rworking/deepseatools/indata/',
                '20250623-0_',
                'NOAA_PC2202L1_MDBC_143699_CTD_patch',
                '.csv',
                sep = ''),
          row.names = F)



##### check #####

## load patch
filename <- "20250623-0_NOAA_PC2202L1_MDBC_143699_CTD_patch.csv"
patch <- read.csv(paste("c:/rworking/deepseatools/indata/",
                        filename, sep=''),)
# load data
setwd('c:/rworking/deepseatools/indata/')
filename <- '20250618-2_NOAA_PC2202L1_MDBC_143699'
sub <- read.csv(paste(filename, '.csv', sep=''))

## build character vectors
patchSampleIDs <-  sub(".*_(\\d+)$", "\\1", patch$SampleID)
subSampleIDs <- sub(".*_(\\d+)$", "\\1", sub$SampleID)

## get length of intersect the strings
length(intersect(patchSampleIDs, subSampleIDs))
length(intersect(sub$SampleID, patch$SampleID))

## check specific values

grep("256665150", subSampleIDs, value = TRUE)
grep("256665150", patchSampleIDs, value = TRUE)



























##### ***** #####


##### ***** NEW ORIGINAL from Tator (2025-06-22) ***** #####
## Tator report: 'PC2202L1_Forward Videos_2025-06-22_report_KZOWtUnqfT.xlsx'

##### load exports from tator #####
tatorexport <- 'PC2202L1_Forward Videos_2025-06-22_report_KZOWtUnqfT.xlsx'

corals <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                         sheet = 'corals_inverts')

fish <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                  sheet = 'fish')

events <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                    sheet = 'events')

geology <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                     sheet = 'geology')

##### check #####
# names(corals)
#
# intersect(corals$TatorId, corals_test$TatorId)
##### load data from SQL files #####
## version 20250516

filename <- 'SAMPLEMETADATA.xlsx'
samplemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                            sheet = 'BiologicalSamples')

filename <- 'DIVEVEHICLEMETADATA.xlsx'
divevehiclemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                                 sheet = 'DiveMetadata')

##### check #####
# names(events)
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
locality_table <- divevehiclemetadata %>%
  filter(CruiseID == unique(corals$CruiseId)) %>%
  select(DiveID, GeneralLocation)
result <- merge(result, locality_table, by.x= 'DiveId', by.y = 'DiveID')

##### check #####
# dim(result)
# length(unique(result$TatorElementalId))
# result %>% select(TatorElementalId, MediaId)


##### ***** #####
##### main crosswalk and transformations to create export #####
dscrtp_export <- result %>%
  mutate(
    SampleID = TatorElementalId,
    ImageFilePath = paste(MediaId, Frame, sep = '_'), ## this will need to be adapted when we get the media in house
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


## transforming date and time
split_position = 10
date <- substr(dscrtp_export$ObservationDate, 1, split_position)
time <- substr(dscrtp_export$ObservationDate, split_position + 2, nchar(dscrtp_export$ObservationDate))
dscrtp_export$ObservationDate <- date
dscrtp_export$ObservationTime <- time

## get rid of everything after the '+' in the ObservationTime
dscrtp_export$ObservationTime <- sub("\\+.*", "", dscrtp_export$ObservationTime)

## get just the DSCRTP fields out that we need
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
dscrtp_export$RecordType <- 'video observation'
dscrtp_export$Modified <- '2025-06-25'
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
                                'Database version: ', unique(filt$DatabaseVersion), '. ')

##### check #####
# dscrtp_export %>% pull(IdentifiedBy) %>% table()
# dscrtp_export %>% pull(OccurrenceComments) %>% table()
# dscrtp_export %>% pull(Habitat) %>% table(useNA = 'always')  %>% sort()
# dscrtp_export %>% pull(Locality) %>% table(useNA = 'always')  %>% sort()

# dim(dscrtp_export)
# dscrtp_export %>% select(Habitat, CMECSGeoForm) %>% distinct() %>%  View()
#
# x <- s %>% filter(PointNew == "R") %>% pull(FieldName)
# setdiff(names(dscrtp_export), x)
# setdiff(x,names(dscrtp_export))
#
# dscrtp_export %>% pull(CategoricalAbundance) %>% table() %>% sort()
# dscrtp_export %>% pull(IndividualCount) %>% table() %>% sort()
# dscrtp_export %>% pull(SampleID) %>% head()

##### write the file to disk #####
## write
write.csv(dscrtp_export,
          paste('c:/rworking/deepseatools/indata/',
                '20250627-0_',
                'NOAA_PC2202L1_MDBC_143699',
                '.csv',
                sep = ''),
          row.names = F)

##### create a patch to bring forward #####
Locality_patch <- dscrtp_export %>% select(Locality, SampleID) %>%
  write.csv('c:/rworking/deepseatools/indata/locality_patch.csv')



##### ***** NEW Version 20250627-3 Taxonomic Patch ****** #####
##### load dataset from CSV #####
setwd('c:/rworking/deepseatools/indata/')
filename <- '20250627-3_NOAA_PC2202L1_MDBC_143699'
sub <- read.csv(paste(filename, '.csv', sep=''))

##### load the most current taxonomy from Google Sheets #####
# https://drive.google.com/open?id=0B9c2c_XdhpFBT29NQmxIeUQ4Tlk
## manual: make sure the IDs below are pointing at the correct sheets
tax <- read_sheet('1v3yZO7ATMtV-wp9lePl2pV9-ycxFo3VGVrR_SIunbdQ')
taxfl <- read_sheet('1ZfR4wiBQbDsFGpYXXDjHrsF1QJyoCMqfocmxbpBPo9M')
taxch <- read_sheet('11FgDuNmIZRSf2W4MeFqn2h8pOekvQEP2nG4vcy46pY8')

##### make taxonomic changes to incoming (manual: specific to each new dataset) #####
## filter out these names
sub1 <- sub %>% filter(ScientificName != 'Not Set')

## change these taxa
sub2 <- sub1  %>%
  mutate(ScientificName = str_replace(ScientificName, "Stichopathes sp", "Stichopathes")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Nicella Sp.", "Nicella")) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Agelas cf. flabelliformis', 'Agelas')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Davidastar discoideus', 'Davidaster discoideus')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Ircinidae', 'Irciniidae')) %>%
  mutate(ScientificName = str_replace(ScientificName, '\\bOctocoral\\b', 'Octocorallia')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Crustose Coralline Algae', 'Rhodophyta')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Cladocora caespitosa', 'Scleractinia')) %>%
  mutate(ScientificName = str_replace(ScientificName, 'Eel', 'Anguilliformes'))

##### check #####
# sub %>% filter(ScientificName == 'Octocoral') %>% dim()
# filt %>% filter(ScientificName == 'Scleralcyonacea') %>% pull(VernacularNameCategory)

##### create vector of names #####
my_vector <- unique(sub2$ScientificName)
my_vector <- my_vector[complete.cases(my_vector)]

##### parse the list using taxize function 'gbif_parse' #####
parsed_list <- gbif_parse(my_vector)

## get only unique parsed names
parsed_list <- distinct(parsed_list)

## View(parsed_list)
my_vector_parsed <- parsed_list$canonicalname

## get only unique names
my_vector_parsed <- unique(my_vector_parsed)

##### check #####
# my_vector_parsed
# sort(my_vector_parsed)
# sort(my_vector)

##### make groups of 50 (because the API limit is 50) #####
my_groups <- split(my_vector_parsed, ceiling(seq_along(my_vector)/50))

##### loop to get records by names list #####
## run this just once to get the proper data structure for an empty dataframe
species_list <- wm_records_name("Octocorallia", fuzzy = F)

## initiate the empty data frame
df <- species_list[0,]

## loop to get WoRMS records from names (b)
for (i in seq_along(my_groups)){
  species_list <- wm_records_names(name = my_groups[[i]],
                                   fuzzy = F,
                                   marine_only = T,

  )
  species_list <- do.call("rbind", species_list)
  df <- rbind(df, species_list)
}
species_list <- df

## get rid of any extinct matches
species_list <- species_list %>%
  filter(isExtinct == 0 |
           is.na(isExtinct) == T)

## get just the data that are distinct
species_list <- distinct(species_list)

##### check #####
# dim(species_list)
# View(species_list)

##### left join the parsed list #####
by <- join_by(canonicalname == scientificname)
joined <- left_join(parsed_list, species_list, by)

##### create a summary joined file #####
summary <- joined %>%
  group_by(status,
           phylum,
           scientificname,
           canonicalname,
           valid_name,
           valid_AphiaID,
           rank,
           authority) %>%
  summarize(n=n())

##### check: test for difficult taxa #####
summary$sametest <- ifelse(summary$canonicalname == summary$valid_name,"Yes","No")
changes <- summary %>% filter(sametest == "No") %>% pull(scientificname)
nomatch <- summary %>% filter(is.na(sametest) == T) %>% pull(scientificname)

changes
nomatch

##### check: test for duplicate AphiaID #####
## Find duplicate values
duplicates <- summary$valid_AphiaID[duplicated(summary$valid_AphiaID)]
unique_duplicates <- unique(duplicates)
print(unique_duplicates)

summary %>% filter(valid_AphiaID == '731943') %>% View()
table(summary$status)

##### get rid of NA from valid_AphiaID #####
summary <- summary %>% filter(is.na(valid_AphiaID) == F)

##### get rid of duplicate values #####
summary <- summary %>% filter(!(valid_AphiaID == '731943' & status == 'unaccepted'))

##### create vector from valid AphiaIDs #####
my_vector <- unique(summary$valid_AphiaID)

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list <- df

##### loop to get classification #####
df <- data.frame(
  Domain = character(),
  Kingdom = character(),
  Subkingdom = character(),
  Phylum = character(),
  Subphylum = character(),
  Superclass = character(),
  Class = character(),
  Subclass = character(),
  Infraclass = character(),
  Superorder = character(),
  Order = character(),
  Suborder = character(),
  Infraorder = character(),
  Superfamily = character(),
  Family = character(),
  Subfamily = character(),
  Tribe = character(),
  Subtribe = character(),
  Genus = character(),
  Subgenus = character(),
  Species = character(),
  Subspecies = character(),
  Variety = character(),
  stringsAsFactors=FALSE)

## loop to get full classification
for (i in my_vector){
  try(classification <- wm_classification(i))
  classification_wide <- classification %>%
    select(rank,scientificname) %>%
    pivot_wider(
      names_from = rank,
      values_from = scientificname
    )
  classification_wide$AphiaID <- i
  df <- bind_rows(df, classification_wide)
}

classification <- df

##### loop to get vernacular name #####
## initialize an empty list to store successful results
result_list <- list()

## loop through my_vector
for (i in my_vector) {
  ## use try() to handle errors
  vernaculars <- try({
    wm_result <- wm_common_id(i)
    wm_result %>% filter(language == 'English')
  }, silent = TRUE) ## Use silent = TRUE to suppress error messages

  ## check if there was an error in wm_common_id()
  if (!inherits(vernaculars, "try-error")) {
    ## If no error, proceed to append the data to the result_list
    vernaculars_list <- paste(vernaculars$vernacular, collapse = " | ")
    AphiaID <- i
    vernaculars_wide <- data.frame(AphiaID, vernaculars_list)
    result_list[[length(result_list) + 1]] <- vernaculars_wide
  }
}

## Combine the successful results into the final data frame df
df <- do.call(rbind, result_list)
vernaculars <- df

##### check #####
# wm_common_id(1567760)

##### loop to get synonyms #####
## initialize a results list
result_list <- list()

## loop through my_vector
for (i in my_vector) {
  ## use try() to handle errors
  synonyms <- try({
    wm_result <- wm_synonyms(i) # i = 423632
  }, silent = TRUE) # Use silent = TRUE to suppress error messages

  ## check if there was an error in wm_synonyms()
  if (!inherits(synonyms, "try-error")) {
    ## if no error, proceed to append the data to the result_list
    synonyms_list <- paste(synonyms$scientificname, collapse = " | ")
    AphiaID <- i
    synonyms_wide <- data.frame(AphiaID, synonyms_list)
    result_list[[length(result_list) + 1]] <- synonyms_wide
  }
}

## Combine the successful results into the final data frame df
df <- do.call(rbind, result_list)
synonyms <- df

##### check #####
# View(classification)
# View(vernaculars)
# View(classification)
# View(synonyms)
#
# dim(species_list)
# dim(classification)
# dim(vernaculars)
# dim(synonyms)
#
# names(species_list)
# names(classification)
# names(vernaculars)
# names(synonyms)
#
# head(species_list$AphiaID)
# head(classification$AphiaID)
# head(vernaculars$AphiaID)
# head(synonyms$AphiaID)
#
# tail(tax_tom_enhanced$AphiaID)
# tail(classification$AphiaID)
# tail(vernaculars$AphiaID)
# tail(synonyms$AphiaID)
#
# class(tax_tom_enhanced$AphiaID)
# class(classification$AphiaID)
# class(vernaculars$AphiaID)
# class(synonyms$AphiaID)


##### get rid of duplicates in each table #####
classification <- classification %>%
  mutate(across(everything(), as.character)) %>%
  distinct()
vernaculars <- vernaculars %>%
  mutate(across(everything(), as.character)) %>%
  distinct()
synonyms <- synonyms %>%
  mutate(across(everything(), as.character)) %>%
  distinct()
summary <- summary %>%
  mutate(across(everything(), as.character)) %>%
  distinct()

##### check #####
duplicates <- classification_clean %>%
  group_by(AphiaID) %>%
  filter(n() > 1) %>%
  ungroup()
View(duplicates)

duplicates <- summary %>%
  group_by(valid_AphiaID) %>%
  filter(n() > 1) %>%
  ungroup()

View(duplicates)

##### left join the summary from above with all of the other API tables #####
summary$valid_AphiaID <- as.integer(summary$valid_AphiaID)
classification$AphiaID <- as.integer(classification$AphiaID)

by <- join_by(valid_AphiaID == AphiaID)
joined2 <- left_join(summary, classification, by)
joined2 <- distinct(joined2) # get distinct records

vernaculars$AphiaID <- as.integer(vernaculars$AphiaID)
by <- join_by(valid_AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

synonyms$AphiaID <- as.integer(synonyms$AphiaID)
by <- join_by(valid_AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

##### clean up joined4 #####
joined4 <- joined4 %>%
  mutate(across(everything(), as.character)) %>%
  distinct()

joined4 <- joined4 %>% filter(phylum %in% c('Porifera', 'Cnidaria', 'Chordata'))
joined4 <- joined4 %>% filter(!(scientificname == 'Zoantharia' & valid_AphiaID == 1340))
joined4 <- joined4 %>% filter(!(scientificname == 'Chromis' & valid_AphiaID == 271096))
joined4 <- joined4 %>% filter(!(scientificname == 'Aplysina cauliformis' & valid_AphiaID == 166230))
joined4 <- joined4 %>% filter(!(scientificname == 'Seriola' & valid_AphiaID == 131995))

##### check #####
# sub2[11388,c('ScientificName')]
# sort(joined4$scientificname)
# joined4 %>% filter(scientificname == 'Seriola') %>% View()

##### add taxonomy to sub #####
by <- join_by(ScientificName == scientificname)
sub_enhanced <- left_join(sub2, joined4, by)

##### check #####
# sub_enhanced %>% filter(is.na(phylum) == T) %>%
#   pull(ScientificName) %>%
#   unique()

##### gather information into proper variables #####
sub_enhanced$VerbatimScientificName <- sub1$ScientificName
sub_enhanced$ScientificName <- sub_enhanced$valid_name
sub_enhanced$VernacularName <- sub_enhanced$vernaculars_list
sub_enhanced$TaxonRank <- sub_enhanced$rank
sub_enhanced$AphiaID <- sub_enhanced$valid_AphiaID
sub_enhanced$Phylum <- sub_enhanced$phylum
sub_enhanced$Class <- sub_enhanced$Class.y
sub_enhanced$Subclass <- sub_enhanced$Subclass.y
sub_enhanced$Order <- sub_enhanced$Order.y
sub_enhanced$Suborder <- sub_enhanced$Suborder.y
sub_enhanced$Family <- sub_enhanced$Family.y
sub_enhanced$Subfamily <- sub_enhanced$Subfamily.y
sub_enhanced$Genus <- sub_enhanced$Genus.y
sub_enhanced$Subgenus <- sub_enhanced$Subgenus.y
sub_enhanced$Species <- word(sub_enhanced$Species.y, -1)
sub_enhanced$Subspecies <- sub_enhanced$Subspecies.y
sub_enhanced$ScientificNameAuthorship <- sub_enhanced$authority
sub_enhanced$Synonyms <- sub_enhanced$synonyms_list

##### get rid of any flagged taxa #####
sub_enhanced <- sub_enhanced %>% filter(!(ScientificName %in% intersect(sub_enhanced2$ScientificName, taxfl$ScientificName)))

##### apply taxonomic filter #####
sub_enhanced_filter <- sub_enhanced %>%
  filter(Subphylum == 'Vertebrata' |
           Phylum == 'Cnidaria' |
           Phylum == 'Porifera')

`%notin%` <- Negate(`%in%`)
sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Class %notin% c('Scyphozoa', 'Thalicacea', 'Ascidiacea'))

sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Order == 'Scleractinia' |
           Order == 'Antipatharia' |
           Genus == 'Savalia' |
           Genus == 'Kulamanamana' |
           Genus == 'Gerardia' |
           Family == 'Stylasteridae' |
           Order  == 'Alcyonacea' |
           Order ==  'Gorgonacea' |
           Order ==  'Helioporacea' |
           Order == 'Pennatulacea' |
           Order == 'Scleralcyonacea' |
           Family == 'Stylasteridae' |
           Genus == 'Solanderia' |
           Genus == 'Janaria' |
           Genus == 'Hydrocorella' |
           Genus == 'Hydrodendron' |
           Phylum == 'Chordata' |
           Phylum == 'Porifera' |
           Order == 'Malacalcyonacea'
  )


##### check #####
# sub_enhanced %>% filter(ScientificName == "Anthozoa") %>% pull(ScientificName)
# sub_enhanced_filter %>% filter(ScientificName == "Anthozoa") %>% pull(ScientificName)
# filt %>% filter(ScientificName == "Anthozoa") %>% pull(ScientificName)
#
# table(sub_enhanced_filter$Phylum, useNA = 'always')
# table(sub_enhanced_filter$Subphylum, useNA = 'always')
#
# sub_enhanced_filter %>%
#   group_by(AphiaID, Phylum, Subphylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()

##### assign VernacularNameCategory #####
## define not in
## define not in
`%notin%` <- Negate(`%in%`)

gorgfamilies <- c("Paramuriceidae","Chrysogorgiidae","Dendrobrachiidae",
                  "Ellisellidae", "Isididae",
                  "Pleurogorgiidae", "Primnoidae",
                  "Acanthogorgiidae", "Gorgoniidae","Keroeididae",
                  "Plexauridae", "Anthothelidae",
                  "Coralliidae", "Melithaeidae",
                  "Paragorgiidae", "Parisididae","Spongiodermidae", "Subergorgiidae",
                  "Victorgorgiidae", "Keratoisididae", "Malacalcyonacea incertae sedis")

softfamilies <- c("Alcyoniidae","Aquaumbridae", "Ifalukellidae",
                  "Nephtheidae","Nidaliidae", "Paralcyoniidae",
                  "Xeniidae", "Taiaroidae")

othercorallikehydrozoanfamilies <- c("Solanderiidae", "Haleciidae")

stonycoralbranching <- tax %>%
  filter(VernacularNameCategory == 'stony coral (branching)') %>%
  pull(ScientificName)

stonycoralcupcoral <- tax %>%
  filter(VernacularNameCategory == 'stony coral (cup coral)') %>%
  pull(ScientificName)

sub_enhanced2 <- sub_enhanced_filter %>%
  mutate(VernacularNameCategory = case_when(
    Phylum %in% c('Chordata') ~ 'fish',
    TaxonRank %in% c('Order') &
      Order %in% c('Alcyonacea') ~ 'alcyonacean (unspecified)',
    Order %in% c('Antipatharia') ~ 'black coral',
    Class %in% c('Calcarea')~ 'calcareous sponge',
    Class %in% c('Demospongiae') ~ 'demosponge',
    Class %in% c('Hexactinellida') ~ 'glass sponge',
    Class %in% c('Homoscleromorpha') ~ 'homoscleromorph sponge',
    Family %in% c('Parazoanthidae') ~ 'gold coral',
    Family %in% gorgfamilies ~ 'gorgonian coral',
    Family %in% softfamilies ~ 'soft coral',
    Order %in% c('Malacalcyonacea') ~ 'soft coral',
    Order %in% c('Anthoathecata') &
      Family %notin%  c('Solanderiidae') ~ 'lace coral',
    Family %in% c('Lithotelestidae') ~ 'lithotelestid coral',
    Family %in% othercorallikehydrozoanfamilies ~ 'other coral-like hydrozoan',
    Superfamily %in% c('Pennatuloidea') ~ 'sea pen',
    ScientificName %in% c('Porifera') ~ 'sponge',
    Suborder %in% c('Stolonifera') ~ 'stoloniferan coral',
    Family %in% c('Clavulariidae') ~ 'stoloniferan coral',
    Genus %in% c('Clavularia') ~ 'stoloniferan coral',
    Order %in% c('Scleractinia') ~ 'stony coral (unspecified)',
    Order %in% c('Scleralcyonacea') ~  'scleralcyonacea (unspecified)',
    ScientificName %in% stonycoralbranching ~ 'stony coral (branching)',
    ScientificName %in% stonycoralcupcoral ~ 'stony coral (cup coral)',
    Genus %in% c('Acanthogorgia') ~ 'gorgonian coral',
    Genus %in% c('Hydrodendron') ~ 'other coral-like hydrozoan',
    Genus %in% c('Caryophyllia') ~ 'stony coral (cup coral)',
    Genus %in% c('Cladocora') ~ 'stony coral (cup coral)',
    TRUE ~ ''))

##### check #####
# table(sub_enhanced2$VernacularNameCategory, useNA = 'always')
# sub_enhanced2 %>% filter(VernacularNameCategory == '') %>% pull(ScientificName) %>% unique()
# intersect(sub_enhanced2$ScientificName, taxfl$ScientificName)
# sub_enhanced2 %>% filter(ScientificName == 'Vacatina') %>% pull(TaxonRank)
# filt %>% filter(ScientificName == 'Scleralcyonacea') %>% pull(Order) %>% table()
# filt %>% filter(ScientificName == 'Scleractinia') %>% pull(VernacularNameCategory) %>% table()
# filt %>% filter(ScientificName == 'Vacatina') %>% pull(Order) %>% table()
# sub_enhanced2 %>% filter(ScientificName == 'Vacatina') %>% pull(Order) %>% table()
# filt %>% filter(ScientificName == 'Cladocora') %>% pull(VernacularNameCategory) %>% table()

##### get rid of unneeded column names #####
names_list <- names(sub)
sub_enhanced2 <- sub_enhanced2 %>%
  dplyr::select(all_of(names_list))

##### select just the taxonomic variables + CatalogNumber to create patch #####
sub_enhanced3<- sub_enhanced2 %>%
  select(CatalogNumber,
         VerbatimScientificName,
         ScientificName,
         VernacularName,
         VernacularNameCategory,
         TaxonRank,
         AphiaID,
         Phylum,
         Class,
         Subclass,
         Order,
         Suborder,
         Family,
         Subfamily,
         Genus,
         Subgenus,
         Species,
         Subspecies,
         ScientificNameAuthorship,
         Synonyms)

##### check #####
# View(sub_enhanced3)
# sub_enhanced3 %>% pull(VerbatimScientificName) %>% table(useNA = 'always')
# sub_enhanced3 %>% filter(VerbatimScientificName == 'Agelas cf. flabelliformis') %>%
#   pull(ScientificName) %>% table(useNA = 'always')
cats <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
length(cats)
dim(sub)- dim(sub_enhanced3)

sub %>%
  filter(CatalogNumber %in% cats) %>%
  pull(ScientificName) %>% unique()

sub_enhanced3 %>%
  filter(VerbatimScientificName == 'Eel') %>%
  pull(ScientificName)

sub_enhanced3 %>%
  filter(VerbatimScientificName == 'Eel') %>%
  pull(Class) %>% table()

##### export result to csv (export to CSV) #####
filename <- "20250703-0_NOAA_PC2202L1_MDBC_143699_taxonomy_patch.csv"
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)


##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))






##### ***** NEW Version: 20250703-0: QA report ****** #####
##### load data #####
setwd('c:/rworking/deepseatools/indata')
filename <- '20250703-0_NOAA_PC2202L1_MDBC_143699'
sub <- read.csv(paste(filename, '.csv', sep = ''))

##### check #####
# table(sub$Flag)
# sub %>% filter(Flag == 1) %>% group_by(VerbatimLatitude) %>%
#   summarize(n=n()) %>% View()
# table(sub$IndividualCount, useNA = 'always')
# filt %>% filter(grepl('Eiwa', VehicleName)) %>% pull(VehicleName) %>% table()
# sub %>% filter(grepl('Eiwa', VehicleName)) %>% pull(VehicleName) %>% table()
# filt %>% filter(grepl('NOAA_SH-22-09', DatasetID)) %>% pull(VehicleName) %>% table()
# filt %>% filter(grepl('SH', SurveyID)) %>% pull(SurveyID) %>% table()

##### make some change so that the QA report runs (OPTIONAL)
sub$Citation <- 'place holder'

##### run QA report #####
## manual change version of dashboard version number is required
rmarkdown::render("C:/rworking/deepseatools/code/20250401-0_rmd_accession_qa_dashboard.Rmd",
                  output_file =  paste(filename,".docx", sep=''),
                  output_dir = 'C:/rworking/deepseatools/reports')

## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1nsHRBtj1UUBZtticYJEjx5CBst6x8IDb"
setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".docx", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".docx", sep=''),
             overwrite = T)

##### check #####
filt %>% filter(DatasetID == 'NOAA_RL-19-05') %>%
  pull(LocationAccuracy) %>% table()

dim(sub)

unique(sub$VehicleName)
sub %>% pull(Phylum) %>% table()
sub %>% filter(Flag == 1) %>% pull(Longitude) %>% table()




