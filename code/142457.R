##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20250313
## purpose:ingest for '142457'

##### linkage #####
filename <- '142457' ## manual: for this code file name, match to redmine
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
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)
library(robis)
library(openxlsx)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### check #####
# filt %>% filter(grepl('EX', DatasetID)) %>% pull(DatasetID) %>% unique()

##### ***** ORIGINAL ***** #####
##### load dataset from excel #####
filename <- '20250124_Graiff_CBNMS_ROV2014.xlsx'
sub <- read.xlsx(file.path('c:/rworking/deepseatools/indata', filename),
                 sheet = 'observations',
                 startRow = 14)

meta <- read.xlsx(file.path('c:/rworking/deepseatools/indata', filename),
                 sheet = 'metadata',
                 startRow = 1)

##### rename columns #####
# sub <- sub %>%
#   rename(
#     'VerbatimSize' = 'Size',
#   )

##### check #####
# names(sub)
# setdiff(names(sub),names(filt))
# size_columns <- grep("Size", names(filt), value = TRUE)

table(sub$DataProvider)
table(sub$DataContact)
table(sub$Citation)
table(sub$Repository)
table(sub$Modified)
table(sub$Reporter)
table(sub$ReporterComments)
table(sub$SurveyID)
table(sub$Vessel)
table(sub$VehicleName)
table(sub$PI)
table(sub$PIAffiliation)
table(sub$SamplingEquipment)
table(sub$DepthMethod)
table(sub$NavType)
table(sub$LocationAccuracy)
table(sub$Purpose)
table(sub$SurveyComments)
table(sub$RecordType)
table(sub$IdentifiedBy)
table(sub$IdentificationQualifier)
table(sub$IdentificationDate)
table(sub$IdentificationComments)

##### add metadata (NOTE: this mapping should be inspected and tested before using) #####
sub$DataProvider <- meta[1,4]
sub$DataContact <- meta[2,4]
sub$Citation <- meta[3,4]
sub$Repository <- meta[4,4]
sub$Modified <- meta[5,4]
sub$Reporter <- meta[6,4]
sub$ReporterComments <- meta[7,4]
sub$SurveyID <- meta[8,4]
sub$Vessel <- meta[9,4]
sub$VehicleName <- meta[10,4]
sub$PI <- meta[11,4]
sub$PIAffiliation <- meta[12,4]
sub$SamplingEquipment <- meta[13,4]
sub$DepthMethod <- meta[14,4]
sub$NavType <- meta[15,4]
sub$LocationAccuracy <- meta[16,4]
sub$Purpose <- meta[17,4]
sub$SurveyComments <- meta[18,4]
sub$RecordType <- meta[19,4]
sub$IdentifiedBy <- meta[20,4]
sub$IdentificationQualifier <- meta[21,4]
sub$IdentificationDate <- meta[22,4]
sub$IdentificationComments <- meta[23,4]

###### fix dates #####
sub$ObservationDate <- convertToDate(sub$ObservationDate)
sub$ObservationDate <- as.character(sub$ObservationDate)

sub$Modified <- convertToDate(sub$Modified)
sub$Modified <- as.character(sub$Modified)

##### explore #####
length(sub$SampleID)
length(unique(sub$SampleID))
dim(sub)
summary(sub)
names(sub)
table(sub$ImageFilePath, useNA = 'always')
table(sub$ObservationDate, useNA = 'always')
table(sub$Modified, useNA = 'always')
table(sub$DataProvider, useNA = 'always')
table(sub$SurveyID, useNA = 'always')
table(sub$Vessel, useNA = 'always')
table(sub$EventID, useNA = 'always')
table(sub$NavType, useNA = 'always')
table(sub$LocationAccuracy, useNA = 'always')
table(sub$EndLatitude, useNA = 'always')
table(sub$StartLatitude, useNA = 'always')
table(sub$EndLongitude, useNA = 'always')
table(sub$StartLongitude, useNA = 'always')
table(sub$Longitude, useNA = 'always')
table(sub$Latitude, useNA = 'always')
table(sub$Locality, useNA = 'always')
table(sub$DepthInMeters, useNA = 'always')
table(sub$MinimumDepthInMeters, useNA = 'always')
table(sub$MaximumDepthInMeters, useNA = 'always')
table(sub$DepthMethod, useNA = "always")
table(sub$ScientificName, useNA = "always")
table(sub$AphiaID, useNA = "always")


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

unique(grep("OET", filt$DatasetID, value = TRUE))
table(unique(sub$ObservationDate))

filt %>% filter(grepl("Fulmar", Vessel)) %>% select(DatasetID, Vessel, SurveyID) %>% distinct()
filt %>% filter(grepl("CBNMS", DatasetID)) %>% select(DatasetID, Vessel, SurveyID) %>% distinct()










##### write new version for pipeline #####
write.csv(sub, 'c:/rworking/deepseatools/indata/20250320-0_NOAA_AFSC_GOA_Coral_Survey_2022_142452.csv')

##### ***** NEW VERSION  *****  #####
##### load dataset from CSV #####
filename <- "20250321-3_NOAA_CBNMS_ROV_2014_142457"
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv(paste(filename, '.csv', sep = ''))

##### load the most current taxonomy from Google Sheets #####
## https://drive.google.com/open?id=0B9c2c_XdhpFBT29NQmxIeUQ4Tlk
## manual: make sure the IDs below are pointing at the correct sheets
# tax <- read_sheet('1v3yZO7ATMtV-wp9lePl2pV9-ycxFo3VGVrR_SIunbdQ')
# taxfl <- read_sheet('1ZfR4wiBQbDsFGpYXXDjHrsF1QJyoCMqfocmxbpBPo9M')
# taxch <- read_sheet('11FgDuNmIZRSf2W4MeFqn2h8pOekvQEP2nG4vcy46pY8')

##### check #####
# table(unique(sub$ScientificName), useNA = 'always')
# table(sub$AphiaID, useNA = 'always')
# length(unique(sub$ScientificName))
# length(sub$ScientificName)
# table(sub$Genus, useNA = 'always')

##### make taxonomic changes to incoming (manual: specific to each new dataset) #####
## filter taxa list (Optional)
sub1 <- sub

## change the following taxa for a better match
sub2 <- sub1  %>%
  mutate(ScientificName = trimws(ScientificName),  # Remove extra spaces
         ScientificName2 = ScientificName)


##### check #####
# sub2 %>% filter(ScientificName2 == "Polycapus") %>% pull(ScientificName2)

# table(sub2$ScientificName2, useNA = 'always')
#
# sub2 %>%
#   group_by(ScientificName, ScientificName2) %>%
#   summarize(n=n()) %>% View()


##### create vector of names #####
my_vector <- unique(sub2$ScientificName)

# remove any missing value.
my_vector <- my_vector[complete.cases(my_vector)]

##### make groups of 50 (because the API limit is 50) #####
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

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

## get just the data that are distinc
species_list <- distinct(species_list)

##### join sub2 with species list #####
by <- join_by(ScientificName2 == scientificname)
joined <- left_join(sub2, species_list, by)

##### check #####
# joined %>% group_by(Genus, VerbatimScientificName, ScientificName, ScientificName2, valid_name) %>%
#   summarize(n=n()) %>% View()

##### create a summary joined file #####
summary <- joined %>%
  group_by(status,
           phylum,
           ScientificName,
           ScientificName2,
           valid_name,
           valid_AphiaID,
           rank,
           authority) %>%
  summarize(n=n())

##### check: test for difficult taxa #####
summary$sametest <- ifelse(summary$ScientificName2 == summary$valid_name,"Yes","No")

changes <- summary %>% filter(sametest == "No") %>%
  group_by(ScientificName, ScientificName2, valid_name, sametest) %>%
  summarize(n=n())

nomatch <- summary %>%
  filter(is.na(sametest) == T) %>%
  group_by(ScientificName, ScientificName2, valid_name, sametest
  ) %>%
  summarize(n=n())

changes
nomatch

##### check #####
# summary %>% filter(ScientificName2 == "Polycapus")


##### strip out CatalogNumber and valid AphiaID #####
aphia<- joined %>% select(CatalogNumber, valid_AphiaID)

##### check #####
# names(aphia)

##### join proper aphia ID values back to original sub #####
sub <- left_join(sub, aphia)
sub$AphiaID <- sub$valid_AphiaID
sub <- sub %>% select(-all_of(c("valid_AphiaID")))

##### create vector of valid aphiaID  #####
my_vector <- unique(sub$AphiaID)
my_vector <- my_vector[complete.cases(my_vector)]

##### check #####
# length(my_vector)

##### make groups of 50 (because the API limit is 50) #####
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
# species_list_original %>% filter(status != 'accepted') %>%
#   group_by(AphiaID, valid_AphiaID, valid_AphiaID_complete) %>%
#   summarize(n=n()) %>% View()

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
# View(species_list)
# table(is.na(species_list$AphiaID))
# table(species_list$status)
# dim(species_list)

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
                  "Xeniidae", "Taiaroidae", 'Clavulariidae')

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
    ScientificName %in% c('Malacalcyonacea')  ~ 'soft coral',
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
sub_enhanced3 %>%
  group_by(VerbatimScientificName, ScientificName) %>%
  summarize(n=n()) %>% View()

##### change the following to deal with new species (OPTIONAL) ######
# ## set ScientificName
# sub_enhanced3 <- sub_enhanced3  %>%
#   mutate(ScientificName = trimws(ScientificName),  # Remove extra spaces
#          ScientificName = case_when(
#            VerbatimScientificName == "Aaptos mucronatus n.sp." ~  "Aaptos mucronatus n.sp.",
#            VerbatimScientificName == "Aaptos n.sp. (new undescribed species)" ~ "Aaptos n.sp.",
#            VerbatimScientificName == "Cladocroce cylindrica n. sp." ~ "Cladocroce cylindrica n. sp.",
#            VerbatimScientificName == "Forcepia atka n.sp." ~ "Forcepia atka n.sp.",
#            VerbatimScientificName == "Haliclona (Haliclona) n.sp. (new undescribed species)" ~ "Haliclona (Haliclona) n.sp.",
#            VerbatimScientificName == "Haliclona (Reniera) n.sp. (new undescribed species)" ~  "Haliclona (Reniera) n.sp." ,
#            VerbatimScientificName == "Homaxinella fruticosa n.sp." ~ "Homaxinella fruticosa n.sp.",
#            VerbatimScientificName == "Julavis borealis n.sp." ~ "Julavis borealis n.sp.",
#            VerbatimScientificName == "Megaciella aurantia n.sp." ~ "Megaciella aurantia n.sp.",
#            VerbatimScientificName == "Polycapus rubrum n. gen. n. sp." ~ "Polycapus rubrum n. gen. n. sp.",
#            VerbatimScientificName == "Stelletta plana n.sp." ~ "Stelletta plana n.sp.",
#            TRUE ~ as.character(ScientificName)  # Keep original name if no match
#          ))
#
#
# ## set ScientificNameAuthorship to null
# sub_enhanced3 <- sub_enhanced3  %>%
#   mutate(VerbatimScientificName = trimws(VerbatimScientificName),  # Remove extra spaces
#          ScientificNameAuthorship = case_when(
#            VerbatimScientificName == "Aaptos mucronatus n.sp." ~  "",
#            VerbatimScientificName == "Aaptos n.sp. (new undescribed species)" ~ "",
#            VerbatimScientificName == "Cladocroce cylindrica n. sp." ~ "",
#            VerbatimScientificName == "Forcepia atka n.sp." ~ "",
#            VerbatimScientificName == "Haliclona (Haliclona) n.sp. (new undescribed species)" ~ "",
#            VerbatimScientificName == "Haliclona (Reniera) n.sp. (new undescribed species)" ~  "" ,
#            VerbatimScientificName == "Homaxinella fruticosa n.sp." ~ "",
#            VerbatimScientificName == "Julavis borealis n.sp." ~ "",
#            VerbatimScientificName == "Megaciella aurantia n.sp." ~ "",
#            VerbatimScientificName == "Polycapus rubrum n. gen. n. sp." ~ "",
#            VerbatimScientificName == "Stelletta plana n.sp." ~ "",
#            TRUE ~ ScientificNameAuthorship  # Keep original name if no match
#          ))
#
# ## set AphiaID to null
# sub_enhanced3 <- sub_enhanced3  %>%
#   mutate(ScientificName = trimws(ScientificName),  # Remove extra spaces
#          AphiaID = case_when(
#            VerbatimScientificName == "Aaptos mucronatus n.sp." ~  "",
#            VerbatimScientificName == "Aaptos n.sp. (new undescribed species)" ~ "",
#            VerbatimScientificName == "Cladocroce cylindrica n. sp." ~ "",
#            VerbatimScientificName == "Forcepia atka n.sp." ~ "",
#            VerbatimScientificName == "Haliclona (Haliclona) n.sp. (new undescribed species)" ~ "",
#            VerbatimScientificName == "Haliclona (Reniera) n.sp. (new undescribed species)" ~  "" ,
#            VerbatimScientificName == "Homaxinella fruticosa n.sp." ~ "",
#            VerbatimScientificName == "Julavis borealis n.sp." ~ "",
#            VerbatimScientificName == "Megaciella aurantia n.sp." ~ "",
#            VerbatimScientificName == "Polycapus rubrum n. gen. n. sp." ~ "",
#            VerbatimScientificName == "Stelletta plana n.sp." ~ "",
#            TRUE ~ as.character(AphiaID)  # Keep original name if no match
#          ))
#
# ## set Species
# sub_enhanced3 <- sub_enhanced3  %>%
#   mutate(Species = case_when(
#     VerbatimScientificName == "Aaptos mucronatus n.sp." ~  "mucronatus",
#     VerbatimScientificName == "Aaptos n.sp. (new undescribed species)" ~ "",
#     VerbatimScientificName == "Cladocroce cylindrica n. sp." ~ "cylindrica",
#     VerbatimScientificName == "Forcepia atka n.sp." ~ "atka",
#     VerbatimScientificName == "Haliclona (Haliclona) n.sp. (new undescribed species)" ~ "",
#     VerbatimScientificName == "Haliclona (Reniera) n.sp. (new undescribed species)" ~  "" ,
#     VerbatimScientificName == "Homaxinella fruticosa n.sp." ~ "fruticosa",
#     VerbatimScientificName == "Julavis borealis n.sp." ~ "borealis",
#     VerbatimScientificName == "Megaciella aurantia n.sp." ~ "aurantia",
#     VerbatimScientificName == "Polycapus rubrum n. gen. n. sp." ~ "rubrum",
#     VerbatimScientificName == "Stelletta plana n.sp." ~ "plana",
#     TRUE ~ as.character(Species)  # Keep original name if no match
#   ))
#
# ## set TaxonRank
# sub_enhanced3 <- sub_enhanced3  %>%
#   mutate(TaxonRank = case_when(
#     VerbatimScientificName == "Aaptos mucronatus n.sp." ~  "species",
#     VerbatimScientificName == "Aaptos n.sp. (new undescribed species)" ~ "species",
#     VerbatimScientificName == "Cladocroce cylindrica n. sp." ~ "species",
#     VerbatimScientificName == "Forcepia atka n.sp." ~ "species",
#     VerbatimScientificName == "Haliclona (Haliclona) n.sp. (new undescribed species)" ~ "species",
#     VerbatimScientificName == "Haliclona (Reniera) n.sp. (new undescribed species)" ~  "species" ,
#     VerbatimScientificName == "Homaxinella fruticosa n.sp." ~ "species",
#     VerbatimScientificName == "Julavis borealis n.sp." ~ "species",
#     VerbatimScientificName == "Megaciella aurantia n.sp." ~ "species",
#     VerbatimScientificName == "Polycapus rubrum n. gen. n. sp." ~ "species",
#     VerbatimScientificName == "Stelletta plana n.sp." ~ "species",
#     TRUE ~ as.character(TaxonRank)  # Keep original name if no match
#   ))
#
# ## set Genus
# sub_enhanced3 <- sub_enhanced3  %>%
#   mutate(Genus = case_when(
#     VerbatimScientificName == "Polycapus rubrum n. gen. n. sp." ~ "Polycapus",
#     VerbatimScientificName == "Julavis borealis n.sp." ~ 'Julavis',
#     TRUE ~ as.character(VerbatimScientificName)  # Keep original name if no match
#   ))


##### check #####
# sub_enhanced3 %>%  filter(VerbatimScientificName == "Aaptos mucronatus n.sp.") %>%
#   pull(Species)
#
# sub_enhanced3 %>%  filter(VerbatimScientificName == "Polycapus rubrum n. gen. n. sp.") %>%
#   pull(Species)

sub_enhanced3 %>%  filter(VerbatimScientificName == "Julavis borealis n.sp.") %>%
  pull(Species)


# sub_enhanced3$IdentificationComments
# sub_enhanced3$VernacularNameCategory
# table(sub_enhanced3$VernacularNameCategory, useNA = 'always')
# View(sub_enhanced3)
# dim(sub_enhanced3)
# dim(sub)
# length(sub$CatalogNumber) - length(sub_enhanced3$CatalogNumber)
#
# x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
# sub %>% filter(CatalogNumber %in% x) %>%
#   group_by(CatalogNumber,
#            VerbatimScientificName,
#            ScientificName,
#            VernacularNameCategory,
#            valid_AphiaID) %>%
#   summarize(n=n()) %>% View()

# filt %>% filter(ScientificName == 'Callistephanus') %>% pull(VernacularNameCategory) %>% table()
#
# x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
# sub_enhanced %>% filter(CatalogNumber %in% x) %>%
#   group_by(AphiaID, Phylum, Class, Order, Suborder, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()
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


##### export result to csv (export to CSV) #####
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
filename <- '20250331-1_NOAA_CBNMS_ROV_2014_142457'
sub <- read.csv(paste(filename, '.csv', sep = ''))

##### check #####
# table(sub$Flag)
# sub %>% filter(Flag == 0) %>% group_by(VernacularNameCategory, Phylum, Class, Order, Family, ScientificName) %>%
#   summarize(n=n()) %>% View()
# table(sub$IndividualCount, useNA = 'always')
# filt %>% filter(grepl('Eiwa', VehicleName)) %>% pull(VehicleName) %>% table()
# sub %>% filter(grepl('Eiwa', VehicleName)) %>% pull(VehicleName) %>% table()
# filt %>% filter(grepl('NOAA_SH-22-09', DatasetID)) %>% pull(VehicleName) %>% table()
# filt %>% filter(grepl('SH', SurveyID)) %>% pull(SurveyID) %>% table()

##### run QA report and post #####
## manual change version of dashboard version number is required
rmarkdown::render("C:/rworking/deepseatools/code/20250401-0_rmd_accession_qa_dashboard.Rmd",
                  output_file =  paste(filename,".docx", sep=''),
                  output_dir = 'C:/rworking/deepseatools/reports')

## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1SsgNnhlXJDz07e1BXBIEhlFP5Qg-a3p4"
setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".docx", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".docx", sep=''),
             overwrite = T)

##### check #####
filt %>% filter(grepl('Fulmar', Vessel)) %>% pull(VehicleName) %>% table(useNA = 'always')

##### manual map check #####
x <- sub %>% filter(
  Flag ==  0) %>%
  group_by(CatalogNumber, Latitude, Longitude, FlagReason) %>%
  summarize(n=n())
points <- st_as_sf(x, coords = c("Longitude", "Latitude"), crs = 4326)
st_write(points, "C:/rworking/deepseatools/indata/sub_geo.shp", delete_dsn = T)

library(leaflet)
library(sf)

# Create a Leaflet map with an ocean relief background
leaflet(data = points) %>%
  addProviderTiles("Esri.OceanBasemap") %>%  # Ocean relief tiles
  addCircleMarkers(
    radius = 4,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~CatalogNumber  # Shows CatalogNumber on click
  )
