##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20250313
## purpose:ingest of Accession:

##### linkage #####
filename <- '' ## manual: for this code file name, match to redmine
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
library(worrms)
library(googledrive)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### check #####
# filt %>% filter(grepl('EX', DatasetID)) %>% pull(DatasetID) %>% unique()

##### ***** NEW VERSION YYYYMMDD-X ***** #####
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

























