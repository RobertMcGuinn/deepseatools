##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240808
## purpose: global taxonomy patch for 20240726-0 'Mick Jagger' [Mick Stanley]

##### linkage #####
## manual input here
filename <- '20250711-0_global_taxonomy_patch_NatDB_20250409.R' ## entry: name of the current file.  make sure to include '.R'
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
redmine_link <- 'https://vlab.noaa.gov/redmine/issues/147793'

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(worrms)
library(openxlsx)
library(taxize)
library(googlesheets4)
library(googledrive)

##### authorizations #####
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### load current NDB version #####
source('c:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### fix one -999 AphiaID that actually has one #####
filt_fixed <- filt %>%
  mutate(AphiaID = ifelse(AphiaID == -999 & ScientificName == 'Aaptos', 132064, AphiaID))

##### check #####
# length(filt$CatalogNumber)-length(filt_fixed$CatalogNumber)
filt_fixed%>% filter(AphiaID == -999) %>% pull(ScientificName) %>% unique()
filt %>% filter(ScientificName == 'Aaptos') %>% pull(AphiaID) %>% table()

x <-wm_name2id ('Aaptos')
wm_record(x) %>% View()

##### deal with duplicates in filt_fixed (breaks join operations otherwise) #####
# filt_fixed <- filt_fixed %>%
#   mutate(AphiaID = ifelse(AphiaID == 602367, 125286, AphiaID)) %>%
#   mutate(AphiaID = ifelse(AphiaID == 246100, 170653, AphiaID)) %>%
#   mutate(AphiaID = ifelse(AphiaID == 286696, 1393629, AphiaID))

# 602367 125286 # Clavularia
# 246100 170653 # Polymastia pacifica
# 286696 1393629 # Stylatula gracilis

##### change incorrect AphiaIDs #####
# filt_fixed <- filt_fixed %>%
#   mutate(AphiaID = ifelse(AphiaID == 196178, 196168, AphiaID)) %>%
#   mutate(AphiaID = ifelse(AphiaID == 1287836, 1287835, AphiaID))

##### load the taxonomy table from CSV #####
tax <- read_sheet('1v3yZO7ATMtV-wp9lePl2pV9-ycxFo3VGVrR_SIunbdQ')
taxfl <- read_sheet('1ZfR4wiBQbDsFGpYXXDjHrsF1QJyoCMqfocmxbpBPo9M')
taxch <- read_sheet('11FgDuNmIZRSf2W4MeFqn2h8pOekvQEP2nG4vcy46pY8')

##### check #####
filt_fixed %>% filter(AphiaID == -999) %>%
  pull(ScientificName) %>%
  unique()
#
# filt_fixed %>%
#   filter(ScientificName == 'Callogorgia cracentis') %>%
#   pull(AphiaID)

##### ***** #####
##### filter #####
# filt_fixed <- filt_fixed %>%
#   filter(AphiaID == '1473600')

##### grab all AphiaIDs #####
aphiaIDs <- unique(filt_fixed$AphiaID)

##### remove any -999 from aphiaIDs #####
aphiaIDs <- aphiaIDs[aphiaIDs != -999]

##### create vector from incoming AphiaIDs #####
my_vector <- aphiaIDs

##### check #####
# length(my_vector)

##### make groups of 50 (because the API limit is 50) #####
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get WoRMS records by AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list_original <- df

##### check #####
dim(species_list_original)
# View(species_list_original)
length(unique(filt$AphiaID))-1

## view any duplicated entries in the table
yo <- species_list_original %>% filter(duplicated(scientificname) == T) %>%
  pull(scientificname)
yo <- na.omit(yo)
yo

species_list_original %>%
  filter(is.na(valid_AphiaID) == T) %>%
  group_by(scientificname,
           AphiaID,
           valid_AphiaID,
           status,
           valid_name,
           isExtinct,
           isMarine) %>%
  summarize(n=n()) %>%
  View()

##### create a complete valid AphiaID list #####
species_list_original <- species_list_original %>%
  mutate(valid_AphiaID_complete = ifelse(is.na(valid_AphiaID) == T,
                                         AphiaID,
                                         valid_AphiaID))

##### check #####
length(species_list_original$AphiaID)
length(unique(species_list_original$AphiaID))

length(species_list_original$valid_AphiaID_complete)
length(unique(species_list_original$valid_AphiaID_complete))

table(species_list_original$valid_AphiaID, useNA = 'always')

length(aphiaIDs)

species_list_original %>% pull(AphiaID) %>% length()
species_list_original %>% pull(valid_AphiaID) %>% length()

species_list_original %>% filter(is.na(valid_AphiaID_complete) == T) %>%
  pull(valid_AphiaID)

species_list_original %>% filter(status != 'accepted') %>%
  group_by(AphiaID, scientificname, valid_AphiaID,
           valid_AphiaID_complete) %>%
  summarize(n=n()) %>% View()

table(is.na(species_list_original$valid_AphiaID_complete))

##### ***** #####
##### create vector from valid AphiaIDs #####
my_vector <- unique(species_list_original$valid_AphiaID_complete)

## get rid of any NA values.
my_vector <- na.omit(my_vector)

##### check #####
length(my_vector)
table(is.na(my_vector))
View(my_vector)
summary(my_vector)
length(unique(filt$AphiaID))

##### make groups of 50 (because the API limit is 50) #####
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by the valid AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list <- df

## get rid of any NA rows
species_list <-
  species_list %>% filter(is.na(AphiaID) == F)

##### check #####
# View(species_list)
# table(is.na(species_list$AphiaID))
# table(species_list$status)
# table(species_list_original$status)
# dim(species_list)
# dim(species_list_original)
#
# species_list %>% filter(status == 'uncertain') %>%
#   group_by(AphiaID, status, valid_AphiaID, class, scientificname, valid_name) %>%
#   summarize(n=n()) %>% View()


# accepted      nomen dubium
# 4055                11
# nomen nudum taxon inquirendum
# 1                 7
# temporary name        unassessed
# 2                 3
# uncertain
# 21

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
# wm_common_id(135161)
# wm_common_id(1245747)
# wm_common_id(395098)
# wm_common_id(1116761)
#
# filt_fixed %>% filter(VernacularName == 'snowflake coral') %>%
#   pull(ScientificName)

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

# dim(species_list)
# dim(classification)
# dim(vernaculars)
# dim(synonyms)
# #
# setdiff(species_list$AphiaID, species_list_original$AphiaID)# introduced by changes from original
# setdiff(species_list_original$AphiaID, species_list$AphiaID)# changed from original

#
# setdiff(unique(species_list$AphiaID), unique(filt$AphiaID))

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

##### save objects for future use #####
# save multiple objects
save(species_list_original,
     species_list,
     classification,
     vernaculars,
     synonyms,
     file = 'c:/rworking/deepseatools/indata/20250714-0_taxonomy_objects.Rdata')

## clean up everything except filt
# rm(list=setdiff(ls(), c("filt")))

# rm(species_list_original,
#    species_list,
#    classification,
#    vernaculars,synonyms)

##### load objects from past work #####
## load the objects back ()
load('../indata/20250714-0_taxonomy_objects.Rdata')

##### check #####
# table(species_list$status, useNA = 'always')
# table(species_list_original$status, useNA = 'always')
# table(species_list_original$status, useNA = 'always')
# setdiff(species_list$AphiaID, species_list_original$AphiaID)# introduced by changes from original
# setdiff(species_list_original$AphiaID, species_list$AphiaID)# changed from original
#
# joined4 %>%
#   filter(CatalogNumber == '183413') %>%
#   group_by(scientificname, valid_name) %>%
#   summarize(n=n())
#

##### left join the species list from above with all of the other API tables #####
## joining species_list, classification, vernaculars, and synonyms
by <- join_by(AphiaID == AphiaID)
joined2 <- left_join(species_list, classification, by)

by <- join_by(AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

by <- join_by(AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

##### check #####
# length(names(species_list)) + length(names(classification))
# length(names(joined2))
#
# joined4 %>% filter(Subphylum == 'Anthozoa') %>% pull(Class) %>% unique()
#
# joined2 %>% filter(scientificname == 'Distichoptilum rigidum') %>%
#   group_by(AphiaID, Genus) %>%
#   summarize(n=n())
#
# i <- 1392554
# wm_classification(i)
#
# classification %>% filter(AphiaID == i)
#
# species_list %>% filter(AphiaID == i) %>%
#   group_by(AphiaID,valid_AphiaID) %>%
#   summarize(n=n())

# names(joined4)
# setdiff(joined4$AphiaID, species_list_original$valid_AphiaID_complete)
# setdiff(species_list_original$valid_AphiaID_complete, joined4$AphiaID)
# View(species_list_original)
#

# joined4 %>% names()

##### create taxonomy table: join original table with the new table #####
joined4$AphiaID2 <- joined4$AphiaID
by <- join_by(valid_AphiaID_complete == AphiaID2)
taxonomy_table <- left_join(species_list_original, joined4, by)

##### check #####
# View(taxonomy_table)
# names(taxonomy_table)

# taxonomy_table %>% filter(status.x == 'unaccepted') %>%
#   group_by(AphiaID.x, AphiaID.y, scientificname.x, scientificname.y) %>%
#   summarize(n=n()) %>% View()
#
# taxonomy_table %>% filter(AphiaID.x != AphiaID.y) %>%
#   group_by(AphiaID.x, AphiaID.y, status.x, status.y, scientificname.x, scientificname.y) %>%
#   summarize(n=n()) %>% View()
#
# filt %>% filter(ScientificName == 'Geodia japonica var. spherulifera') %>%
#   group_by(VerbatimScientificName, ScientificName, IdentificationComments) %>%
#   summarize(n=n()) %>% View()

##### add taxonomy to filt #####
by <- join_by(AphiaID == AphiaID.x)
sub_enhanced <- left_join(filt_fixed, taxonomy_table, by)

##### check #####
# x<-setdiff(filt$CatalogNumber, sub_enhanced$CatalogNumber)
# filt %>% filter(CatalogNumber %in% x) %>%
#   pull(AphiaID)
#
# table(sub_enhanced$phylum.y, useNA = 'always')
#
# sub_enhanced %>% filter(AphiaID != AphiaID.y) %>%
#   group_by(AphiaID, AphiaID.y, VerbatimScientificName, ScientificName, scientificname.x, scientificname.y, Phylum.x, phylum.y) %>%
#   summarize(n=n()) %>% View()
#
# table(sub_enhanced$phylum.x, useNA = 'always')
#
# sub_enhanced %>% filter(Phylum.x != Phylum.y) %>%
#   group_by(Phylum.x, Phylum.y, phylum.x, phylum.y, Class.x, Genus.x, Species.x,
#                           ScientificName, scientificname.x, scientificname.y, AphiaID, AphiaID.y, CatalogNumber)%>%
#   summarize(n=n()) %>% View()
#
#
# sub_enhanced %>% filter(Class.x != class.y) %>%
#   group_by(Class.x, class.y,
#            AphiaID, AphiaID.y, ScientificName, scientificname.y, VerbatimScientificName) %>%
#   summarize(n=n()) %>% View()
#
# filt %>% filter(ScientificName == 'Keroeididae') %>%
#   pull(AphiaID) %>% table()

##### exclude where AphiaID is -999 #####
null_aphiaIDs <- sub_enhanced %>% filter(AphiaID == -999) %>%
  pull(CatalogNumber) %>%
  unique()

## define not in
`%notin%` <- Negate(`%in%`)

sub_enhanced <-
  sub_enhanced %>%
  filter(CatalogNumber %notin% null_aphiaIDs)

##### check #####

## https://www.marinespecies.org/aphia.php?p=taxdetails&id=196168 # Keroeididae Kinoshita, 1910
## https://www.marinespecies.org/aphia.php?p=taxdetails&id=1287835 # Stichopathes luetkeni

sub_enhanced %>% filter(AphiaID == '196168') %>% pull(ScientificName) %>% unique() # 196178
sub_enhanced %>% filter(AphiaID == '1287835') %>% pull(ScientificName) %>% unique() # 1287836

sub_enhanced %>% filter(is.na(phylum.y) == T) %>%
  pull(ScientificName) %>%
  unique()

table(sub_enhanced$Phylum.x)
table(sub_enhanced$Phylum.y)

x <- sub_enhanced %>% filter(Phylum.y == 'Echinodermata' | Phylum.y == 'Arthropoda') %>%
  group_by(scientificname.x, scientificname.y, ScientificName,  phylum.x, phylum.y, Phylum.x, Phylum.y, AphiaID, AphiaID.y, CatalogNumber) %>%
  summarize(n=n()) %>% pull(CatalogNumber) %>% unique()

x <- c(1010837,1015581,522726,525492)

sub_enhanced %>% filter(CatalogNumber %in% x) %>%
  group_by(ScientificName, Class.x, Order.x) %>% summarize(n=n())

sub_enhanced %>% filter(Phylum.y == 'Echinodermata' | Phylum.y == 'Arthropoda') %>%
  group_by(scientificname.x, scientificname.y, ScientificName,  phylum.x, phylum.y, Phylum.x, Phylum.y, AphiaID, AphiaID.y, CatalogNumber) %>%
  summarize(n=n()) %>% View()


##### gather information into proper variables#####

## This optional step 1 (be careful) meant to preserve names where needed inside of IdentificationComments.
## This was done on Aretha Franklin and results of this operation will be present within Mick Jagger
## Don't need to do this again.

# sub_enhanced$IdentificationComments <-
#   paste('DSCRTP_og_scientificname:',
#         ' ',
#         sub_enhanced$ScientificName,
#         ' | ',
#         sub_enhanced$IdentificationComments,
#         sep = '')

## this step overwrites (be careful)
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

##### check #####
# x <- setdiff(sub_enhanced$AphiaID, species_list_original$AphiaID)# introduced by changes from original
# y <- setdiff(species_list_original$AphiaID, sub_enhanced$AphiaID)# changed from original
#
# filt %>% filter(AphiaID == '1392016') %>%
#   group_by(VerbatimScientificName, IdentificationComments, ScientificName, DatasetID) %>%
#   summarize(n=n()) %>% View()

##### apply taxonomic filter #####
sub_enhanced_filter <- sub_enhanced

# %>%
#   filter(Subphylum == 'Vertebrata' |
#            Phylum == 'Cnidaria' |
#            Phylum == 'Porifera')
#
# `%notin%` <- Negate(`%in%`)
# sub_enhanced_filter <- sub_enhanced_filter %>%
#   filter(Class %notin% c('Scyphozoa', 'Thalicacea', 'Ascidiacea'))
#
# sub_enhanced_filter <- sub_enhanced_filter %>%
#   filter(Family %notin% c('Acroporidae', 'Poritidae'))
#
# sub_enhanced_filter <- sub_enhanced_filter %>%
#   filter(Genus %notin% c('Pocillopora'))
#
# sub_enhanced_filter <- sub_enhanced_filter %>%
#   filter(Order == 'Scleractinia' |
#            Order == 'Antipatharia' |
#            Genus == 'Savalia' |
#            Genus == 'Kulamanamana' |
#            Genus == 'Gerardia' |
#            Family == 'Stylasteridae' |
#            Order  == 'Alcyonacea' |
#            Order ==  'Gorgonacea' |
#            Order ==  'Helioporacea' |
#            Order == 'Pennatulacea' |
#            Order == 'Scleralcyonacea' |
#            Family == 'Stylasteridae' |
#            Genus == 'Solanderia' |
#            Genus == 'Janaria' |
#            Genus == 'Hydrocorella' |
#            Genus == 'Hydrodendron' |
#            Phylum == 'Chordata' |
#            Phylum == 'Porifera' |
#            Order == 'Malacalcyonacea' |
#            Order == 'Octocorallia incertae sedis' |
#            ScientificName == 'Octocorallia'
# )

##### check #####
# dim(sub_enhanced) - dim(sub_enhanced_filter)
# setdiff(sub_enhanced$ScientificName, sub_enhanced_filter$ScientificName)

## check on records that were excluded by the filtering criteria
# excluded_cats <- setdiff(sub_enhanced$CatalogNumber, sub_enhanced_filter$CatalogNumber)
#
# sub_enhanced %>% filter(CatalogNumber %in% excluded_cats) %>%
#   group_by(status.x, status.y, VerbatimScientificName, TaxonRank, ScientificName, AphiaID, AphiaID.y, Phylum, Class, Order, Family, Genus) %>%
#   summarize(n=n()) %>% View


# sub_enhanced %>% filter(ScientificName == 'Distichoptilum rigidum') %>%
#   group_by(Genus, Genus.y, genus.x, genus.y, ScientificName, scientificname.x, scientificname.y) %>%
#   summarize(n=n()) %>% View()
#
# sub_enhanced %>% filter(ScientificName == 'Octocorallia') %>%
#   group_by(ScientificName, AphiaID, VernacularNameCategory) %>%
#   summarize(n=n()) %>% View
#
# dim(species_list_original)
# summary(species_list_original)
#
# filt %>% filter(ScientificName == "Clavularia") %>%
#   group_by(ScientificName, VerbatimScientificName, AphiaID) %>%
#   summarize(n=n())
#
# table(sub_enhanced_filter$Phylum, useNA = 'always')
# table(sub_enhanced_filter$Subphylum, useNA = 'always')
#
# sub_enhanced_filter %>%
#   group_by(VernacularName, AphiaID, Phylum, Subphylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()
#
# filt %>%
#   filter(Family == 'Corallimorphidae') %>%
#   group_by(VernacularName, AphiaID, ScientificName) %>%
#   summarize(n=n()) %>% View()
#
# sub_enhanced_filter %>% filter(Phylum == 'Cnidaria') %>%
#   group_by(ScientificName, VernacularName) %>%
#   summarize(n=n()) %>% View()
#
# sub_enhanced_filter %>% filter(Phylum == 'Porifera') %>%
#   group_by(ScientificName, VernacularName) %>%
#   summarize(n=n()) %>% View()
#
# sub_enhanced_filter %>% filter(Phylum == 'Chordata') %>%
#   group_by(ScientificName, VernacularName) %>%
#   summarize(n=n()) %>% View()

##### ***** #####
##### assign VernacularNameCategory if they don't already have one #####
## define not in
`%notin%` <- Negate(`%in%`)

gorgfamilies <- c("Paramuriceidae","Chrysogorgiidae","Dendrobrachiidae",
                  "Ellisellidae", "Isididae",
                  "Pleurogorgiidae", "Primnoidae",
                  "Acanthogorgiidae", "Gorgoniidae","Keroeididae",
                  "Plexauridae", "Anthothelidae",
                  "Coralliidae", "Melithaeidae",
                  "Paragorgiidae", "Parisididae","Spongiodermidae", "Subergorgiidae",
                  "Victorgorgiidae", "Keratoisididae", "Malacalcyonacea incertae sedis", 'Mopseidae',
                  'Chelidonisididae')

softfamilies <- c("Alcyoniidae","Aquaumbridae", "Ifalukellidae",
                  "Nephtheidae","Nidaliidae", "Paralcyoniidae",
                  "Xeniidae", "Taiaroidae", 'Cornulariidae', 'Haimeidae')

othercorallikehydrozoanfamilies <- c("Solanderiidae", "Haleciidae")

stonycoralbranching <- tax %>%
  filter(VernacularNameCategory == 'stony coral (branching)') %>%
  pull(ScientificName)

stonycoralcupcoral <- tax %>%
  filter(VernacularNameCategory == 'stony coral (cup coral)') %>%
  pull(ScientificName)

sub_enhanced2 <- sub_enhanced_filter %>%
  mutate(VernacularNameCategory = case_when(
    is.na(VernacularNameCategory) & Phylum %in% c('Chordata') ~ 'fish',
    is.na(VernacularNameCategory) & TaxonRank %in% c('Order') & Order %in% c('Alcyonacea') ~ 'alcyonacean (unspecified)',
    is.na(VernacularNameCategory) & Order %in% c('Antipatharia') ~ 'black coral',
    is.na(VernacularNameCategory) & Class %in% c('Calcarea')~ 'calcareous sponge',
    is.na(VernacularNameCategory) & Class %in% c('Demospongiae') ~ 'demosponge',
    is.na(VernacularNameCategory) & Class %in% c('Hexactinellida') ~ 'glass sponge',
    is.na(VernacularNameCategory) & Class %in% c('Homoscleromorpha') ~ 'homoscleromorph sponge',
    is.na(VernacularNameCategory) & Family %in% c('Parazoanthidae') ~ 'gold coral',
    is.na(VernacularNameCategory) & Family %in% gorgfamilies ~ 'gorgonian coral',
    is.na(VernacularNameCategory) & Family %in% softfamilies ~ 'soft coral',
    is.na(VernacularNameCategory) & Genus %in% c('Aspera', 'Verseveldtia') ~ 'soft coral',
    is.na(VernacularNameCategory) & Order %in% c('Malacalcyonacea') ~ 'soft coral',
    is.na(VernacularNameCategory) & Order %in% c('Anthoathecata') & Family %notin% c('Solanderiidae') ~ 'lace coral',
    is.na(VernacularNameCategory) & Family %in% c('Lithotelestidae', 'Aulopsammiidae') ~ 'lithotelestid coral',
    is.na(VernacularNameCategory) & Family %in% othercorallikehydrozoanfamilies ~ 'other coral-like hydrozoan',
    is.na(VernacularNameCategory) & Superfamily %in% c('Pennatuloidea') ~ 'sea pen',
    is.na(VernacularNameCategory) & ScientificName %in% c('Porifera') ~ 'sponge',
    is.na(VernacularNameCategory) & Suborder %in% c('Stolonifera') ~ 'stoloniferan coral',
    is.na(VernacularNameCategory) & Family %in% c('Clavulariidae') ~ 'stoloniferan coral',
    is.na(VernacularNameCategory) & Genus %in% c('Clavularia', 'Sarcodictyon', 'Scleranthelia', 'Pseudocladochonus') ~ 'stoloniferan coral',
    is.na(VernacularNameCategory) & Order %in% c('Scleractinia') & TaxonRank %in% c('Order')  ~ 'stony coral (unspecified)',
    is.na(VernacularNameCategory) & ScientificName %in% stonycoralbranching ~ 'stony coral (branching)',
    is.na(VernacularNameCategory) & ScientificName %in% c(stonycoralcupcoral, 'Desmophyllum hourigani') ~ 'stony coral (cup coral)',
    is.na(VernacularNameCategory) & Genus %in% c('Acanthogorgia', 'Hypnogorgia', 'Thelogorgia',
                                                 'Stephanogorgia', 'Helicogorgia', 'Distichogorgia',
                                                 'Xenogorgia', 'Caliacis', 'Briareopsis', 'Elasmogorgia',
                                                 'Pseudothesea', 'Bayergorgia', 'Flagelligorgia') ~ 'gorgonian coral',
    is.na(VernacularNameCategory) & Genus %in% c('Hydrodendron') ~ 'other coral-like hydrozoan',
    is.na(VernacularNameCategory) & Genus %in% c('Heterocyathus') ~ 'stony coral (cup coral)',
    is.na(VernacularNameCategory) & Genus %in% c('Caryophyllia', 'Paracyathus', 'Asterosmilia', 'Heteropsammia') ~ 'stony coral (cup coral)',
    is.na(VernacularNameCategory) & Family %in% c('Micrabaciidae', 'Flabellidae', 'Turbinoliidae', 'Astrangiidae',
                                                  'Rhizangiidae', 'Anthemiphylliidae') ~ 'stony coral (cup coral)',
    is.na(VernacularNameCategory) & ScientificName %in% c('Caryophylliidae') ~ 'stony coral (unspecified)',
    is.na(VernacularNameCategory) & Genus %in% c('Telestula') ~ 'stoloniferan coral',
    is.na(VernacularNameCategory) & ScientificName %in% c('Dendrophylliidae') ~ 'stony coral (unspecified)',
    is.na(VernacularNameCategory) & Genus %in% c('Leptoseris', 'Dactylotrochus', 'Anomocora', 'Paraconotrochus',
                                                 'Trochocyathus', 'Cladopsammia', 'Balanophyllia', 'Deltocyathus', 'Tubastraea') ~ 'stony coral (cup coral)',
    is.na(VernacularNameCategory) & Genus %in% c('Madracis', 'Solenosmilia', 'Atlantia') ~ 'stony coral (branching)',
    is.na(VernacularNameCategory) & ScientificName %in% c('Pocilloporidae') ~ 'stony coral (unspecified)',
    is.na(VernacularNameCategory) & ScientificName %in% c('Octocorallia', 'Octocorallia incertae sedis', 'Scleralcyonacea') ~ 'insufficient taxonomic resolution',
    is.na(VernacularNameCategory) & ScientificName %in% c('Verseveldtia granulosa') ~ 'Verseveldtia granulosa',
    TRUE ~ VernacularNameCategory
  ))
##### check #####
table(sub_enhanced2$Phylum)
table(sub_enhanced$Phylum)

table(sub_enhanced2$VernacularNameCategory, useNA = 'always')

x <- c(1010837,1015581,522726,525492)
sub_enhanced2 %>% filter(CatalogNumber %in% x) %>%
  group_by(ScientificName, VernacularNameCategory) %>%
  summarize(n=n())

sub_enhanced2 %>%
  filter(VernacularNameCategory == '') %>%
  group_by(AphiaID, Class, Order, Family, Genus, ScientificName) %>%
  summarize(n=n()) %>% View()

#
# filt_fixed %>% filter(Genus == 'Flagelligorgia') %>%
#   group_by(VernacularNameCategory, Genus, ScientificName) %>%
#   summarize(n=n())
#
# filt_fixed %>% filter(Family == 'Dendrophylliidae') %>%
#   group_by(VernacularNameCategory, Genus, ScientificName) %>%
#   summarize(n=n()) %>% pull(VernacularNameCategory) %>% unique
#
# filt_fixed %>% filter(Order == 'Scleralcyonacea') %>%
#   group_by(VernacularNameCategory, Genus, ScientificName) %>%
#   summarize(n=n()) %>% pull(VernacularNameCategory) %>% table(useNA = 'always')
#
# sub_enhanced2 %>% filter(Order == 'Octocorallia incertae sedis') %>%
#   group_by(AphiaID, Class, Order, Family , Genus, Species) %>%
#   summarize(n=n()) %>% View
#
# sub_enhanced2 %>% filter(ScientificName == 'Hypnogorgia pendula') %>%
#   group_by(AphiaID, Phylum, Class, Order, Family, Genus, Species, ScientificName) %>%
#   summarize(n=n()) %>% View
#
# x <- sub_enhanced

#
# x <- sub_enhanced2 %>%
#   filter(VernacularNameCategory == '') %>%
#   pull(Family) %>% unique()
#
# filt %>% filter(Genus %in% c('Pocillopora')) %>%
#   group_by(Class, Order, Family, Genus, ScientificName, VernacularNameCategory) %>%
#   summarize(n=n()) %>% View()
#
# filt %>% filter(Family %in% c(filt %>% filter(Family %in% x) %>%
#   group_by(Class, Order, Family, Genus, ScientificName, VernacularNameCategory) %>%
#   summarize(n=n()) %>% View()
# )) %>%
#   group_by(Class, Order, Family, Genus, ScientificName, VernacularNameCategory) %>%
#   summarize(n=n()) %>% View()
#
# sub %>%
#   filter(Family %in% x) %>%
#   group_by(DatasetID, VernacularNameCategory, Family, ScientificName, DepthInMeters, EntryDate) %>%
#   summarize(n=n()) %>% View()
#
##
# filt %>% filter(Order == 'Malacalcyonacea') %>% pull(VernacularNameCategory) %>%
#   table(useNA = 'always')
#
# filt %>% filter(Genus == 'Clavularia') %>% pull(VernacularNameCategory) %>%
#   table(useNA = 'always')
#
# sub_enhanced2 %>%
#   filter(VernacularNameCategory == 'stony coral (cup)') %>%
#   pull(VernacularNameCategory) %>% unique()

# length(sub_enhanced$CatalogNumber)
# length(filt$CatalogNumber)
#
# x <- setdiff(filt$CatalogNumber, sub_enhanced$CatalogNumber)
# filt %>% filter(CatalogNumber %in% x) %>%
#   group_by(AphiaID, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View
#
# x <- setdiff(sub_enhanced$CatalogNumber, sub_enhanced2$CatalogNumber)
# filt %>% filter(CatalogNumber %in% x) %>%
#   group_by(AphiaID, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View

# filt %>% filter(Family == 'Funiculinidae') %>%
#   pull(VernacularNameCategory) %>% unique()

##### get rid of unneeded column names #####
# sub_enhanced2 <- sub_enhanced

names_list <- names(filt)
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

##### decaptitalize TaxonRank #####
sub_enhanced3$TaxonRank <- tolower(sub_enhanced3$TaxonRank)

##### check #####
x <- sub_enhanced3 %>% filter( grepl('Cirrhipathes', ScientificName)) %>%
    group_by(CatalogNumber, ScientificName, VerbatimScientificName, IdentificationComments, AphiaID, Phylum,
             Class, Order, Suborder,
             Family, Genus, Species) %>%
    summarize(n=n()) %>% pull(CatalogNumber) %>% unique()

y <- filt_fixed %>% filter(CatalogNumber %in% x,
                      FishCouncilRegion == 'Gulf of Mexico') %>%
  pull(DatasetID) %>% unique()

y <- setdiff(filt$CatalogNumber, sub_enhanced3$CatalogNumber)
filt %>% filter(CatalogNumber %in% y) %>%
  group_by(CatalogNumber, ScientificName, VerbatimScientificName, IdentificationComments, AphiaID, Phylum,
           Class, Order, Suborder,
           Family, Genus, Species) %>%
  summarize(n=n()) %>% View()

# dim(sub_enhanced3)
# dim(filt)
# length(sub$CatalogNumber) - length(sub_enhanced3$CatalogNumber)
#
x <- setdiff(filt$CatalogNumber, sub_enhanced3$CatalogNumber)

filt %>% filter(CatalogNumber %in% x) %>%
  group_by(CatalogNumber, ScientificName, AphiaID, Phylum, Class, Order, Suborder, Family, Genus, Species) %>%
  summarize(n=n()) %>% View()

## taxa that are -999 AphiaID
x <- setdiff(filt$CatalogNumber, sub_enhanced3$CatalogNumber)
taxa <- filt %>% filter(CatalogNumber %in% x) %>%
  group_by(CatalogNumber, ScientificName, AphiaID, Phylum, Class, Order, Suborder, Family, Genus, Species) %>%
  summarize(n=n()) %>% pull(ScientificName) %>% unique()

## OPTIONAL break up for worms API (optional for long lists)
# taxa1 <- taxa[1:50]
# taxa2 <- taxa[51:66]

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
# sub %>% filter(CatalogNumber %in% x) %>% pull(AphiaID) %>% unique()
#
# table(sub_enhanced3$VernacularNameCategory, useNA = 'always')
#
# sub_enhanced3 %>% filter(VernacularNameCategory == '') %>% pull(Order) %>% unique()
#
# sub_enhanced3 %>% filter(VernacularNameCategory == '') %>%
#   group_by(AphiaID, Phylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()
#
sub_enhanced3 %>% filter(VernacularNameCategory == 'stony coral (branching)') %>%
  group_by(AphiaID, ScientificName, VerbatimScientificName, Phylum, Class, Order, Family, Genus, Species, VernacularName) %>%
  summarize(n=n()) %>% View()

table(sub_enhanced3$VernacularNameCategory, useNA = 'always')
table(filt$VernacularNameCategory, useNA = 'always')

x <- sub_enhanced3 %>% filter(is.na(VernacularNameCategory) == T) %>%
  pull(CatalogNumber)
sub_enhanced3 %>% filter(CatalogNumber %in% x) %>%
  group_by(ScientificName, AphiaID, VernacularNameCategory, TaxonRank, Phylum, Class, Subclass, Order, Suborder, Family, Genus) %>%
  summarize(n=n()) %>% View()

sub_enhanced3 %>% pull(TaxonRank) %>% table(useNA = 'always')
filt %>% pull(TaxonRank) %>% table(useNA = 'always')

filt %>% filter(Genus == "Heterocyathus") %>% pull(VernacularNameCategory) %>% table()

##### save object from Rdata  #####
# save multiple objects
save(sub_enhanced3,
     file = 'c:/rworking/deepseatools/indata/20250714-0_sub_enhanced3.Rdata')

##### load object from Rdata #####
load('indata/20250714-0_sub_enhanced3.Rdata')

##### check #####


filt %>% select(AphiaID, ScientificName) %>%
  group_by(ScientificName) %>%
  summarise(L = length(unique(AphiaID))) %>%
  View()

sub_enhanced3 %>% pull(VernacularNameCategory) %>% table(useNA = 'always')

sub_enhanced3 %>% filter(VernacularNameCategory == 'insufficient taxonomic resolution') %>%
  group_by(ScientificName, VerbatimScientificName) %>%
  summarize(n=n())

x <- filt %>% select(CatalogNumber, ScientificName, Class)
y <- sub_enhanced3 %>% select(CatalogNumber, ScientificName, Class)
z <- left_join(x,
               y,
               by = "CatalogNumber")

z %>%
  filter(is.na(ScientificName.y) == T) %>%
  group_by(CatalogNumber, ScientificName.x, ScientificName.y) %>%
  summarize(n=n()) %>% View()

sub_enhanced3 %>%
  filter(ScientificName == 'Haliclona (Haliclona)') %>%
  pull(AphiaID) %>% table(useNA = 'always')

filt %>%
  filter(ScientificName == 'Haliclona (Haliclona)') %>%
  pull(AphiaID) %>% table(useNA = 'always')

filt %>% group_by(ScientificName, AphiaID) %>%
  summarize(n=n()) %>% View()

filt %>%
  filter(CatalogNumber == '607200') %>%
  pull(ScientificName)

filt %>%
  filter(CatalogNumber == '607200') %>%
  pull(AphiaID)

filt %>%
  filter(AphiaID == '166589') %>% pull(ScientificName) %>% table()

sub_enhanced3 %>%
  filter(AphiaID == '166589') %>% pull(ScientificName) %>% table()

z %>% filter(Class.x != Class.y) %>%
  group_by(ScientificName.x, ScientificName.y, Class.x, Class.y) %>%
  summarize(n=n()) %>% View()

cats <- z %>% filter(ScientificName.x != ScientificName.y) %>% pull(CatalogNumber)
sub_enhanced3 %>% filter(CatalogNumber %in% cats) %>% pull(ScientificName) %>% unique()

filt %>% filter(ScientificName == 'Neopelta aberrans') %>%
  group_by(ScientificName, Class, Order, Family, Genus, Species) %>%
  summarize(n=n()) %>% View()

x <- setdiff(filt$CatalogNumber, sub_enhanced3$CatalogNumber)
filt %>% filter(CatalogNumber %in% x) %>%
  group_by(ScientificName, Class, Order, Family, Genus, Species, AphiaID) %>%
  summarize(n=n()) %>% View()


x <- setdiff(sub_enhanced3$CatalogNumber, filt$CatalogNumber)
filt %>% filter(CatalogNumber %in% x) %>%
  group_by(ScientificName, Class, Order, Family, Genus, Species, AphiaID) %>%
  summarize(n=n()) %>% View()


##### export result to csv (export to CSV) #####
filename <- "20250714-0_global_taxonomy_patch_NDB_20250711-1.csv"
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

