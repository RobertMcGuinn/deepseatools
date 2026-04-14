##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20260414
## purpose: taxonomy patch maker

##### linkage #####
## manual input here
filename <- 'dst_tool_taxonomy_patch_maker.R' ## entry: name of the current file.  make sure to include '.R'
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')

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

##### load the current NDB #####
source('code/dst_tool_load_current_ndb.R')
##### load the taxonomy table from CSV #####
tax <- read_sheet('1v3yZO7ATMtV-wp9lePl2pV9-ycxFo3VGVrR_SIunbdQ')
taxfl <- read_sheet('1ZfR4wiBQbDsFGpYXXDjHrsF1QJyoCMqfocmxbpBPo9M')
taxch <- read_sheet('11FgDuNmIZRSf2W4MeFqn2h8pOekvQEP2nG4vcy46pY8')

##### grab all AphiaIDs #####
aphiaIDs <- unique(sub$AphiaID)

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

##### create a complete valid AphiaID list #####
species_list_original <- species_list_original %>%
  mutate(valid_AphiaID_complete = ifelse(is.na(valid_AphiaID) == T,
                                         AphiaID,
                                         valid_AphiaID))

##### create vector from valid AphiaIDs #####
my_vector <- unique(species_list_original$valid_AphiaID_complete)

## get rid of any NA values.
my_vector <- na.omit(my_vector)

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

##### create taxonomy table: join original table with the new table #####
joined4$AphiaID2 <- joined4$AphiaID
by <- join_by(valid_AphiaID_complete == AphiaID2)
taxonomy_table <- left_join(species_list_original, joined4, by)

##### add taxonomy to filt #####
by <- join_by(AphiaID == AphiaID.x)
sub_enhanced <- left_join(sub, taxonomy_table, by)

##### gather information into proper variables#####
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

##### apply taxonomic filter #####
sub_enhanced_filter <- sub_enhanced %>%
  filter(Subphylum == 'Vertebrata' |
           Phylum == 'Cnidaria' |
           Phylum == 'Porifera')

`%notin%` <- Negate(`%in%`)
sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Class %notin% c('Scyphozoa', 'Thalicacea', 'Ascidiacea'))

sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Family %notin% c('Acroporidae', 'Poritidae'))

sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Genus %notin% c('Pocillopora'))

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
           Order == 'Malacalcyonacea' |
           Order == 'Octocorallia incertae sedis' |
           ScientificName == 'Octocorallia'
)

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


##### get rid of unneeded column names #####

names_list <- names(filt)
sub_enhanced2 <- sub_enhanced2 %>%
  dplyr::select(all_of(names_list))

##### select just the taxonomic variables #####
taxonomy_patch<- sub_enhanced2 %>%
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
taxonomy_patch$TaxonRank <- tolower(taxonomy_patch$TaxonRank)



