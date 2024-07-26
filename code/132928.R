##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20240726
## purpose: see Redmine issue linked below

##### linkage #####
filename <- '132928' ## manual: for this code file name, match to redmine
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
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)
library(worrms)
library(taxize)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### load the corrected aphiaIDs from previous patches #####
patch1 <- read.csv('../indata/patch_for_correct_cf_handling.csv')
patch2 <- read.csv('../indata/patch_for_individual_AphiaID_corrections.csv')
patch_rbind <- rbind(patch1, patch2)

##### check #####
# patch_rbind %>%
#   filter(AphiaID == '132038') %>%
#   pull(CatalogNumber) %>% length()
# ## 7370

filt %>% filter(CatalogNumber == '423553') %>% pull(ScientificName)

##### create vector from incoming AphiaIDs #####
my_vector <- unique(patch_rbind$AphiaID)

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
# species_list %>%  filter(AphiaID == '132038') %>% View()

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
by <- join_by(AphiaID == AphiaID)
joined2 <- left_join(species_list, classification, by)

by <- join_by(AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

by <- join_by(AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

##### check #####
# View(joined4)

##### gather information into proper variables #####
joined4$ScientificName <- joined4$scientificname
joined4$VernacularName <- joined4$vernaculars_list
joined4$TaxonRank <- joined4$rank
joined4$Phylum <- joined4$phylum
joined4$Class <- joined4$class
joined4$Subclass <- joined4$Subclass
joined4$Order <- joined4$order
joined4$Suborder <- joined4$Suborder
joined4$Family <- joined4$family
joined4$Subfamily <- joined4$Subfamily
joined4$Genus <- joined4$genus
joined4$Subgenus <- joined4$Subgenus
joined4$Species <- word(joined4$Species, -1)
joined4$Subspecies <- joined4$Subspecies
joined4$ScientificNameAuthorship <- joined4$authority
joined4$Synonyms <- joined4$synonyms_list

##### check #####
# table(sub_enhanced$VerbatimScientificName, useNA = 'always')
# table(sub_enhanced$ScientificName, useNA = 'always')
#
# sub_enhanced %>% group_by(VerbatimScientificName, ScientificName, AphiaID) %>%
#   summarize(n=n()) %>% View()

# joined4 %>% filter(AphiaID == '132038') %>% View()

##### apply taxonomic filter #####
sub_enhanced_filter <- joined4 %>%
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
           Order == 'Malacalcyonacea' |
           Order == 'Octocorallia incertae sedis'
  )


##### check #####
# table(sub_enhanced_filter$Phylum, useNA = 'always')
# table(sub_enhanced_filter$Subphylum, useNA = 'always')
#
# sub_enhanced_filter %>% filter(Subphylum == 'Medusozoa') %>%
#   group_by(Phylum, Subphylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()
#
# sub_enhanced_filter %>%
#   group_by(AphiaID) %>%
#   summarize(n=n()) %>% View()

##### load the taxonomy table from CSV #####
tax <- read.csv("C:/rworking/deepseatools/indata/tax.csv")


##### assign VernacularNameCategory #####
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
    # Suborder %in% c('Stolonifera') ~ 'stoloniferan coral',
    Family %in% c('Clavulariidae') ~ 'stoloniferan coral',
    Genus %in% c('Clavularia') ~ 'stoloniferan coral',
    Order %in% c('Scleractinia') &
      TaxonRank %in% c('Order')  ~ 'stony coral (unspecified)',
    ScientificName %in% stonycoralbranching ~ 'stony coral (branching)',
    ScientificName %in% stonycoralcupcoral ~ 'stony coral (cup coral)',
    Genus %in% c('Acanthogorgia') ~ 'gorgonian coral',
    Genus %in% c('Hydrodendron') ~ 'other coral-like hydrozoan',
    Genus %in% c('Caryophyllia') ~ 'stony coral (cup coral)',
    Genus %in% c('Letepsammia') ~ 'stony coral (cup coral)',
    Family %in% c('Dendrophylliidae') ~ 'stony coral (unspecified)',
    Genus %in% c('Thelogorgia') ~ 'gorgonian coral',
    TRUE ~ ''))

##### check #####
table(sub_enhanced2$VernacularNameCategory, useNA = 'always')

sub_enhanced2 %>%
  filter(VernacularNameCategory == '') %>%
  pull(ScientificName) %>% unique()

filt %>%
  filter(ScientificName == 'Thelogorgia') %>%
  pull(VernacularNameCategory) %>% unique()

filt %>% filter(CatalogNumber %in% patch_rbind$CatalogNumber,
                AphiaID == '135074') %>% pull(VernacularNameCategory) %>% unique()


##### select just the taxonomic variables #####
sub_enhanced3<- sub_enhanced2 %>%
  select(ScientificName,
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

##### make TaxonRank lower case to conform to our schema #####
sub_enhanced3$TaxonRank <- tolower(sub_enhanced3$TaxonRank)


##### check #####
# dim(sub_enhanced3)
#
# sub_enhanced3 %>%
#   group_by(VernacularNameCategory,
#            ScientificName,
#            Phylum,
#            Class,
#            Order,
#            Family,
#            Genus,
#            Species,
#            TaxonRank,
#            AphiaID) %>%
#   summarize(n=n()) %>%
#   View()
#
#
# sub_enhanced3 %>% View()
#
# filt %>% filter(ScientificName == 'Latrunculia (Latrunculia)') %>% pull(CatalogNumber) %>% length()

##### export result to csv (export to CSV) #####
filename <- "20240726-1_taxonomy_patch_for_DSCRTP_NatDB_20240723-0.csv"
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

filename <- "20240726-0_taxonomy_patch_CatalogNumbers.csv"
write.csv(unique(patch_rbind$CatalogNumber),
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)




