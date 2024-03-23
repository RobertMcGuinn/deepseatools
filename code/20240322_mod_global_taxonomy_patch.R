##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240103
## purpose: global taxonomy patch

##### linkage #####
## manual input here
filename <- '20240110-0_global_taxonomy_patch_NDB_20230828-0_125104.R' ## entry: name of the current file.  make sure to include '.R'
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)
## google drive: https://drive.google.com/drive/folders/1OQPfeFOBLLKhDe6RU4UzqSdxxm6n7bqY?usp=drive_link

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(worrms)
library(openxlsx)
library(taxize)

##### check #####
## find where AphiaID are missing
# filt %>% filter(AphiaID == -999) %>%
#   pull(ScientificName) %>%
#   unique()

##### add a few missing AphiaIDs to the NDB ######
filt_fixed <- filt %>%
  mutate(AphiaID = ifelse(ScientificName == "Bathypathes pseudoalternata",
                          1578714,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Parantipathes pluma",
                          1521953,
                         AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Kophobelemnon biflorum",
                          1391784,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Lithophytum roseum",
                          1392576,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Iridogorgia densispicula",
                          1635628,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Iridogorgia squarrosa",
                          1635629,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Sibogagorgia californica",
                          1647390,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Aphanostichopathes",
                          1514479,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Callogorgia cracentis",
                          1317063,
                          AphiaID))

## The following taxa remain without AphiaID matches in worms as of 20240104
# -999 # [2] "Leptogorgia porrecta"
# -999 # [4] "Pseudothesea pailoloensis"
# -999 # [5] "Tribrachium symmetra"
# -999 # [6] "Atlantisella alenuihaha"
# -999 # [7] "Lepidisis cornucopia"
# -999 # [10] "Chrysogorgia arbuscula"
# -999 # [11] "Halisarca membrana"
# -999 # [12] "Neopelta aberrans"
# -999 # [13] "Acromuricea hirtella"
# -999 # [14] "Acromuricea alatispina"

##### check #####
# filt_fixed %>% filter(AphiaID == -999) %>% pull(CatalogNumber)

##### deal with duplicates in filt_fixed (breaks join operations otherwise) #####
filt_fixed <- filt_fixed %>%
  mutate(AphiaID = ifelse(AphiaID == 602367, 125286, AphiaID)) %>%
  mutate(AphiaID = ifelse(AphiaID == 246100, 170653, AphiaID)) %>%
  mutate(AphiaID = ifelse(AphiaID == 286696, 1393629, AphiaID))

# 602367 125286 # Clavularia
# 246100 170653 # Polymastia pacifica
# 286696 1393629 # Stylatula gracilis

##### load the taxonomy table from CSV #####
tax <- read.csv("C:/rworking/deepseatools/indata/tax.csv")
##### cleanup #####
##### check #####
# filt_fixed %>% filter(AphiaID == -999) %>%
#   pull(ScientificName) %>%
#   unique()
#
# filt_fixed %>%
#   filter(ScientificName == 'Callogorgia cracentis') %>%
#   pull(AphiaID)

##### ***** #####
##### grab all AphiaIDs #####
aphiaIDs <- unique(filt_fixed$AphiaID)

##### check #####
# x <- filt_fixed %>%
#   filter(AphiaID < 1) %>%
#   pull(ScientificName) %>%
#   unique()
# x
#
# length(aphiaIDs)
# summary(aphiaIDs)
# table(aphiaIDs < 1)

##### create vector from incoming AphiaIDs #####
my_vector <- aphiaIDs

##### check #####
# length(my_vector)

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
# ## view any duplicated entries in the table
# yo <- species_list_original %>% filter(duplicated(scientificname) == T) %>%
#   pull(scientificname)
# yo <- na.omit(yo)
#
# species_list_original %>% filter(scientificname %in% yo) %>%
#   group_by(scientificname, AphiaID, valid_AphiaID, valid_name, isExtinct, isMarine) %>%
#   summarize(n=n()) %>%
#   View()
#
# species_list_original %>%
#   filter(AphiaID == '-999') %>%
#   View()

##### create a complete valid AphiaID list #####
species_list_original <- species_list_original %>%
  mutate(valid_AphiaID_complete = ifelse(is.na(valid_AphiaID) == T,
                                         AphiaID,
                                         valid_AphiaID))

##### check #####
# length(aphiaIDs)
#
# length(species_list_original$AphiaID)
#
# species_list_original %>% filter(status != 'accepted') %>%
#   group_by(AphiaID, valid_AphiaID, valid_AphiaID_complete) %>%
#   summarize(n=n()) %>% View()
#
# table(is.na(species_list_original$valid_AphiaID_complete))


##### ***** #####
##### create vector from valid AphiaIDs #####
my_vector <- unique(species_list_original$valid_AphiaID_complete)

## get rid of any NA values.
my_vector <- na.omit(my_vector)

##### check #####
# table(is.na(my_vector))
# View(my_vector)
# summary(my_vector)

##### make groups of 50 (because the API limit is 50) #####
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by the valid AphiaID #####
# species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
# df <- species_list[0,]
#
# for (i in seq_along(my_groups)){
#   species_list <- wm_record(my_groups[[i]])
#   df <- rbind(df, species_list)
# }
# species_list <- df
#
# ## get rid of any NA rows
# species_list <-
#   species_list %>% filter(is.na(AphiaID) == F)

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
wm_common_id(135161)
wm_common_id(1245747)
wm_common_id(395098)
wm_common_id(1116761)

filt_fixed %>% filter(VernacularName == 'snowflake coral') %>%
  pull(ScientificName)

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

##### save and load objects for future use #####
## save multiple objects
# save(species_list_original,
#      species_list,
#      classification,
#      vernaculars,
#      synonyms,
#      file = 'c:/rworking/deepseatools/indata/taxonomy_objects.Rdata')

## clean up everything except filt
# rm(list=setdiff(ls(), c("filt")))

# rm(species_list_original,
#    species_list,
#    classification,
#    vernaculars,synonyms)

## load the objects back
load('c:/rworking/deepseatools/indata/taxonomy_objects.Rdata')

##### ***** #####
##### left join the species list from above with all of the other API tables #####
## joining species_list, classification, vernaculars, and synonyms
by <- join_by(AphiaID == AphiaID)
joined2 <- left_join(species_list, classification, by)

by <- join_by(AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

by <- join_by(AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

##### check #####
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

joined4 %>% names()

##### join original table with the new table #####
joined4$AphiaID2 <- joined4$AphiaID
by <- join_by(valid_AphiaID_complete == AphiaID2)
taxonomy_table <- left_join(species_list_original, joined4, by)
# View(taxonomy_table)
# names(taxonomy_table)

##### add taxonomy to filt #####
by <- join_by(AphiaID == AphiaID.x)
sub_enhanced <- left_join(filt_fixed, taxonomy_table, by)

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
# sub_enhanced %>% filter(is.na(phylum.y) == T) %>%
#   pull(ScientificName) %>%
#   unique()
# #
# dim(filt)
# dim(filt_fixed)
# dim(sub_enhanced)

##### gather information into proper variables #####
sub_enhanced$IdentificationComments <-
  paste('DSCRTP_og_scientificname:',
        ' ',
        sub_enhanced$ScientificName,
        ' | ',
        sub_enhanced$IdentificationComments,
        sep = '')
# sub_enhanced$VerbatimScientificName <- filt$VerbatimScientificName
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
# head(sub_enhanced$IdentificationComments)
#
# table(sub_enhanced$Phylum, useNA = 'always')
#
# x <- sub_enhanced %>% filter(is.na(sub_enhanced$phylum.y) == T) %>%
#   group_by(ScientificName, status.x, status.y, AphiaID) %>%
#   summarize(n=n())
# View(x)
#
# filt %>% filter(CatalogNumber == 249815) %>% pull(AphiaID)
#
#
# table(x$Phylum, useNA = 'always')
# table(sub_enhanced$Class, useNA = 'always')
# sub_enhanced_filter %>% filter(Class == 'Hydrozoa') %>%
#   group_by(Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()
#
# length(sub_enhanced$CatalogNumber)
# length(filt$CatalogNumber)
# setdiff(filt$CatalogNumber, sub_enhanced$CatalogNumber)
# setdiff(sub_enhanced$CatalogNumber, filt$CatalogNumber)


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

##### check #####
# dim(sub_enhanced) - dim(sub_enhanced_filter)
# setdiff(sub_enhanced$ScientificName, sub_enhanced_filter$ScientificName)

## check on records that were excluded by the filtering criteria
# excluded_cats <- setdiff(sub_enhanced$CatalogNumber, sub_enhanced_filter$CatalogNumber)
#
# sub_enhanced %>% filter(CatalogNumber %in% excluded_cats) %>%
#   group_by(status.x, status.y, VerbatimScientificName, TaxonRank, ScientificName, AphiaID, AphiaID.y, Phylum, Class, Order, Family, Genus) %>%
#   summarize(n=n()) %>% View

## *****
sub_enhanced %>% filter(ScientificName == 'Distichoptilum rigidum') %>%
  group_by(Genus, Genus.y, genus.x, genus.y, ScientificName, scientificname.x, scientificname.y) %>%
  summarize(n=n()) %>% View()

sub_enhanced %>% filter(ScientificName == 'Octocorallia') %>%
  group_by(ScientificName, AphiaID, VernacularNameCategory) %>%
  summarize(n=n()) %>% View

dim(species_list_original)
summary(species_list_original)

filt %>% filter(ScientificName == "Clavularia") %>%
  group_by(ScientificName, VerbatimScientificName, AphiaID) %>%
  summarize(n=n())

table(sub_enhanced_filter$Phylum, useNA = 'always')
table(sub_enhanced_filter$Subphylum, useNA = 'always')

sub_enhanced_filter %>%
  group_by(VernacularName, AphiaID, Phylum, Subphylum, Class, Order, Family, Genus, Species) %>%
  summarize(n=n()) %>% View()

filt %>%
  filter(Family == 'Corallimorphidae') %>%
  group_by(VernacularName, AphiaID, ScientificName) %>%
  summarize(n=n()) %>% View()

sub_enhanced_filter %>% filter(Phylum == 'Cnidaria') %>%
  group_by(ScientificName, VernacularName) %>%
  summarize(n=n()) %>% View()

sub_enhanced_filter %>% filter(Phylum == 'Porifera') %>%
  group_by(ScientificName, VernacularName) %>%
  summarize(n=n()) %>% View()

sub_enhanced_filter %>% filter(Phylum == 'Chordata') %>%
  group_by(ScientificName, VernacularName) %>%
  summarize(n=n()) %>% View()

##### ***** #####
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

sub2 <- sub %>%
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
    Genus %in% c('Aspera', 'Verseveldtia') ~ 'soft coral',
    Order %in% c('Malacalcyonacea') ~ 'soft coral',
    Order %in% c('Anthoathecata') &
      Family %notin%  c('Solanderiidae') ~ 'lace coral',
    Family %in% c('Lithotelestidae', 'Aulopsammiidae') ~ 'lithotelestid coral',
    Family %in% othercorallikehydrozoanfamilies ~ 'other coral-like hydrozoan',
    ScientificName %in% c('Pennatuloidea') ~ 'sea pen',
    ScientificName %in% c('Porifera') ~ 'sponge',
    Suborder %in% c('Stolonifera') ~ 'stoloniferan coral',
    Family %in% c('Clavulariidae') ~ 'stoloniferan coral',
    Genus %in% c('Clavularia', 'Sarcodictyon', 'Scleranthelia', 'Pseudocladochonus') ~ 'stoloniferan coral',
    Order %in% c('Scleractinia') &
      TaxonRank %in% c('Order')  ~ 'stony coral (unspecified)',
    ScientificName %in% stonycoralbranching ~ 'stony coral (branching)',
    ScientificName %in% c(stonycoralcupcoral, 'Desmophyllum hourigani') ~ 'stony coral (cup coral)',
    Genus %in% c('Acanthogorgia', 'Hypnogorgia', 'Thelogorgia',
                 'Stephanogorgia', 'Helicogorgia', 'Distichogorgia',
                 'Xenogorgia', 'Caliacis', 'Briareopsis', 'Elasmogorgia',
                 'Pseudothesea', 'Bayergorgia', 'Flagelligorgia') ~ 'gorgonian coral',
    Genus %in% c('Hydrodendron') ~ 'other coral-like hydrozoan',
    Genus %in% c('Caryophyllia', 'Paracyathus', 'Asterosmilia',
                 'Heteropsammia') ~ 'stony coral (cup coral)',
    Family %in% c('Micrabaciidae', 'Flabellidae', 'Turbinoliidae', 'Astrangiidae',
                  'Rhizangiidae', 'Anthemiphylliidae') ~ 'stony coral (cup coral)',
    ScientificName %in% c('Caryophylliidae') ~ 'stony coral (unspecified)',
    Genus %in% c('Telestula') ~ 'stoloniferan coral',
    ScientificName %in% c('Dendrophylliidae') ~ 'stony coral (unspecified)',
    Genus %in% c('Leptoseris', 'Dactylotrochus',
                 'Anomocora', 'Paraconotrochus',
                 'Trochocyathus', 'Cladopsammia',
                 'Balanophyllia', 'Deltocyathus', 'Tubastraea') ~ 'stony coral (cup coral)',
    Genus %in% c('Madracis', 'Solenosmilia') ~ 'stony coral (branching)',
    ScientificName %in% c('Pocilloporidae') ~ 'stony coral (unspecified)',
    ScientificName %in% c('Octocorallia', 'Octocorallia incertae sedis', 'Scleralcyonacea') ~ 'insufficient taxonomic resolution',
    Family %in% c('Funiculinidae', 'Virgulariidae', 'Umbellulidae', 'Protoptilidae') ~ 'sea pen',
    ScientificName %in% c('Verseveldtia granulosa') ~ 'soft coral',
    Genus %in% c('Rhizopsammia', 'Notophyllia', 'Vaughanella', 'Thalamophyllia', 'Tethocyathus', 'Coenocyathus') ~ 'stony coral (cup coral)',
    TRUE ~ ''))

##### check #####
# table(sub_enhanced2$VernacularNameCategory, useNA = 'always')
# sub_enhanced2 %>%
#   filter(VernacularNameCategory == '') %>%
#   group_by(AphiaID, Class, Order, Family, Genus, ScientificName) %>%
#   summarize(n=n()) %>% View()
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
#
#
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

##### check #####
x <- sub_enhanced3 %>% filter( grepl('Cirrhipathes', ScientificName)) %>%
    group_by(CatalogNumber, ScientificName, VerbatimScientificName, IdentificationComments, AphiaID, Phylum,
             Class, Order, Suborder,
             Family, Genus, Species) %>%
    summarize(n=n()) %>% pull(CatalogNumber)

y <- filt_fixed %>% filter(CatalogNumber %in% x,
                      FishCouncilRegion == 'Gulf of Mexico') %>%
  pull(DatasetID)

sub_enhanced3 %>% filter(CatalogNumber %in% y) %>%
  group_by(CatalogNumber, ScientificName, VerbatimScientificName, IdentificationComments, AphiaID, Phylum,
           Class, Order, Suborder,
           Family, Genus, Species) %>%
  summarize(n=n()) %>% View()







# dim(sub_enhanced3)
# dim(filt)
# length(sub$CatalogNumber) - length(sub_enhanced3$CatalogNumber)
#
# x <- setdiff(filt_fixed$CatalogNumber, sub_enhanced2$CatalogNumber)
# sub_enhanced %>% filter(CatalogNumber %in% x) %>%
#   group_by(CatalogNumber, AphiaID, Phylum, Class, Order, Suborder, Family, Genus, Species) %>%
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

##### export result to csv (export to CSV) #####
filename <- "20240110-0_global_taxonomy_patch_NDB_20230828-0_125104.csv"
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))





