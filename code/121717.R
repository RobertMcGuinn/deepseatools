##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20231129
## purpose: see redmine issue

##### linkage #####
## manual input here
filename <- '121717' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, ".R", sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
## manual input here
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(worrms)
library(openxlsx)
library(taxize)

##### load data #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv('20231128-3_NewRecords-NMNH-Q1-2024_THourigan.csv')

##### make any corrections #####
sub$AphiaID <- replace(sub$AphiaID, sub$AphiaID == 286809, 286810)
# sub$AphiaID <- replace(sub$AphiaID, sub$AphiaID == 125274, 520681)

286809 %in% sub$AphiaID
286810 %in% sub$AphiaID

##### load the taxonomy table from CSV #####
tax <- read.csv("C:/rworking/deepseatools/indata/tax.csv")

##### create vector from incoming AphiaIDs #####
my_vector <- unique(sub$AphiaID)

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
setdiff(species_list_original$AphiaID, sub$AphiaID)
setdiff(sub$AphiaID, species_list_original$AphiaID)
View(species_list_original)
table(species_list$status)
species_list_original %>% filter(status != 'accepted') %>% View()

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

###### join original table with the new table #####
joined4$AphiaID2 <- joined4$AphiaID
by <- join_by(valid_AphiaID_complete == AphiaID2)
taxonomy_table <- left_join(species_list_original, joined4, by)
# View(taxonomy_table)
# names(taxonomy_table)

##### add taxonomy to sub #####
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

sub_enhanced2 <- sub_enhanced %>%
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
    Order %in% c('Scleractinia') &
      TaxonRank %in% c('Order')  ~ 'stony coral (unspecified)',
    ScientificName %in% stonycoralbranching ~ 'stony coral (branching)',
    ScientificName %in% stonycoralcupcoral ~ 'stony coral (cup)',
    Genus %in% c('Acanthogorgia') ~ 'gorgonian coral',
    TRUE ~ ''))

##### check #####
table(sub_enhanced2$VernacularNameCategory, useNA = 'always')

sub_enhanced2 %>%
  filter(VernacularNameCategory == 'stony coral (cup)') %>%
  pull(ScientificName) %>% unique()

sub_enhanced2 %>%
  filter(VernacularNameCategory == 'stony coral (cup)') %>%
  pull(VernacularNameCategory) %>% unique()

##### get rid of unneeded column names #####
names_list <- names(sub)
sub_enhanced2 <- sub_enhanced2 %>%
  dplyr::select(all_of(names_list))

##### get rid of everything not Chordata, Cnidaria, and Porifera #####
sub_enhanced2 <- sub_enhanced2 %>%
  filter(Phylum == 'Chordata' |
           Phylum == 'Cnidaria' |
           Phylum == 'Porifera')

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
         Synonyms)

##### check #####
View(sub_enhanced3)
dim(sub_enhanced3)
sub %>% filter(ScientificName == 'Dichotella gemmacea') %>% pull(AphiaID)
'Dichotella gemmacea'

x <- setdiff(sub_enhanced3$VerbatimScientificName, sub_enhanced3$ScientificName)
sub_enhanced3 %>% filter(VerbatimScientificName %in% x) %>%
  group_by(VerbatimScientificName, ScientificName) %>%
  summarize(n=n()) %>% View()

x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
sub %>% filter(CatalogNumber %in% x) %>% pull(AphiaID)

table(sub_enhanced3$VernacularNameCategory, useNA = 'always')

##### export result to csv (export to CSV) #####
filename <- "20231128-3_NMNH-IZ_THourigan_1890_2022_121717_taxonomy_patch.csv"
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))






























##### *** NEW VERSION: 20231204-1 (post-checker) #####
##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(worrms)
library(openxlsx)
library(taxize)

##### load data #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv('20231204-1_NMNH-IZ_THourigan_1890_2022_121717.csv')

##### check #####
table(sub$VernacularNameCategory, useNA = 'always')

sub %>%
  filter(is.na(VernacularNameCategory) == T) %>%
  group_by(ScientificName, Phylum, Class, Order, Family, Genus, VernacularNameCategory) %>%
  summarize(n=n()) %>% View

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
sub2 %>% pull(VernacularNameCategory) %>%
  table(useNA = 'always')

sub2 %>%
  group_by(Phylum, Class, Order, Family, Genus, ScientificName, VernacularNameCategory) %>%
  summarize(n=n()) %>%
  View()

filt %>%
  filter(Family == 'Caryophylliidae') %>%
  pull(VernacularNameCategory) %>% unique()

filt %>%
  filter(Genus == 'Coenocyathus') %>%
  pull(VernacularNameCategory) %>% unique()

##### produce a patch for VernacularNameCategory #####
x <- sub2 %>% select(CatalogNumber, VernacularNameCategory)
x %>% write.csv('c:/rworking/deepseatools/indata/20240321_VernacularNameCategory_patch_RPMcGuinn.csv')

##### check #####
sub %>%
  filter(CatalogNumber %in% c(1303120, 1303121)) %>%
  group_by(CatalogNumber, SampleID, Phylum, Class, Order, Family, Genus, ScientificName) %>%
  summarize(n=n())

##### map check #####
sub %>%
  filter(SamplingEquipment == 'slurp') %>%
  group_by(Vessel, VehicleName, ObservationDate, SamplingEquipment) %>%
  summarize(n=n())

sub %>%
  filter(SamplingEquipment == 'sediment catcher') %>%
  group_by(Vessel, VehicleName, ObservationDate, SamplingEquipment) %>%
  summarize(n=n())


points <- st_as_sf(sub3, coords = c("Longitude", "Latitude"), crs = 4326)

st_write(points,
         "C:/rworking/deepseatools/indata/geo3.shp",
         append = F)

sub3 %>%
  group_by(Phylum, Class, Order, Family, Genus, ScientificName) %>%
  summarize(n=n()) %>% View()

filt %>% filter(EventID == 'JSL-II-2161') %>%
  group_by(Phylum, Class, Order, Family, Genus, ScientificName, ObservationDate) %>%
  summarize(n=n()) %>% View()









