##### Header #####
## Author: Robert McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## Started On: 20230905
## Purpose: To check the full taxonomic contents of the national database against the WoRMS API

##### packages #####
library(tidyverse)
library(worrms)
library(openxlsx)
library(taxize)

##### load NDB from local file (manual)#####
setwd("C:/rworking/deepseatools/indata")
filename <- "DSCRTP_NatDB_20230928-0.csv"
indata <- read.csv(filename,
                   encoding = "latin9",
                   header = TRUE,
                   stringsAsFactors = FALSE)
filt <- indata %>%
  filter(Flag == 0)

rm(indata)
rm(filename)

##### check #####
filt %>% filter(grepl('cf.', ScientificName)) %>% pull(ScientificName) %>% unique()

##### create vector from valid AphiaIDs #####
my_vector <- unique(filt$AphiaID)
# my_vector <- my_vector[0:100]

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### check #####
table(filt$AphiaID, useNA = 'always')

##### loop to get records by AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list <- df

##### check #####
View(species_list)
table(species_list$status, useNA = 'always')

x <- species_list %>%
  filter(status == 'unaccepted') %>% group_by(scientificname, valid_name) %>%
  summarize(n=n())
View(x)

setdiff(species_list$AphiaID, species_list$valid_AphiaID)
setdiff(species_list$valid_AphiaID, species_list$AphiaID)



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
View(vernaculars)

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

##### save R objects and bring back in for later use (being friendly to the WoRMS server folks) #####
## make sure to add version to the front of the filename
saveRDS(species_list, file = "C:/rworking/deepseatools/indata/taxonomy_objects/20230918-0_species_list.rds")
saveRDS(classification, file = "C:/rworking/deepseatools/indata/taxonomy_objects/20230918-0_classification.rds")
saveRDS(vernaculars, file = "C:/rworking/deepseatools/indata/taxonomy_objects/20230918-0_vernaculars.rds")
saveRDS(synonyms, file = "C:/rworking/deepseatools/indata/taxonomy_objects/20230918-0_synonyms.rds")


## read them all back in when you need them (don't do this unless you know version #)
species_list <- readRDS(file ="C:/rworking/deepseatools/indata/taxonomy_objects/20230918-0_species_list.rds")
classification <- readRDS(file ="C:/rworking/deepseatools/indata/taxonomy_objects/20230918-0_classification.rds")
vernaculars <- readRDS(file ="C:/rworking/deepseatools/indata/taxonomy_objects/20230918-0_vernaculars.rds")
synonyms <- readRDS(file ="C:/rworking/deepseatools/indata/taxonomy_objects/20230918-0_synonyms.rds")

## cleanup
objects <- c("species_list", "classification", "vernaculars", "synonyms")
rm(list = objects)

##### left join the summary from above with all of the other API tables #####
by <- join_by(valid_AphiaID == AphiaID)
joined2 <- left_join(species_list, classification, by)

by <- join_by(valid_AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

by <- join_by(valid_AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

filt_tax <- filt %>%
  group_by(AphiaID) %>%
  summarize(ScientificName_DSCRTP = paste(unique(ScientificName), collapse = ' | '),
            VerbatimScientificName_DSCRTP = paste(unique(VerbatimScientificName), collapse = ' | '))

by <- join_by(AphiaID == AphiaID)
master_worms <- left_join(filt_tax, joined4, by)

##### save or reload the master_worms file #####
saveRDS(master_worms, file = "c:/rworking/deepseatools/indata/taxonomy_objects/20230928-0_master_worms.rds")
master_worms <- readRDS(file ="C:/rworking/deepseatools/indata/taxonomy_objects/20230928-0_master_worms.rds")

##### check #####

names(master_worms)
unique(filt$DatabaseVersion)

filt %>%
  filter(ScientificName == 'Staurocalyptus pamelaturnerae') %>%
  group_by(AphiaID, ScientificName) %>%
  summarize(n=n())

master_worms <- master_worms %>% mutate(ScientificName_DSCRTP =
                    ifelse(ScientificName_DSCRTP == "Muriceides k\xfckenthali",
                           'Muriceides kuekenthali',
                           as.character(ScientificName_DSCRTP)))
yo <- master_worms %>%
  filter(grepl(" \\| ", ScientificName_DSCRTP)) %>%
  group_by(AphiaID, ScientificName_DSCRTP, scientificname, valid_AphiaID, valid_name, status) %>%
  summarize(n=n())
View(yo)

yo <- master_worms %>%
  filter(scientificname != ScientificName_DSCRTP) %>%
  group_by(AphiaID, ScientificName_DSCRTP, scientificname, valid_AphiaID, valid_name, status) %>%
  summarize(n=n())
View(yo)

yo <- master_worms %>%
  filter(AphiaID != valid_AphiaID) %>%
  group_by(AphiaID, VerbatimScientificName_DSCRTP, ScientificName_DSCRTP, scientificname, valid_AphiaID, valid_name, status, genus) %>%
  summarize(n=n())
View(yo)

filt$AphiaID
joined4$AphiaID
filt %>% filter(is.na(AphiaID) == T)
table(filt$AphiaID, useNA = 'always')

# names(joined4)
table(joined4$status, useNA = 'always')
notaccepted <- joined4 %>% filter(status != 'accepted') %>%
  group_by(AphiaID, valid_AphiaID, scientificname, valid_name, unacceptreason) %>%
  summarize(n=n())
View(notaccepted)

##### export master_worms
write_csv(master_worms, "c:/rworking/deepseatools/indata/20230927-0_master_worms.csv")



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
    ScientificName %in% stonycoralcupcoral ~ 'stony coral (branching)',
    TRUE ~ ''))

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

##### export result to csv (export to CSV) #####
filename <- "20221021-0_NOAA_HB1504_TowCam_Fishes_MRhode_taxonomy_patch.csv"
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))
























