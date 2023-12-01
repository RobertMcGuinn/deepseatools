##### header #####
## Author: Robert McGuinn
## Started on: 20230415
## Purpose: Adding taxonomy to incoming dataset using WoRMS API calls

##### packages #####
library(tidyverse)
library(worrms)
library(openxlsx)
library(taxize)

##### load the taxonomy table from CSV #####
tax <- read.csv("C:/rworking/deepseatools/indata/tax.csv")

##### load dataset of interest ('sub') from local file #####
setwd("C:/rworking/deepseatools/indata")
filename <- "20230804120029_SH-18-12_dscrtp_submission_nearly_final.csv"
sub <- read.csv(filename,
                encoding = "latin9",
                header = TRUE,
                stringsAsFactors = FALSE)

##### make taxonomic changes to incoming (manual: specific to each new dataset) #####
## flag these taxa
sub1 <- sub # %>% filter(ScientificName != 'Vertebrata')

## change these
sub2 <- sub1 %>%
  mutate(ScientificName = str_replace(ScientificName, "Macrouridaev", "Macrouridae"))
#  mutate(ScientificName = str_replace(ScientificName, "Cottunculus sp.", "Cottunculus" )) %>%
#  mutate(ScientificName = str_replace(ScientificName, "Hydrolagus sp.", "Hydrolagus")) %>%
#  mutate(ScientificName = str_replace(ScientificName, "Lophius sp.", "Lophius")) %>%
#  mutate(ScientificName = str_replace(ScientificName, "Luciobrotula sp.", "Luciobrotula")) %>%
#  mutate(ScientificName = str_replace(ScientificName, "Lycodes sp.", "Lycodes")) %>%
#  mutate(ScientificName = str_replace(ScientificName, "Merluccius sp.", "Merluccius")) %>%
#  mutate(ScientificName = str_replace(ScientificName, "Urophycis sp.", "Urophycis"))
 # mutate(ScientificName = str_replace(ScientificName, "Nezumia sp.", "Nezumia")) %>%
 # mutate(ScientificName = str_replace(ScientificName, "Urophycis sp.", "Urophycis"))
#   mutate(ScientificName = str_replace(ScientificName, "Scyliorhinidae egg cases", "Scyliorhinidae")) %>%
#   mutate(ScientificName = str_replace(ScientificName, "Sebastes spp.", "Sebastes")) %>%
#   mutate(ScientificName = str_replace(ScientificName, "Sebastes spp. YOY", "Sebastes")) %>%
#   mutate(ScientificName = str_replace(ScientificName, "Sebastes YOY", "Sebastes")) %>%
#   mutate(ScientificName = str_replace(ScientificName, "Sebastolobus spp.", "Sebastolobus")) %>%
#   mutate(ScientificName = str_replace(ScientificName, "unidentified sebastomus", "Sebastes (Sebastosomus)"))


## flag 'Vertebrata'

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

##### check #####
# View(summary)

##### check: test for difficult taxa #####
summary$sametest <- ifelse(summary$canonicalname == summary$valid_name,"Yes","No")
changes <- summary %>% filter(sametest == "No") %>% pull(scientificname)
nomatch <- summary %>% filter(is.na(sametest) == T) %>% pull(scientificname)

changes
nomatch

##### create vector from valid AphiaIDs #####
summary <- summary %>% filter(is.na(valid_AphiaID) == F)
my_vector <- summary$valid_AphiaID

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

##### left join the summary from above with all of the other API tables #####
by <- join_by(valid_AphiaID == AphiaID)
joined2 <- left_join(summary, classification, by)

by <- join_by(valid_AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

by <- join_by(valid_AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

##### check #####
# names(joined4)

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
























