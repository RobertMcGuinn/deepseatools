##### header #####
## Author: Robert McGuinn
## Started on: 20230415
## Purpose: Adding taxonomy to incoming dataset using WoRMS API calls

##### packages #####
library(tidyverse)
library(worrms)
library(openxlsx)
library(taxize)

##### load NDB from local file (manual)#####
setwd("C:/rworking/deepseatools/indata")
filename <- "DSCRTP_NatDB_20230620-0.csv"
indata<-read_csv(filename,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = 'latin9'),
                 na = c("-999", "NA"))

filt <- indata %>%
  filter(Flag == 0)

rm(indata)
rm(filename)

##### load the taxonomy tables from CSV #####
tax <- read.csv("C:/rworking/deepseatools/indata/tax.csv")

##### load dataset of interest ('sub') from local file #####
setwd("C:/rworking/deepseatools/indata")
filename <- "20230718-0_NOAA_HB1703_ROPOS_Fishes_MRhode.csv"
sub <- read_csv(filename,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = 'latin9'))

##### make any needed taxonomic changes to incoming (specific to each new dataset) #####
sub2 <- sub %>%
  mutate(ScientificName = str_replace(ScientificName, "Anarchichas minor", "Anarhichas minor")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Corhyphaenoides", "Coryphaenoides" )) %>%
  mutate(ScientificName = str_replace(ScientificName, "Corhyphaenoides ruprestris", "Coryphaenoides rupestris")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Macrourine", "Macrourinae")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Notacanthus chementzii", "Notacanthus chemnitzii")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Selachimorpha", "Elasmobranchii")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Coryphaenoides ruprestris", "Coryphaenoides rupestris")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Arctozenus rissoi", "Arctozenus risso"))

##### create vector of names #####
my_vector <- unique(sub2$ScientificName)
my_vector <- my_vector[complete.cases(my_vector)]

##### parse the list using taxize function 'gbif_parse' #####
parsed_list <- gbif_parse(my_vector)

# View(parsed_list)
my_vector_parsed <- parsed_list$canonicalname

##### make groups of 50 (because the API limit is 50) #####
my_groups <- split(my_vector_parsed, ceiling(seq_along(my_vector)/50))

##### loop to get records by names list #####
## run this just once to get the proper data structure for an empty dataframe
species_list <- wm_records_name("Haliclona", fuzzy = F)

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

##### **check #####
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

##### **check #####
View(summary)

##### **check: test for difficult taxa #####
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
df <- data.frame(
  AphiaID = numeric(),
  vernaculars_list = character()
)

for (i in my_vector){
  try(
  vernaculars <- wm_common_id(i)) # i <- 125588
  vernaculars <- vernaculars %>% filter(language == 'English')
  vernaculars_list <- paste(vernaculars$vernacular, collapse=" | ")
  AphiaID <- i
  vernaculars_wide <- data.frame(AphiaID, vernaculars_list)
  df <- bind_rows(df, vernaculars_wide)
}

vernaculars <- df

##### loop to get synonyms #####
df <- data.frame(
  AphiaID = numeric(),
  synonyms_list = character()
)

for (i in my_vector){
  try(
    synonyms <- wm_synonyms(i)) # i <- 274788
  synonyms_list <- paste(synonyms$scientificname, collapse=" | ")
  AphiaID <- i
  synonyms_wide <- data.frame(AphiaID, synonyms_list)
  df <- bind_rows(df, synonyms_wide)
}

synonyms <- df

##### **check #####
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

##### **check #####
# names(joined4)

##### add taxonomy to sub #####
by <- join_by(ScientificName == scientificname)
sub_enhanced <- left_join(sub2, joined4, by)

##### check #####
# sub_enhanced %>% filter(is.na(phylum) == T) %>%
#   pull(ScientificName) %>%
#   unique()

##### gather information into proper variables #####
sub_enhanced$VerbatimScientificName <- sub$ScientificName
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
                  "Victorgorgiidae", "Keratoisididae")

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

##### **check #####
# table(sub_enhanced2$VernacularNameCategory, useNA = 'always')
#
sub_enhanced2 %>%
  filter(VernacularNameCategory == '') %>%
  select(VernacularNameCategory, VerbatimScientificName, ScientificName, Phylum, Class, Order) %>%
  View()
#
# sub_enhanced2 %>%
#   group_by(VernacularNameCategory,
#          VerbatimScientificName,
#          ScientificName,
#          Phylum,
#          Class,
#          Subclass,
#          Order,
#          Suborder,
#          Family,
#          Subfamily,
#          Genus,
#          Species) %>%
#   summarize(n=n()) %>%
#   View()

##### **check #####
table(sub_enhanced2$Phylum)
sub_enhanced2 %>% filter(VerbatimScientificName == "Selachimorpha") %>%
  group_by(VerbatimScientificName, ScientificName) %>%
  summarize(n=n())
View(sub_enhanced2)

##### get rid of everything not Chordata, Cnidaria, and Porifera #####
sub_enhanced2 <- sub_enhanced2 %>%
  filter(Phylum == 'Chordata' |
           Phylum == 'Cnidaria' |
           Phylum == 'Porifera')

##### export result to csv (export to CSV) #####
filename <- '20230719-0_NOAA_HB1703_ROPOS_Fishes_MRhode.csv'
write_csv(sub_enhanced2,
          paste("c:/rworking/deepseatools/indata/",
                filename))



























