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
filename <- "DSCRTP_NatDB_20221213-0.csv"
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
tax_fl <- read.csv("C:/rworking/deepseatools/indata/tax_fl.csv")
tax_ch <- read.csv("C:/rworking/deepseatools/indata/tax_ch.csv")

##### sub is loaded from another code #####
##### make any needed taxonomic changes to incoming #####
sub2 <- sub %>%
  mutate(ScientificName = str_replace(ScientificName, "Eptatretus spp.", "Eptatretus")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Glytocephalus zachirus", "Glyptocephalus zachirus")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Icelinus spp.", "Icelinus")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Raja spp.", "Raja")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Rajiformes egg cases","Rajiformes")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Rathbunella spp.", "Rathbunella")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Scyliorhinidae egg cases", "Scyliorhinidae")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Sebastes spp.", "Sebastes")) %>%
  mutate(ScientificName = str_replace(ScientificName, "Sebastolobus spp.", "Sebastolobus"))

##### create vector of names #####
my_vector <- unique(sub2$ScientificName)

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
                                   marine_only = T
                                   )
  species_list <- do.call("rbind", species_list)
  df <- rbind(df, species_list)
}
species_list <- df

##### check #####
dim(species_list)
# View(species_list)

##### left join the parsed list #####
by <- join_by(canonicalname == scientificname)
joined <- left_join(parsed_list, species_list, by)

##### check the joined file #####
summary <- joined %>% group_by(status,
                               phylum,
                               scientificname,
                               canonicalname,
                               valid_name,
                               valid_AphiaID) %>%
  summarize(n=n())
View(summary)

##### test the difficult taxa #####
summary$sametest <- ifelse(summary$canonicalname == summary$valid_name,"Yes","No")
changes <- summary %>% filter(sametest == "No") %>% pull(scientificname)
nomatch <- summary %>% filter(is.na(sametest) == T) %>% pull(scientificname)

changes
nomatch

##### match your valid AphiaID #####
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
  vernaculars <- wm_common_id(i))
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
    synonyms <- wm_synonyms(i))
  synonyms_list <- paste(synonyms$scientificname, collapse=" | ")
  AphiaID <- i
  synonyms_wide <- data.frame(AphiaID, synonyms_list)
  df <- bind_rows(df, synonyms_wide)
}

synonyms <- df

##### check #####
View(classification)
View(vernaculars)
View(classification)
View(synonyms)

dim(species_list)
dim(classification)
dim(vernaculars)
dim(synonyms)

head(species_list$AphiaID)
head(classification$AphiaID)
head(vernaculars$AphiaID)
head(synonyms$AphiaID)

tail(tax_tom_enhanced$AphiaID)
tail(classification$AphiaID)
tail(vernaculars$AphiaID)
tail(synonyms$AphiaID)

class(tax_tom_enhanced$AphiaID)
class(classification$AphiaID)
class(vernaculars$AphiaID)
class(synonyms$AphiaID)

names(tax_tom_enhanced2)

tax_tom_enhanced2 %>%
  dplyr::select(AphiaID_x, Species_edit, status, valid_name) %>%
  View()

##### cbind vernaculars, classification, synonyms to Tom's list #####
classification2 <- classification %>% select(-AphiaID)
synonyms2 <- synonyms %>% select(-AphiaID)
vernaculars2 <- vernaculars %>% select(-AphiaID)

tax_tom_enhanced2 <- cbind(tax_tom_enhanced,
                           classification2
)


tax_tom_enhanced2 <- cbind(tax_tom_enhanced2,
                           synonyms2
)

tax_tom_enhanced2 <- cbind(tax_tom_enhanced2,
                           vernaculars2
)

##### check the file #####
dim(tax_tom_enhanced2)

##### deliver the goods (export to CSV) #####
write.xlsx(tax_tom_enhanced2, "c:/rworking/deepseatools/indata/20230317-0_master_taxonomy_checker_THourigan_RPMcGuinn.xlsx", fileEncoding = "UTF-8")

























##### check #####
setdiff("Actinernus nobilis", tax$ScientificName)
setdiff("Actinernus", tax$ScientificName)
setdiff("Actinernus", tax_fl$ScientificName)
setdiff("Actinernus nobilis", tax_fl$ScientificName)

sub %>% filter(ScientificName == "Actinernus" |
                 ScientificName == "Actinernus nobilis") %>%
  pull(CatalogNumber) %>%
  length()

sub %>% filter(ScientificName == "Actinernus" |
                 ScientificName == "Actinernus nobilis") %>%
  pull(Flag) %>%
  table()

sub %>% filter(ScientificName == "Actinernus" |
                 ScientificName == "Actinernus nobilis") %>%
  pull(Family) %>%
  table()

## Actinoscyphia Actinoscyphiidae
setdiff("Actinoscyphiidae", tax_fl$ScientificName)

s %>% filter(FieldName == 'VernacularNameCategory') %>%
  pull(ValidValues)





