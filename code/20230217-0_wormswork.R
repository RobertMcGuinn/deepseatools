##### header #####
## Author: Robert McGuinn
## Started on: 20230217
## Purpose: WoRMS API calls.

##### packages #####
library(tidyverse)
library(worrms)
# install.packages("openxlsx")
library(openxlsx)

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

##### load the Tom Hourigan list #####
## copy the path from clipboard, shift+rightclick_copy as path in windows file explorer.
## copy this path to clipboard: "C:\rworking\deepseatools\indata\20230314-0_2022-ComprehensiveList-11-8-2022_THourigan.csv"

path <- scan("clipboard",what="string")
## conform path
path <- gsub("\\\\", "/", path)
## load csv (make sure it is UTF-8 encoded when saving from excel)
tax_tom <- read_csv(path)

## conform the column names
tax_tom <- tax_tom %>%
  rename(#Caribbean = "U.S. Caribbean",
    Pacific = "U.S. West Coast",
    Hawaii_Johnston = "Hawai'i & Johnston",
    American_Samoa = "American Samoa",
    Line_Phoenix = "U.S. Line & Phoenix Islands")

##### check #####
# filt %>% pull(AphiaID) %>% is.na() %>% table()
#
# filt %>%
#   filter(is.na(AphiaID) == T) %>%
#   pull(ScientificName) %>%
#   table()

##### --OR --taxa of interest by name #####
namesToMatch <- filt %>%
  filter(grepl('Madracis asanoi', ScientificName)) %>%
  # filter(is.na(AphiaID) == T) %>%
  pull(ScientificName) %>%
  unique()

##### --OR-- pull taxa from Tom's list  #####
## parse names
install.packages("taxize")
library(taxize)

## make a few changes
tax_tom$Species_edit <- gsub('\\(C.\\)','', tax_tom$Species)
tax_tom$Species_edit <- gsub('\\(B.\\)','', tax_tom$Species_edit)
tax_tom$Species_edit <- gsub('\\(F.\\)','', tax_tom$Species_edit)
tax_tom$Species_edit <- gsub('\\(S.\\)','', tax_tom$Species_edit)
tax_tom$Species_edit <- gsub('\\(O.\\)','', tax_tom$Species_edit)
tax_tom$Species_edit <- gsub('\\(T.\\)','', tax_tom$Species_edit)
tax_tom$Species_edit <- gsub('\\(U.\\)','', tax_tom$Species_edit)

## parse the 'Species' names in Tom's list for use in the WoRMS API
parsed_list <- gbif_parse(tax_tom$Species_edit)
View(parsed_list)

## add parsed list to Tom's taxonomy
dim(parsed_list)
tax_tom <- cbind(parsed_list$canonicalname, tax_tom)
tax_tom <- tax_tom %>% rename(scientificname = "parsed_list$canonicalname")

##### create names list from parsed_list #####
namesToMatch <- parsed_list$canonicalname

##### checking #####
# class(namesToMatch)
# length(namesToMatch)

##### --OR--match your names list using the worrms API #####

## create vector of names from parsed list
my_vector <- parsed_list$canonicalname

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by names list #####
## run this to get the data structure for an empty dataframe
species_list <- wm_records_name("Antipathes griggi")

## initiate the data frame
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

##### left join the species_list (worms record) results with Tom's original taxonomy file #####
tax_tom_enhanced <- left_join(tax_tom,
                              species_list,
                              multiple = "all")

##### --OR--match your species using AphiaID #####
##### create vector from AphiaIDs #####
my_vector <- tax_tom_enhanced$AphiaID

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
dim(tax_tom_enhanced)
dim(classification)
dim(vernaculars)
dim(synonyms)

head(tax_tom_enhanced$AphiaID)
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





