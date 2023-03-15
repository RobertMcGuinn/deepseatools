##### header #####
## Author: Robert McGuinn
## Started on: 20230217
## Purpose: WoRMS API calls.

##### packages #####
library(tidyverse)
library(worrms)

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

parsed_list <- gbif_parse(tax_tom$Species_edit)
View(parsed_list)

namesToMatch <- parsed_list$canonicalname

##### --OR-- get your names list together manually #####
namesToMatch <- c('Madracis asanoi')

##### checking #####
# class(namesToMatch)
# length(namesToMatch)

##### --OR--match your names list using the worrms API #####
species_list <- wm_records_names(name = namesToMatch[1:50])

## create vector of names from parsed list
my_vector <- parsed_list$canonicalname[1:100]

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records #####
species_list <- wm_records_name("Caryophyllia corrugata")
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_records_names(name = my_groups[[i]])
  species_list <- do.call("rbind", species_list)
  df <- rbind(df, species_list)
}
species_list <- df

##_______________________________________________
##### --OR--match your species using AphiaID #####
## get rid of any -999 AphiaIDs
tax_sub <- tax %>% filter(AphiaID != "-999")

## create vector from AphiaIDs
my_vector <- tax_sub$AphiaID[0:55]

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records #####
species_list <- wm_records_name("Caryophyllia corrugata")
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

##### loop to get full classification #####
for (i in my_vector){
classification <- wm_classification(i)
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

##### checking #####
View(species_list)

##### loop #####
for (i in seq_along(my_vector)){
  species <- species_list[[i]]
  id <- species %>% pull(AphiaID)
  classification <- wm_classification(id)
  vernaculars <- wm_common_id(id)
  synonyms <- wm_synonyms(id)
  classification_wide <- classification %>%
    select(rank,scientificname) %>%
    pivot_wider(
      names_from = rank,
      values_from = scientificname
    )
  classification_wide$AphiaID <- id
  vernaculars_list <- paste(vernaculars$vernacular, collapse=" | ")
  AphiaID = id
  vernaculars_wide <- data.frame(AphiaID, vernaculars_list)
  worms_taxonomy <- left_join(classification_wide, vernaculars_wide, by = 'AphiaID')
  worms_taxonomy <- left_join(worms_taxonomy, synonyms_wide, by = "AphiaID")
  df <- rbind(df, worms_taxonomy)
}




## ______________________________________________

##### targeted AphiaID (note: should start loop here) #####
species <- species_list[[1]]
id <- species %>% pull(AphiaID)

##### get vernaculars and synonyms #####
classification <- wm_classification(id)
vernaculars <- wm_common_id(id)
synonyms <- wm_synonyms(id)

##### do some transformation of the information returned #####
classification_wide <- classification %>%
  select(rank,scientificname) %>%
  pivot_wider(
    names_from = rank,
    values_from = scientificname
  )

classification_wide$AphiaID <- id

##### check #####
# View(classification_wide)

##### get vernaculars #####
vernaculars_list <- paste(vernaculars$vernacular, collapse=" | ")
AphiaID <- id
vernaculars_wide <- data.frame(AphiaID, vernaculars_list)

##### get synonyms
synonym_list <- paste(synonyms$scientificname, collapse=" | ")
AphiaID <- id
synonyms_wide <- data.frame(AphiaID, synonym_list)

##### merge info together #####
worms_taxonomy <- left_join(classification_wide, vernaculars_wide, by = 'AphiaID')
worms_taxonomy <- left_join(worms_taxonomy, synonyms_wide, by = "AphiaID")

##### add some other variables #####
rank <- species %>% pull(rank)

##### checking #####
View(worms_taxonomy)

##### build master taxonomy dataframe #####





















