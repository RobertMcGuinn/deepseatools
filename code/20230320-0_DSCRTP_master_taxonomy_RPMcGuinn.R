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

##### load the Tom Hourigan list (US_only_comprehensive) #####
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

##### --OR--match your species using AphiaID #####
##### create vector from AphiaIDs #####
my_vector <- tax$AphiaID

##### check ####
length(my_vector)

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
## preparation step to get rid of duplicate AphiaID
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
























