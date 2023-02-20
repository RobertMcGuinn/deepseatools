##### header #####
## Author: Robert McGuinn
## Started on: 20230217
## Purpose: WoRMS API calls.

##### packages #####
library(tidyverse)
library(worrms)

##### load NDB from local file (manual)#####
## [manual]
# setwd("C:/rworking/deepseatools/indata")
# filename <- "DSCRTP_NatDB_20221213-0.csv"
# indata<-read_csv(filename,
#                  col_types = cols(.default = "c"),
#                  locale = locale(encoding = 'latin9'),
#                  na = c("-999", "NA"))
#
# filt <- indata %>%
#   filter(Flag == 0)
#
#
# rm(indata)

##### filter the database to taxa of interest #####
namesToMatch <- filt %>%
  filter(grepl('Madracis', ScientificName)) %>%
  pull(ScientificName) %>%
  unique()

##### OPTIONAL get your names list together manually #####
# namesToMatch <- c('Madracis', 'Lophelia')

##### checking #####
class(namesToMatch)
length(namesToMatch)

##### match your names using the worrms API #####
species_list <- wm_records_names(name = namesToMatch)
##### checking #####
# View(species_list)

##### targeted AphiaID (note: should start loop here) #####
species <- species_list[[1]]
id <- species %>% pull(AphiaID)

##### get some additional information about the AphiaID #####
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
classification_wide

##
vernaculars_list <- paste(vernaculars$vernacular, collapse=" | ")
AphiaID = id
vernaculars_wide <- data.frame(AphiaID, vernaculars_list)

##
synonym_list <- paste(synonyms$scientificname, collapse=" | ")
AphiaID = id
synonyms_wide <- data.frame(AphiaID, synonym_list)

##### merge info together #####
worms_taxonomy <- left_join(classification_wide, vernaculars_wide, by = 'AphiaID')
worms_taxonomy <- left_join(worms_taxonomy, synonyms_wide, by = "AphiaID")

##### add some other variables #####
rank <- species %>% pull(rank)


##### checking #####
View(worms_taxonomy)

##### build master taxonomy dataframe #####





















