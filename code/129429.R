##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240404
## purpose: taxonomic trouble shooting

##### linkage #####
filename <- '129429' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

##### packages #####
library(tidyverse)

##### load old ndb #####
digits = 121
path <- "C:/rworking/deepseatools/indata"
csv <- "DSCRTP_NatDB_20230928-0.csv"
setwd(path)
indata <- read.csv(csv, header = T, encoding = 'latin1')
filt_old <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### load new ndb #####
digits = 121
path <- "C:/rworking/deepseatools/indata"
csv <- "DSCRTP_NatDB_20240325-0.csv" # Aretha Franklin
setwd(path)
indata <- read.csv(csv, header = T, encoding = 'latin1')
filt_new <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### compare differences #####
## Merge data frames based on CatalogNumber
merged_df <- merge(filt_old, filt_new, by = "CatalogNumber", all = TRUE)

## Function to compare two columns and report differences
compare_columns <- function(x, y) {
  differences <- ifelse(is.na(x) | is.na(y), NA, ifelse(x != y, paste(x, y, sep = " | "), ""))
  return(differences)
}

## Apply the function to the merged dataframe
merged_df$differences <- compare_columns(merged_df$ScientificName.x, merged_df$ScientificName.y)

## select just the ones that
changes <- merged_df %>% filter(differences != '')

##### select just the taxonomic variables #####
change_summary <- changes %>%
  select(CatalogNumber,
         VerbatimScientificName.x,
         VerbatimScientificName.y,
         ScientificName.x,
         ScientificName.y,
         VernacularName.x,
         VernacularName.y,
         VernacularNameCategory.x,
         VernacularNameCategory.y,
         TaxonRank.x,
         TaxonRank.y,
         AphiaID.x,
         AphiaID.y,
         Phylum.x,
         Phylum.y,
         Class.x,
         Class.y,
         Subclass.x,
         Subclass.y,
         Order.x,
         Order.y,
         Suborder.x,
         Suborder.y,
         Family.x,
         Family.y,
         Subfamily.x,
         Subfamily.y,
         Genus.x,
         Genus.y,
         Subgenus.x,
         Subgenus.y,
         Species.x,
         Species.y,
         Subspecies.x,
         Subspecies.y,
         ScientificNameAuthorship.x,
         ScientificNameAuthorship.y,
         Synonyms.x,
         Synonyms.y)

##### export result to csv (export to CSV) #####
filename <- "20240405-1_taxonomic_change_summary_NBD_version_20240325-0_RPMcGuinn.csv"
write.csv(change_summary,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

##### creatig a patch for cases where VerbatimScientificName is blank for cf taxa #####
cf <- change_summary %>%
  filter(grepl('cf.', ScientificName.x) |
           grepl('cf.', ScientificName.y)) %>%
  group_by(CatalogNumber,
           VerbatimScientificName.x,
           VerbatimScientificName.y,
           ScientificName.x,
           ScientificName.y,
           AphiaID.x,
           AphiaID.y) %>%
  summarize(n=n())

ungroup(cf)

patch <- cf %>%
  ungroup() %>%
  select(CatalogNumber, ScientificName.x) %>%
  rename(VerbatimScientificName = ScientificName.x)

View(patch)
write.csv(patch, '../indata/cf_VerbatimScientificName_patch.csv')














