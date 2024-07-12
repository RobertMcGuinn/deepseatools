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
library(worrms)

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

rm(indata)

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

##### ***** #####
##### creating a patch for cases where VerbatimScientificName is blank for cf taxa #####
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

patch <- cf %>%
  ungroup() %>%
  select(CatalogNumber, ScientificName.x) %>%
  rename(VerbatimScientificName = ScientificName.x)

cf2 <- filt_new %>%
  filter(grepl('cf.', ScientificName)) %>%
  group_by(CatalogNumber,
           ScientificName) %>%
    summarize(n=n()) %>% rename(VerbatimScientificName = ScientificName) %>%
    select(CatalogNumber, VerbatimScientificName)

patch2 <- rbind(patch, cf2)
## View(patch2)
write.csv(patch2,
          '../indata/cf_VerbatimScientificName_patch.csv',
          row.names = F)


##### ***** #####
## working on taxonomic patch for incorrect cf handling for taxonrank == species
##### create a list of AphiaIDs that we need to get parent taxa for #####
aphiaID_list <- change_summary %>%
  ungroup() %>%
  filter(grepl('cf.', ScientificName.x),
         TaxonRank.y == 'species') %>%
  group_by(ScientificName.x,
           ScientificName.y,
           AphiaID.x,
           AphiaID.y) %>%
  summarize(n=n()) %>%
  pull(AphiaID.y)

##### check #####
View(aphiaID_list)

##### function: to get parent taxa from incoming AphiaID #####
get_parent_taxon <- function(aphia_id) {
  # Get detailed information about the taxon
  taxon_info <- wm_record(aphia_id)
  # Get the rank of the taxon
  taxon_rank <- taxon_info$rank
  # Get the classification hierarchy of the taxon
  classification <- wm_classification(aphia_id)
  # Find the index of the current taxon in the classification
  taxon_index <- which(classification$rank == taxon_rank)
  # Get the parent taxon
  parent_taxon <- classification[taxon_index - 1, ]

}

##### apply the parent taxa function over the list #####
parent_taxa <- sapply(aphiaID_list, get_parent_taxon)
parent <- parent_taxa[3, ]
parent <- unlist(parent)
aphiaIDs <- parent_taxa[1,]
aphiaIDs <-  unlist(aphiaIDs)

##### check #####
parent

##### join the new parent taxa list and parent aphiaIDs to original CatalogNumbers #####
x <- change_summary %>%
  ungroup() %>%
  filter(grepl('cf.', ScientificName.x),
         TaxonRank.y == 'species') %>%
  group_by(ScientificName.x,
           ScientificName.y,
           AphiaID.x,
           AphiaID.y) %>%
  summarize(n=n())

x$parent <- parent
## View(x)
x$AphiaID <- aphiaIDs
## View(x)
y <- change_summary %>%
  ungroup() %>%
  filter(grepl('cf.', ScientificName.x),
         TaxonRank.y == 'species') %>%
  group_by(CatalogNumber,
           ScientificName.x,
           ScientificName.y,
           AphiaID.x,
           AphiaID.y) %>%
  summarize(n=n())

## View(y)
yo <- left_join(x,y, by = 'ScientificName.y')
View(yo)

patch <- yo %>%
  ungroup() %>%
  dplyr::select(CatalogNumber, AphiaID)

# View(patch)

patch %>% write.csv("../indata/patch_for_correct_cf_handling.csv", row.names = F)



##### *****
##### working on specific changes to AphiaID #####
change_summary %>% filter(ScientificName.x == "Paramuriceidae") %>%
  group_by(CatalogNumber,
           VerbatimScientificName.x,
           VerbatimScientificName.y,
           VernacularNameCategory.x,
           VernacularNameCategory.y,
           ScientificName.x,
           ScientificName.y,
           AphiaID.x,
           AphiaID.y) %>%
  summarize(n=n()) %>% View()

##### list of specfic changes needed #####
# 968117, 994317 # Bathypathes to Alternatipathes
# 971742, 883738 # Isididae to Bathygorgia
# many, 286810   # Deltocyathus varians (286809) to Deltocyathus vaughani (286810)
# 164149, 135074 # Clavulariidae (125270) to Dendrophylliidae (135074)
# many, 289917   # Errinopora pourtalesii (289915) to Errinopora zarhyncha (289917)
# many, 287016   # Flabellum (Ulocyathus) macandrewi to Flabellum (Ulocyathus) marcus
# 642614, 125295 # Pleurogorgia to  Iridogorgia
# 952693, 125307 # Isididae to Lepidisis
# 971742, 883738 # Isididae to Bathygorgia
# many, 1647267  # Muricella brunnea to Muricella reticulata
# many, 418845   # Nicella americana to Neospongodes agassizii
# 952690, 125311 # Paramuriceidae to Paramuricea
# 250129, 125278 # Stenisis humilis to Primnoidae
# many, 291129  # Stereotelesto to Stereotelesto corallina
# many, 267906  # Thelogorgia stellata to Thelogorgia
# many, 135105  # Trochocyathus (Trochocyathus) to Trochocyathus
# 949115, 131644 # Porifera to Cladorhizidae
# 949143, 131692 # Porifera to Euplectellidae
# many, 1424208 # Farrea to Farrea cordelli
# 488793, 131689 # Geodiidae to Farreidae
# 949638, 131689 # Porifera to Farreidae
# many, 132038 # Latrunculia (Latrunculia) to Latrunculia
# many, 1651922 # Acanthascus (Staurocalyptus) to Acanthascus (Staurocalyptus) pamelaturnerae

change_summary2 <- change_summary %>%
  mutate(AphiaID = case_when(
  ScientificName.x %in% c('Alternatipathes') &
    ScientificName.y %in% c('Bathypathes') ~ '994317',
  ScientificName.x %in% c('Bathygorgia') &
    ScientificName.y %in% c('Isididae') ~ '883738',
  ScientificName.x %in% c('Deltocyathus vaughani') &
    ScientificName.y %in% c('Deltocyathus varians') ~ '286810',
  ScientificName.x %in% c('Dendrophylliidae') &
    ScientificName.y %in% c('Clavulariidae') ~ '135074',
  ScientificName.x %in% c('Errinopora zarhyncha') &
    ScientificName.y %in% c('Errinopora pourtalesii') ~ '289917',
  ScientificName.x %in% c('Flabellum (Ulocyathus) marcus') &
    ScientificName.y %in% c('Flabellum (Ulocyathus) macandrewi') ~ '287016',
  ScientificName.x %in% c('Iridogorgia') &
    ScientificName.y %in% c('Pleurogorgia') ~ '125295',
  ScientificName.x %in% c('Lepidisis') &
    ScientificName.y %in% c('Isididae') ~ '125307',
  ScientificName.x %in% c('Bathygorgia') &
    ScientificName.y %in% c('Isididae') ~ '883738',
  ScientificName.x %in% c('Muricella reticulata') &
    ScientificName.y %in% c('Muricella brunnea') ~ '1647267',
  ScientificName.x %in% c('Neospongodes agassizii') &
    ScientificName.y %in% c('Nicella americana') ~ '418845',
  ScientificName.x %in% c('Paramuricea') &
    ScientificName.y %in% c('Paramuriceidae') ~ '125311',
  ScientificName.x %in% c('Primnoidae') &
    ScientificName.y %in% c('Stenisis humilis') ~ '125278',
  ScientificName.x %in% c('Stereotelesto corallina') &
    ScientificName.y %in% c('Stereotelesto') ~ '291129',
  ScientificName.x %in% c('Thelogorgia') &
    ScientificName.y %in% c('Thelogorgia stellata') ~ '267906',
  ScientificName.x %in% c('Trochocyathus') &
    ScientificName.y %in% c('Trochocyathus (Trochocyathus)') ~ '135105',
  ScientificName.x %in% c('Cladorhizidae') &
    ScientificName.y %in% c('Porifera') ~ '131644',
  ScientificName.x %in% c('Euplectellidae') &
    ScientificName.y %in% c('Porifera') ~ '131692',
  ScientificName.x %in% c('Farrea cordelli') &
    ScientificName.y %in% c('Farrea') ~ '1424208',
  ScientificName.x %in% c('Farreidae') &
    ScientificName.y %in% c('Geodiidae') ~ '131689',
  ScientificName.x %in% c('Farreidae') &
    ScientificName.y %in% c('Porifera') ~ '131689',
  ScientificName.x %in% c('Latrunculia') &
    ScientificName.y %in% c('Latrunculia (Latrunculia)') ~ '131689',
  ScientificName.x %in% c('Staurocalyptus pamelaturnerae') &
    ScientificName.y %in% c('Acanthascus (Staurocalyptus)') ~ '1651922'))






##### check #####
change_summary2 %>% pull(AphiaID) %>% table(useNA = 'always')

change_summary %>% filter(ScientificName.x == "Staurocalyptus pamelaturnerae") %>%
  group_by(ScientificName.x, ScientificName.y) %>%
  summarize(n=n()) %>% View()

change_summary %>% filter(CatalogNumber == "164149") %>%
  group_by(ScientificName.x, ScientificName.y) %>%
  summarize(n=n()) %>% View()


##### ***** #####
##### looking at missing VernacularNameCategory #####
vnc <- change_summary %>%
  filter(is.na(VernacularNameCategory.y) == T) %>%
  group_by(CatalogNumber,
           VerbatimScientificName.x,
           VerbatimScientificName.y,
           VernacularNameCategory.x,
           VernacularNameCategory.y,
           ScientificName.x,
           ScientificName.y,
           AphiaID.x,
           AphiaID.y) %>%
  summarize(n=n())

vnc <- change_summary %>%
  filter(CatalogNumber == "968117") %>%
  group_by(CatalogNumber,
           VerbatimScientificName.x,
           VerbatimScientificName.y,
           VernacularNameCategory.x,
           VernacularNameCategory.y,
           ScientificName.x,
           ScientificName.y,
           AphiaID.x,
           AphiaID.y) %>%
  summarize(n=n())

View(vnc)
View(change_summary)
View(vnc)

filt_new %>%
  filter(is.na(VernacularNameCategory) == T |
           VernacularNameCategory == '') %>%
  group_by(ScientificName, AphiaID) %>%
  summarize(n=n())

filt_new %>%
  filter(is.na(VernacularNameCategory) == T |
           VernacularNameCategory == '') %>%
  group_by(ScientificName) %>%
  summarize(n=n()) %>% pull(ScientificName) %>% unique()

filt_new %>% filter(ScientificName == "Isidella trichotoma") %>%
  pull(VernacularNameCategory) %>% table(useNA = 'always')

filt_new %>% filter(ScientificName == "Isidella") %>%
  pull(VernacularNameCategory) %>% unique()

# "Adinisis"
# "Atlantia denticulata"
# "Calyptrophora lyra"
# "Caulophacus (Caulodiscus) iocasicus"
# "Cladorhiza gelida"
# "Heterocyathus monileseptatum"
# "Isidella trichotoma"
# "Lycopodina"
# "Malacalcyonacea"
# "Narella japonensis"
# "Pennatula aculeata var. alba"
# "Pennatula aculeata var. rosea"
# "Pennatuloidea"
# "Schulzeviella"





