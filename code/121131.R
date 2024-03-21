##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20231128
## purpose:

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(worrms)
library(openxlsx)
library(taxize)

##### linkage #####
## manual input here
filename <- '121131' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, ".R", sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### load data #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv('20231129-0_HURL_archive_records_SBingo_1972_2013_121131.csv')
## exported from original Excel file to (CSV UTF-8)

##### explore #####
dim(sub)
summary(sub)
names(sub)
table(sub$SurveyID, useNA = 'always')
table(sub$Vessel, useNA = 'always')
table(sub$EventID, useNA = 'always')
table(sub$ObservationDate, useNA = 'always')
table(sub$ScientificName, useNA = 'always')
table(is.na(sub$Latitude))
table(is.na(sub$Longitude))
table(is.na(sub$SampleID))
head(sub$CatalogNumber)
head(sub$SampleID)
head(sub$TrackingID)
table(is.na(sub$TrackingID))
table(is.na(sub$Condition))
filt %>%
  filter(grepl('Bingo', Reporter)) %>%
  pull(DatasetID) %>%
  table()

filt %>%
  filter(grepl('OET_NA134', DatasetID)) %>%
  pull(TrackingID)

dim(sub)
length(setdiff(sub$SampleID, filt$SampleID))
length(setdiff(sub$TrackingID, filt$TrackingID))


length(unique(sub$SampleID))
length(unique(sub$TrackingID))
length(setdiff(unique(sub$SampleID), unique(filt$SampleID)))
filt %>% filter(SampleID %in% sub$SampleID) %>% pull(SampleID) %>% length()
filt %>%
  filter(SurveyID %in% sub$SurveyID) %>%
  pull(SurveyID) %>% unique()


##### make any corrections #####
sub <- sub %>%
  mutate(AphiaID = ifelse(ScientificName == 'Actinopterygii', 10194, AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == 'Cnidaria', 1267, AphiaID))

##### check #####
dim(sub)

length(unique(sub$AphiaID))

table(sub$AphiaID, useNA = 'always')

sub %>%
  filter(is.na(AphiaID) == T) %>%
  pull(AphiaID) %>% length()

table(is.na(sub$AphiaID) == T,
      useNA = 'always')

sub %>%
  filter(AphiaID == "-999") %>%
  group_by(AphiaID, ScientificName) %>%
  summarize(n=n())

sub %>%
  filter(AphiaID == "") %>%
  pull(ScientificName)

##### check #####
dim(sub)

##### load the taxonomy table from CSV #####
tax <- read.csv("C:/rworking/deepseatools/indata/tax.csv")

##### create vector from incoming AphiaIDs #####
my_vector <- unique(sub$AphiaID)

##### check #####
# length(my_vector)

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list_original <- df

##### check #####
# dim(species_list_original)
#
# View(species_list_original)
#
# table(species_list_original$AphiaID, useNA = 'always')
#
# table(sub$AphiaID, useNA = 'always')
#
# sub %>% filter(AphiaID == -999) %>% pull(ScientificName)
#
# species_list_original %>% filter(is.na(AphiaID) == T) %>%
#   pull(scientificname)
#
# table(sub$AphiaID, useNA = 'always')
# setdiff(species_list_original$AphiaID, sub$AphiaID)
# setdiff(sub$AphiaID, species_list_original$AphiaID)
# View(species_list_original)
# table(species_list_original$status, useNA = 'always')
# species_list_original %>% filter(status != 'accepted') %>% View()

##### create a complete valid AphiaID list #####
species_list_original <- species_list_original %>%
  mutate(valid_AphiaID_complete = ifelse(is.na(valid_AphiaID) == T,
                                         AphiaID,
                                         valid_AphiaID))

##### check #####
# species_list_original %>% filter(status != 'accepted') %>%
#   group_by(AphiaID, valid_AphiaID, valid_AphiaID_complete) %>%
#   summarize(n=n()) %>% View()

##### create vector from valid AphiaIDs #####
my_vector <- unique(species_list_original$valid_AphiaID_complete)

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by the valid AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list <- df

##### check #####
# View(species_list)
# table(is.na(species_list$AphiaID))
# table(species_list$status)
# dim(species_list)

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

##### left join the species list from above with all of the other API tables #####
by <- join_by(valid_AphiaID == AphiaID)
joined2 <- left_join(species_list, classification, by)

by <- join_by(valid_AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

by <- join_by(valid_AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

##### check #####
# names(joined4)
# setdiff(joined4$AphiaID, species_list_original$valid_AphiaID_complete)
# setdiff(species_list_original$valid_AphiaID_complete, joined4$AphiaID)

###### join original table with the new table #####
joined4$AphiaID2 <- joined4$AphiaID
by <- join_by(valid_AphiaID_complete == AphiaID2)
taxonomy_table <- left_join(species_list_original, joined4, by)
# View(taxonomy_table)
# names(taxonomy_table)

##### add taxonomy to sub #####
by <- join_by(AphiaID == AphiaID.x)
sub_enhanced <- left_join(sub, taxonomy_table, by)

##### check #####
# sub_enhanced %>% filter(is.na(phylum.y) == T) %>%
#   pull(ScientificName) %>%
#   unique()
#
# dim(sub)
# dim(sub_enhanced)

##### gather information into proper variables #####
sub_enhanced$VerbatimScientificName <- sub$VerbatimScientificName
sub_enhanced$ScientificName <- sub_enhanced$scientificname.y
sub_enhanced$VernacularName <- sub_enhanced$vernaculars_list
sub_enhanced$TaxonRank <- sub_enhanced$rank.y
sub_enhanced$AphiaID <- sub_enhanced$valid_AphiaID_complete
sub_enhanced$Phylum <- sub_enhanced$phylum.y
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
sub_enhanced$ScientificNameAuthorship <- sub_enhanced$authority.y
sub_enhanced$Synonyms <- sub_enhanced$synonyms_list

##### check #####
# table(sub_enhanced$Phylum, useNA = 'always')
# table(x$Phylum, useNA = 'always')
# table(sub_enhanced$Class, useNA = 'always')
# sub_enhanced_filter %>% filter(Class == 'Hydrozoa') %>%
#   group_by(Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()

##### apply taxonomic filter #####
sub_enhanced_filter <- sub_enhanced %>%
  filter(Subphylum == 'Vertebrata' |
           Phylum == 'Cnidaria' |
           Phylum == 'Porifera')

`%notin%` <- Negate(`%in%`)
sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Class %notin% c('Scyphozoa', 'Thalicacea', 'Ascidiacea'))

sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Family %notin% c('Acroporidae', 'Poritidae'))

sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Genus %notin% c('Pocillopora'))

sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Order == 'Scleractinia' |
           Order == 'Antipatharia' |
           Genus == 'Savalia' |
           Genus == 'Kulamanamana' |
           Genus == 'Gerardia' |
           Family == 'Stylasteridae' |
           Order  == 'Alcyonacea' |
           Order ==  'Gorgonacea' |
           Order ==  'Helioporacea' |
           Order == 'Pennatulacea' |
           Order == 'Scleralcyonacea' |
           Family == 'Stylasteridae' |
           Genus == 'Solanderia' |
           Genus == 'Janaria' |
           Genus == 'Hydrocorella' |
           Genus == 'Hydrodendron' |
           Phylum == 'Chordata' |
           Phylum == 'Porifera' |
           Order == 'Malacalcyonacea'
  )


##### check #####
# dim(sub_enhanced) - dim(sub_enhanced_filter)
# setdiff(sub_enhanced$ScientificName, sub_enhanced_filter$ScientificName)
#
# table(sub_enhanced_filter$Phylum, useNA = 'always')
# table(sub_enhanced_filter$Subphylum, useNA = 'always')
#
# sub_enhanced_filter %>%
#   group_by(VernacularName, AphiaID, Phylum, Subphylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()
#
# filt %>%
#   filter(Family == 'Corallimorphidae') %>%
#   group_by(VernacularName, AphiaID, ScientificName) %>%
#   summarize(n=n()) %>% View()

##### assign VernacularNameCategory #####
## define not in
`%notin%` <- Negate(`%in%`)

gorgfamilies <- c("Paramuriceidae","Chrysogorgiidae","Dendrobrachiidae",
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

sub_enhanced2 <- sub_enhanced_filter %>%
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
    Order %in% c('Malacalcyonacea') ~ 'soft coral',
    Order %in% c('Anthoathecata') &
      Family %notin%  c('Solanderiidae') ~ 'lace coral',
    Family %in% c('Lithotelestidae') ~ 'lithotelestid coral',
    Family %in% othercorallikehydrozoanfamilies ~ 'other coral-like hydrozoan',
    Superfamily %in% c('Pennatuloidea') ~ 'sea pen',
    ScientificName %in% c('Porifera') ~ 'sponge',
    Suborder %in% c('Stolonifera') ~ 'stoloniferan coral',
    Family %in% c('Clavulariidae') ~ 'stoloniferan coral',
    Genus %in% c('Clavularia') ~ 'stoloniferan coral',
    Order %in% c('Scleractinia') &
      TaxonRank %in% c('Order')  ~ 'stony coral (unspecified)',
    ScientificName %in% stonycoralbranching ~ 'stony coral (branching)',
    ScientificName %in% stonycoralcupcoral ~ 'stony coral (cup coral)',
    Genus %in% c('Acanthogorgia') ~ 'gorgonian coral',
    Genus %in% c('Hydrodendron') ~ 'other coral-like hydrozoan',
    Genus %in% c('Caryophyllia') ~ 'stony coral (cup coral)',
    ScientificName %in% c('Caryophylliidae') ~ 'stony coral (unspecified)',
    Genus %in% c('Telestula') ~ 'stoloniferan coral',
    ScientificName %in% c('Dendrophylliidae') ~ 'stony coral (unspecified)',
    Genus %in% c('Leptoseris') ~ 'stony coral (cup coral)',
    Genus %in% c('Madracis') ~ 'stony coral (branching)',
    ScientificName %in% c('Pocilloporidae') ~ 'stony coral (unspecified)',
    TRUE ~ ''))

##### check #####
# table(sub_enhanced2$VernacularNameCategory, useNA = 'always')
#
# sub_enhanced2 %>%
#   filter(VernacularNameCategory == '') %>%
#   group_by(AphiaID, Class, Family, Order, Genus, ScientificName) %>%
#   summarize(n=n()) %>% View()
#
# x <- sub_enhanced2 %>%
#   filter(VernacularNameCategory == '') %>%
#   pull(Family) %>% unique()
#
# filt %>% filter(Genus %in% c('Pocillopora')) %>%
#   group_by(Class, Order, Family, Genus, ScientificName, VernacularNameCategory) %>%
#   summarize(n=n()) %>% View()
#
# filt %>% filter(Family %in% c(filt %>% filter(Family %in% x) %>%
#   group_by(Class, Order, Family, Genus, ScientificName, VernacularNameCategory) %>%
#   summarize(n=n()) %>% View()
# )) %>%
#   group_by(Class, Order, Family, Genus, ScientificName, VernacularNameCategory) %>%
#   summarize(n=n()) %>% View()
#
# sub %>%
#   filter(Family %in% x) %>%
#   group_by(DatasetID, VernacularNameCategory, Family, ScientificName, DepthInMeters, EntryDate) %>%
#   summarize(n=n()) %>% View()
#
#
#
# filt %>% filter(Order == 'Malacalcyonacea') %>% pull(VernacularNameCategory) %>%
#   table(useNA = 'always')
#
# filt %>% filter(Genus == 'Clavularia') %>% pull(VernacularNameCategory) %>%
#   table(useNA = 'always')
#
# sub_enhanced2 %>%
#   filter(VernacularNameCategory == 'stony coral (cup)') %>%
#   pull(VernacularNameCategory) %>% unique()

##### get rid of unneeded column names #####
names_list <- names(sub)
sub_enhanced2 <- sub_enhanced2 %>%
  dplyr::select(all_of(names_list))

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
dim(sub_enhanced3)
dim(sub)
length(sub$CatalogNumber) - length(sub_enhanced3$CatalogNumber)

x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
sub_enhanced %>% filter(CatalogNumber %in% x) %>%
  group_by(AphiaID, Phylum, Class, Order, Suborder, Family, Genus, Species) %>%
  summarize(n=n()) %>% View()

table(is.na(sub$CatalogNumber))
table(is.na(sub_enhanced3$CatalogNumber))
sub %>% filter(ScientificName == 'Dichotella gemmacea') %>% pull(AphiaID)
'Dichotella gemmacea'

x <- setdiff(sub_enhanced3$VerbatimScientificName, sub_enhanced3$ScientificName)
sub_enhanced3 %>% filter(VerbatimScientificName %in% x) %>%
  group_by(VerbatimScientificName, ScientificName, VernacularNameCategory) %>%
  summarize(n=n()) %>% View()

x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
sub %>% filter(CatalogNumber %in% x) %>% pull(AphiaID) %>% unique()

table(sub_enhanced3$VernacularNameCategory, useNA = 'always')

sub_enhanced3 %>% filter(VernacularNameCategory == '') %>% pull(Order) %>% unique()

sub_enhanced3 %>% filter(VernacularNameCategory == '') %>%
  group_by(AphiaID, Phylum, Class, Order, Family, Genus, Species) %>%
  summarize(n=n()) %>% View()

##### export result to csv (export to CSV) #####
filename <- "20231129-0_HURL_archive_records_SBingo_1972_2013_121131_taxonomy_patch.csv"
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))



##### ***** NEW VERSION *****  #####
##### load NDB #####
source('c:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### load data #####
setwd('c:/rworking/deepseatools/indata')
filename <- '20231219-0_HURL_archive_records_SBingo_1972_2013_121131'
sub <- read.csv(paste(filename, '.csv', sep = ''))

##### check #####
table(sub$Flag)
sub %>% filter(Flag == 1) %>% group_by(Phylum, Class, Order, Family, ScientificName) %>%
  summarize(n=n())
table(sub$IndividualCount, useNA = 'always')
filt %>% filter(grepl('Eiwa', VehicleName)) %>% pull(VehicleName) %>% table()
filt %>% filter(grepl('Shimada', Vessel)) %>% pull(Vessel) %>% table()
filt %>% filter(grepl('HURL', DatasetID)) %>% pull(DatasetID) %>% table()
filt %>% filter(grepl('Hawaii', DataProvider)) %>% pull(DataProvider) %>% table()
sub %>%  filter(Longitude > -2) %>% pull(Longitude)
sub %>% filter(is.na(SurveyID) == T) %>%
  pull(ScientificName) %>%
  unique()

##### map check #####
x <- sub %>% filter(
  # FlagReason == 'Horizontal position or depth is questionable' |
  #   FlagReason == 'Insufficient taxonomic information | Horizontal position or depth is questionable'
  # FlagReason == 'Insufficient taxonomic information | Possible intersection with land'|
  # FlagReason == 'Possible intersection with land'
  FlagReason ==  'Invalid latitude | Invalid longitude'
) %>%
  group_by(CatalogNumber, Latitude, Longitude) %>%
  summarize(n=n())
points <- st_as_sf(x, coords = c("Longitude", "Latitude"), crs = 4326)
st_write(points, "C:/rworking/deepseatools/indata/sub_geo.shp", delete_dsn = T)

##### run QA report #####
## manual change version of dashboard version number is required
rmarkdown::render("C:/rworking/deepseatools/code/20240320-0_rmd_accession_qa_dashboard.Rmd",
                  output_file =  paste(filename,".docx", sep=''),
                  output_dir = 'C:/rworking/deepseatools/reports')

## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/13Jg191pDiJPU9GF-Vp238E2sjzJV9eoq"
setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)


